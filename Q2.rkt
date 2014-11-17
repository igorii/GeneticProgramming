#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require "generic-window.rkt")
(require "regression-drawing.rkt")

(define (chance p) (< (random) p))

;; Restricted functions
(define (r-add a b) (+ a b))
(define (r-sub a b) (- a b))
(define (r-mul a b) (* a b))
(define (r-div a b) (if (= b 0) 0 (/ a b)))
(define (r-sin a)   (sin a))
(define (r-cos a)   (cos a))
(define (r-exp a b)
  (if (and (= a 0) (< b 0)) 0
    (let ([t (expt a b)])
      (if (not (complex? t)) t 1))))

(define *pop-size* 500)
(define *success* 20)
(define *precision* 0.001)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.05)
(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *elitism* #t)
(define *terminals* (list 'x 'R))
(define *nfitcases* 100)

(struct leaf    (x)       #:transparent #:mutable)
(struct branch1 (n a1)    #:transparent #:mutable)
(struct branch2 (n a1 a2) #:transparent #:mutable)
(struct fn (sym arity proc))
(define *func-table* (list (fn 'add 2 r-add)
                           (fn 'sub 2 r-sub)
                           (fn 'mul 2 r-mul)
                           (fn 'div 2 r-div)
                           (fn 'sin 1 r-sin)
                           (fn 'cos 1 r-cos)
                           (fn 'exp 2 r-exp)))

(define (random-fn ftable)  (car (shuffle ftable)))
(define (random-term terms) (car (shuffle terms)))
(define (random-item ftable terms) (car (shuffle (append ftable terms))))

(struct pathnode (path node) #:transparent)
(define (tree->list tree)
  (define (f path tree)
    (match tree [(leaf n)     (list (pathnode path tree))]
           [(branch1 n a1)    (append (list (pathnode path tree)) (f (append path (list 'd)) a1))]
           [(branch2 n a1 a2) (append (list (pathnode path tree)) (f (append path (list 'l)) a1) (f (append path (list 'r)) a2))]))
  (f null tree))

(define (random-subtree tree)
  (let ([nodes (tree->list tree)])
    (list-ref nodes (random (length nodes)))))


(define (copy-tree tree)
  (match tree [(leaf n) (leaf n)]
         [(branch1 n a1) (branch1 n (copy-tree a1))]
         [(branch2 n a1 a2) (branch2 n (copy-tree a1) (copy-tree a2))]))

(define (follow-zipper zip tree)
  (if (null? zip)
    null
    (if (null? (cdr zip))
      (pathnode (car zip) tree)
      (match tree [(leaf x) (raise "bad")]
             [(branch1 n a1) (follow-zipper (cdr zip) a1)]
             [(branch2 n a1 a2) 
              (match (car zip)
                     ['l (follow-zipper (cdr zip) a1)]
                     ['r (follow-zipper (cdr zip) a2)])]))))

(define (tree-depth tree)
  (match tree [(leaf _) 0]
         [(branch1 n a1) (add1 (tree-depth a1))]
         [(branch2 n a1 a2) (add1 (max (tree-depth a1) (tree-depth a2)))]))


;; Ramped Half-and-Half
;; ====================

(define (sym->proc sym ftable)
  (fn-proc (car (filter (lambda (x) (equal? (fn-sym x) sym)) ftable))))

(define (eval-symtree symtree arg ftable)
  (define (f subtree)
    (match subtree [(leaf 'x)            arg]
           [(leaf  r)              r]
           [(branch1 sym a1)       ((sym->proc sym ftable) (f a1))]
           [(branch2 sym a1 a2)    ((sym->proc sym ftable) (f a1) (f a2))]))
  (f symtree))

(struct fitcase (input output) #:transparent)
(define (create-fitness-cases fn minp maxp n)
  (map (lambda (x) (fitcase x (fn x)))
       (map (lambda (x) (+ minp (* (- maxp minp) (/ x n))))
            (range 0 (+ n 1)))))

(define (calc-fit-diffs fitcases symtree ftable)
  (map (lambda (x)
         (let ([rslt (eval-symtree symtree (fitcase-input x) ftable)])
           (fitcase (fitcase-input x) 
                    (sqr (- (fitcase-output x) rslt)))))
       fitcases))

(define (calc-fitness-from-diffs fitdiffs)
  (let ([sum (foldl + 0 (map (lambda (x) (fitcase-output x)) fitdiffs))])
    (sqrt (/ sum *nfitcases*))))

(define (calc-fitness fitcases ftable)
  (lambda (symtree)
    (calc-fitness-from-diffs (calc-fit-diffs fitcases symtree ftable))))

(define (selection-tournament fpop tourny-size prob-best)
  (let* ([tpop (take (shuffle fpop) tourny-size)])           ; Select the individuals for tournament
    (if (chance prob-best)
      (car (sort tpop (lambda (x y) (< (car x) (car y))))) ; bprob % of the time take the best
      (car (shuffle tpop)))))                              ; Otherwise take a random one

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form) 
      (eval `(lambda (x) ,form) ns))))

(define (symtree->proc symtree ftable)
  (match symtree [(leaf 'x) 'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable))]
         [(branch2 sym a1 a2) `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable) ,(symtree->proc a2 ftable))]))

(define (symtree->source symtree)
  (match symtree [(leaf 'x) 'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,sym ,(symtree->source a1))]
         [(branch2 sym a1 a2) `(,sym ,(symtree->source a1) ,(symtree->source a2))]))

(define (crossover-one-point p1 p2)
  (define (find-common-tree p1 p2 acc)
    (match (list p1 p2) 
           [(list (leaf a) (leaf b)) null]
           [(list (branch1 n1 a1) (branch1 n2 a2)) null]
           [(list (branch2 n1 a11 a12) (branch2 n2 a21 a22)) null]
           [_ null]))
  null)

(define (callback target fit individual)
  (displayln "--------------------------------------------------------------------------------")
  (displayln fit)
  (pretty-print (symtree->source individual))
  (update-canvas (window-canvas *window*)
                 (eval-form (symtree->proc individual *func-table*))
                 target -5 5 600 600 fit))

;; GP Algorithm
;; ============
(define (generic-gp popsize maxheight ftable termlist callback)

  (define (create-next-generation mutation% fpop size tourny-size)
    (map (lambda (x)
           (let* ([p1 (selection-tournament fpop tourny-size 1)]
                  [p2 (selection-tournament fpop tourny-size 1)]
                  [candidate (crossover (cadr p1) (cadr p2))])
             (if (chance mutation%) (mutation-point candidate *max-init-program-size*) candidate)))
         (range 0 size)))

  (define (full level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1 ['x (leaf 'x)]
               ['R (leaf (* 3 (random)))]))
      (let ([f (random-fn ftable)])
        (match f [(fn sym 1 _) (branch1 sym (full (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (full (add1 level) maxdepth)
                                      (full (add1 level) maxdepth))]))))
  (define (grow level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1 ['x (leaf 'x)]
               ['R (leaf (* 3 (random)))]))
      (let ([e1 (random-item ftable termlist)])
        (match e1 [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                      (grow (add1 level) maxdepth))]
               ['x (leaf 'x)]
               ['R (leaf (random))]))))

  (define (ramped-half-and-half popsize)
    (let ([count (/ (/ popsize (- (add1 maxheight) 2)) 2)])
      (apply append (map (lambda (maxdepth)
                           (append (map (lambda (x) (full 0 maxdepth)) (range 0 count))
                                   (map (lambda (x) (grow 0 maxdepth)) (range 0 count))))
                         (range 2 (add1 maxheight))))))

  (define (crossover p1 p2)
    (let* ([p1cpy          (copy-tree p1)]
           [p2cpy          (copy-tree p2)]
           [subtree        (random-subtree p1cpy)]
           [incoming-place (random-subtree p2cpy)])
      (if (leaf? p2)
        (pathnode-node subtree)
        (if (null? (pathnode-path incoming-place))
          (crossover p1 p2)
          (let ([parent (follow-zipper (pathnode-path incoming-place) p2cpy)])
            (if (null? parent)
              (crossover p1 p2)
              (let ([parent-node (pathnode-node parent)])
                (cond [(leaf? parent-node) (pathnode-node subtree)]
                      [(branch1? parent-node) (set-branch1-a1! parent-node (pathnode-node subtree)) p2cpy]
                      [(branch2? parent-node) 
                       (match (pathnode-path parent)
                              ['l (set-branch2-a1! parent-node (pathnode-node subtree)) (if (> (tree-depth p2cpy) *max-run-program-size*) (crossover p1 p2) p2cpy)]
                              ['r (set-branch2-a2! parent-node (pathnode-node subtree)) (if (> (tree-depth p2cpy) *max-run-program-size*) (crossover p1 p2) p2cpy)])]))))))))

  (define (mutation-point symtree max-depth)
    (crossover symtree (grow 0 max-depth)))

  (define (loop fn fitcases last-best last-pop)
    (let* ([fits (map (calc-fitness fitcases *func-table*) last-pop)]
           [fpop (map list fits last-pop)]
           [curr-best (argmin (lambda (x) (car x)) fpop)]
           [new-best (if (or (null? last-best) (< (car curr-best) (car last-best))) curr-best last-best)])
      (callback fn (car new-best) (cadr new-best))
      (loop fn fitcases new-best (cons (cadr new-best) (create-next-generation *%mutation* fpop *pop-size* *tourny-size*)))))

  ;; Generate initial population of random programs
  (let* ([initial-population (ramped-half-and-half popsize)]
         ;; Repeat until termination condition
         ;;     Execute each program and assign it a fitness
         ;;     Create a new population by applying the following operations to programs selected with fitness based on probability:
         ;;         Reproduce a progam by copying it into the new generation
         ;;         create 2 new programs by crossover
         ;;     Designate best program so far
         ;[fn (lambda (x) (+ 1 (+ x (+ (expt (* 2 x) 2) (expt (* 3 x) 3)))))]
         [fn (lambda (x) (+ (cos x) (* 3 (sin (expt x 2)))))]
         [fitcases (create-fitness-cases fn -5.0 5.0 *nfitcases*)])
    (loop fn fitcases null initial-population)))

(define (start-regression)
  (generic-gp *pop-size* *max-init-program-size* *func-table* *terminals* callback))

(define *window* null)
(define (main)
  (let ([app-window (create-window "Symbolic Regression" 600 600)])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-regression)))

(main)
