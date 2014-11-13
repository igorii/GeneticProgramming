#lang racket

(require racket/gui/base)
(require racket/match)
;(require plot)
;(plot-new-window? #t)
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
(define (r-exp a b) ;(foldl * 1 (map (lambda (x) a) (range 0 b)))) 
  ;(if (not (and (real? a) (real? b)))
  ;  +inf.f
  (if (and (= a 0) (< b 0)) 0 
    (let ([t (expt a b)])
      (if (not (complex? t)) t 1))))

(define *pop-size* 1000)
(define *success* 20)
(define *precision* 0.001)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.01)
(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *elitism* #t)
(define *terminals* (list 'x 'R))
(define *nfitcases* 20)

(struct leaf    (x)       #:transparent)
(struct branch1 (n a1)    #:transparent)
(struct branch2 (n a1 a2) #:transparent)

(define (tree-size tree)
  (match tree [(leaf _) 1]
              [(branch1 _ a1) (add1 (tree-size a1))]
              [(branch2 _ a1 a2) (add1 (+ (tree-size a1) (tree-size a2)))]))

(define (random-subtree tree)
  (define (search tree p)
    (if (chance p)
      (list tree)
      (match tree [(leaf x) null]
                  [(branch1 _ a1) (search a1 p)]
                  [(branch2 _ a1 a2) (append (search a1) (search a2))])))
  (let ([nodes (search tree (/ 1 (tree-size tree)))])
    (if (null? nodes) tree (car (shuffle nodes)))))

(define (crossover p1 p2)
  (define (insert node tree p cb)
    (if inserted
      )
    )

  (let ([subtree (random-subtree p1)])
    
    )
  )

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

;; Ramped Half-and-Half
;; ====================
(define (ramped-half-and-half popsize maxheight ftable termlist)
  (define (full level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1 ['x (leaf 'x)]
               ['R (leaf (random))]))
      (let ([f (random-fn ftable)])
        (match f [(fn sym 1 _) (branch1 sym (full (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (full (add1 level) maxdepth)
                                      (full (add1 level) maxdepth))]))))
  (define (grow level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1 ['x (leaf 'x)]
               ['R (leaf (random))]))
      (let ([e1 (random-item ftable termlist)])
        (match e1 [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                      (grow (add1 level) maxdepth))]
               ['x (leaf 'x)]
               ['R (leaf (random))]))))
  (let ([count (/ (/ popsize (- (add1 maxheight) 2)) 2)])
    (apply append (map (lambda (maxdepth)
                         (append (map (lambda (x) (full 0 maxdepth)) (range 0 count))
                                 (map (lambda (x) (grow 0 maxdepth)) (range 0 count))))
                       (range 2 (add1 maxheight))))))

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
      (first (sort tpop (lambda (x y) (< (car x) (car y))))) ; bprob % of the time take the best
      (first (shuffle tpop)))))                              ; Otherwise take a random one

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form) 
      (eval `(lambda (x) ,form) ns))))

(define (symtree->proc symtree ftable)
  (match symtree [(leaf 'x) 'x]
         [(leaf  r)  r]
         [(branch1 sym a1)    `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable))]
         [(branch2 sym a1 a2) `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable) ,(symtree->proc a2 ftable))]))

(define (crossover-one-point p1 p2)
  (define (find-common-tree p1 p2 acc)
    (match (list p1 p2) 
           [(list (leaf a) (leaf b)) null]
           [(list (branch1 n1 a1) (branch1 n2 a2)) null]
           [(list (branch2 n1 a11 a12) (branch2 n2 a21 a22)) null]
           [_ null]))
  null)

;; GP Algorithm
;; ============
(define (generic-gp popsize maxheight ftable termlist)
  (define (loop fn fitcases last-best last-pop)
    (let* ([fits (map (calc-fitness fitcases *func-table*) last-pop)]
           [fpop (map list fits last-pop)]
           [curr-best (argmin (lambda (x) (car x)) fpop)]
           [new-best (if (or (null? last-best) (< (car curr-best) (car last-best))) curr-best last-best)])
      (displayln (car new-best))
      (update-canvas (window-canvas *window*) (eval-form (symtree->proc (cadr new-best) ftable)) fn -5 5 600 600)
      (loop fn fitcases new-best (ramped-half-and-half popsize maxheight ftable termlist))))

  ;; Generate initial population of random programs
  (let* ([initial-population (ramped-half-and-half popsize maxheight ftable termlist)]
    ;; Repeat until termination condition
    ;;     Execute each program and assign it a fitness
    ;;     Create a new population by applying the following operations to programs selected with fitness based on probability:
    ;;         Reproduce a progam by copying it into the new generation
    ;;         create 2 new programs by crossover
    ;;     Designate best program so far
         [fn (lambda (x) (+ 1 (+ x (+ (expt (* 2 x) 2) (expt (* 3 x) 3)))))]
         [fitcases (create-fitness-cases fn -5.0 5.0 *nfitcases*)])
    (loop fn fitcases null initial-population)))

(define (start-regression)
  (generic-gp *pop-size* *max-init-program-size* *func-table* *terminals*))

(define *window* null)
(define (main)
  (let ([app-window (create-window "Symbolic Regression" 600 600)])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-regression)))

(main)
