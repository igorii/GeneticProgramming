#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require "generic-window.rkt")

(provide generic-gp eval-symtree symtree->source symtree->proc)
(provide (struct-out fn))

(struct leaf    (x)       #:transparent #:mutable)
(struct branch1 (n a1)    #:transparent #:mutable)
(struct branch2 (n a1 a2) #:transparent #:mutable)
(struct fn (sym arity proc))
(struct pathnode (path node) #:transparent)

(define (tree->list tree)
  (define (f path tree)
    (match tree 
           [(leaf n)          (list (pathnode path tree))]
           [(branch1 n a1)    (append (list (pathnode path tree)) (f (append path (list 'd)) a1))]
           [(branch2 n a1 a2) (append (list (pathnode path tree)) (f (append path (list 'l)) a1) (f (append path (list 'r)) a2))]))
  (f null tree))

(define (random-subtree tree)
  (let ([nodes (tree->list tree)])
    (list-ref nodes (random (length nodes)))))

(define (copy-tree tree)
  (match tree 
         [(leaf n) (leaf n)]
         [(branch1 n a1) (branch1 n (copy-tree a1))]
         [(branch2 n a1 a2) (branch2 n (copy-tree a1) (copy-tree a2))]))

(define (follow-zipper zip tree)
  (if (null? zip)
    null
    (if (null? (cdr zip))
      (pathnode (car zip) tree)
      (match tree 
             [(leaf x)        (raise "bad")]
             [(branch1 n a1)  (follow-zipper (cdr zip) a1)]
             [(branch2 n a1 a2) 
              (match (car zip)
                     ['l (follow-zipper (cdr zip) a1)]
                     ['r (follow-zipper (cdr zip) a2)])]))))

(define (tree-depth tree)
  (match tree 
         [(leaf _)          0]
         [(branch1 n a1)    (add1 (tree-depth a1))]
         [(branch2 n a1 a2) (add1 (max (tree-depth a1) (tree-depth a2)))]))

(define (sym->proc sym ftable)
  (fn-proc (car (filter (lambda (x) (equal? (fn-sym x) sym)) ftable))))

(define (eval-symtree symtree arg ftable)
  (define (f subtree)
    (match subtree 
           [(leaf 'x)            arg]
           [(leaf  r)              r]
           [(branch1 sym a1)       ((sym->proc sym ftable) (f a1))]
           [(branch2 sym a1 a2)    ((sym->proc sym ftable) (f a1) (f a2))]))
  (f symtree))

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form) 
      (eval `(lambda (x) ,form) ns))))

(define (symtree->proc symtree ftable)
  (match symtree 
         [(leaf 'x)           'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable))]
         [(branch2 sym a1 a2) `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable) ,(symtree->proc a2 ftable))]))

(define (symtree->source symtree)
  (match symtree 
         [(leaf 'x)           'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,sym ,(symtree->source a1))]
         [(branch2 sym a1 a2) `(,sym ,(symtree->source a1) ,(symtree->source a2))]))

;; GP Algorithm
;; ============
(define (generic-gp
          #:population-size popsize
          #:max-init-tree-height max-init-tree-height
          #:max-run-tree-height max-run-tree-height
          #:function-table ftable
          #:terminals termlist
          #:mutation-rate %mutation
          #:tournament-size tourny-size
          #:fitness-fn calc-fitness
          #:callback [callback (lambda (x) x)])

  ;; Utils
  (define (chance p) (< (random) p))
  (define (random-fn   ftable)       (car (shuffle ftable)))
  (define (random-term terms)        (car (shuffle terms)))
  (define (random-item ftable terms) (car (shuffle (append ftable terms))))

  (define (selection-tournament fpop tourny-size prob-best)
    (let* ([tpop (take (shuffle fpop) tourny-size)])         ; Select the individuals for tournament
      (if (chance prob-best)
        (car (sort tpop (lambda (x y) (< (car x) (car y))))) ; bprob % of the time take the best
        (car (shuffle tpop)))))                              ; Otherwise take a random one

  (define (create-next-generation mutation% fpop size tourny-size)
    (map (lambda (x)
           (let* ([p1        (selection-tournament fpop tourny-size 1)]
                  [p2        (selection-tournament fpop tourny-size 1)]
                  [candidate (crossover (cadr p1) (cadr p2))])
             (if (chance mutation%) (mutation-point candidate max-init-tree-height) candidate)))
         (range 0 size)))

  (define (full level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1
               ['x (leaf 'x)]
               ['R (leaf (* 3 (random)))]))
      (let ([f (random-fn ftable)])
        (match f
               [(fn sym 1 _) (branch1 sym (full (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (full (add1 level) maxdepth)
                                      (full (add1 level) maxdepth))]))))
  (define (grow level maxdepth)
    (if (= level maxdepth)
      (let ([t1 (random-term termlist)])
        (match t1 
               ['x (leaf 'x)]
               ['R (leaf (* 3 (random)))]))
      (let ([e1 (random-item ftable termlist)])
        (match e1 
               [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                      (grow (add1 level) maxdepth))]
               ['x (leaf 'x)]
               ['R (leaf (random))]))))

  (define (ramped-half-and-half popsize)
    (let ([count (/ (/ popsize (- (add1 max-init-tree-height) 2)) 2)])
      (apply append
             (map (lambda (maxdepth)
                    (append (map (lambda (x) (full 0 maxdepth)) (range 0 count))
                            (map (lambda (x) (grow 0 maxdepth)) (range 0 count))))
                  (range 2 (add1 max-init-tree-height))))))

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
                              ['l (set-branch2-a1! parent-node (pathnode-node subtree)) (if (> (tree-depth p2cpy) max-run-tree-height) (crossover p1 p2) p2cpy)]
                              ['r (set-branch2-a2! parent-node (pathnode-node subtree)) (if (> (tree-depth p2cpy) max-run-tree-height) (crossover p1 p2) p2cpy)])]))))))))

  (define (mutation-point symtree max-depth)
    (crossover symtree (grow 0 max-depth)))

  (define (loop last-best last-pop)
    (let* ([fits (map calc-fitness last-pop)]
           [fpop (map list fits last-pop)]
           [curr-best (argmin (lambda (x) (car x)) fpop)]
           [new-best (if (or (null? last-best) (< (car curr-best) (car last-best))) curr-best last-best)])
      (callback (car new-best) (cadr new-best))
      (loop new-best (cons (cadr new-best) (create-next-generation %mutation fpop popsize tourny-size)))))

  (let* ([initial-population (ramped-half-and-half popsize)])
    (loop null initial-population)))

