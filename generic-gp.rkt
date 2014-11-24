#lang racket

(require racket/date)
(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require "generic-window.rkt")

(provide program->string string->program file->program)
(provide generic-gp eval-symtree symtree->source symtree->proc)
(provide (struct-out fn))
(provide (struct-out leaf))
(provide (struct-out branch1))
(provide (struct-out branch2))

(struct leaf    (x)       #:transparent #:mutable)
(struct branch1 (n a1)    #:transparent #:mutable)
(struct branch2 (n a1 a2) #:transparent #:mutable)
(struct fn (sym arity proc))
(struct pathnode (path node) #:transparent)

(define (file->program path)
  (string->program 
    (file->value path)))

(define (program->string program)
  (match program 
         [(leaf n) 
          (symbol->string n)]
         [(branch1 n a1) 
          (string-append "(" (symbol->string n) " " (program->string a1) ")")]
         [(branch2 n a1 a2) 
          (string-append "(" (symbol->string n) " " (program->string a1) " " (program->string a2) ")")]))

(define (string->program str)
  (define (parse-list l)
    (match l
           ['move-random  (leaf 'move-random)]
           ['move-to-nest (leaf 'move-to-nest)]
           ['pick-up      (leaf 'pick-up)]
           ['drop-phermn  (leaf 'drop-phermn)]
           [(list 'move-to-adjacent-food-else x ...)
            (branch1 'move-to-adjacent-food-else
                        (parse-list (cadr l)))]
           [(list 'move-to-adjacent-phermn-else x ...)
            (branch1 'move-to-adjacent-phermn-else
                        (parse-list (cadr l)))]
           [(list 'if-food-here x ...)
            (branch2 'if-food-here 
                        (parse-list (cadr l))
                        (parse-list (caddr l)))]
           [(list 'if-carrying-food x ...)
            (branch2 'if-carrying-food
                        (parse-list (cadr l))
                        (parse-list (caddr l)))]
           [(list 'progn x ...)
            (branch2 'progn
                        (parse-list (cadr l))
                        (parse-list (caddr l)))]))
  (parse-list (read (open-input-string str))))

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
          #:minimizing [minimizing #t]
          #:generations [generations -1]
          #:callback [callback (lambda (x) x)])

  ;; Utils
  (define (chance p) (< (random) p))
  (define (random-fn   ftable)       (car (shuffle ftable)))
  (define (random-term terms)        (car (shuffle terms)))
  (define (random-item ftable terms) (car (shuffle (append ftable terms))))
  (define sortf
    (if minimizing
      (lambda (x y) (< (car x) (car y)))
      (lambda (x y) (> (car x) (car y)))))
  (define (get-best fpop)
    (car (sort fpop sortf)))
  (define (better? last new)
    (not (eq? last (get-best (list last new)))))

  (define (selection-tournament fpop tourny-size prob-best)
    (let* ([tpop (take (shuffle fpop) tourny-size)])         ; Select the individuals for tournament
      (if (chance prob-best)
        (let ([best (car (sort tpop sortf))])
          (car (shuffle (filter (lambda (x) (= (car x) (car best))) tpop))))
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
               ['R (leaf (* 3 (random)))]
               [a  (leaf a)]))
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
               ['R (leaf (* 3 (random)))]
               [a  (leaf a)]))
      (let ([e1 (random-item ftable termlist)])
        (match e1 
               [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                      (grow (add1 level) maxdepth))]
               ['x (leaf 'x)]
               ['R (leaf (random))]
               [a  (leaf a)]))))

  (define (intialize-sub-pop method n maxdepth)
    (define (loop n acc-dict)
      (if (= n 0)
        (map car (hash->list acc-dict))
        (let ([candidate (method 0 maxdepth)])
          (if (hash-has-key? acc-dict candidate)
            (loop n acc-dict) ; To ensure diversity in the initial population, skip any duplicates
            (loop (- n 1) (hash-set acc-dict candidate null))))))
    (loop n (make-immutable-hash '()))) ; Initialize all individuals

  (define (ramped-half-and-half popsize)
    (let ([count (/ (/ popsize (- (add1 max-init-tree-height) 2)) 2)])
      (apply append
             (map (lambda (maxdepth)
                    (append (intialize-sub-pop grow count maxdepth)
                            (intialize-sub-pop full count maxdepth)))
                    ;(append (map (lambda (x) (full 0 maxdepth)) (range 0 count))
                    ;        (map (lambda (x) (grow 0 maxdepth)) (range 0 count))))
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

  (define (loop iter last-best last-pop)
    (if (and (> generations 0) (> iter generations))
      last-best
      (let* ([fits (map calc-fitness last-pop)]
             [fpop (map list fits last-pop)]
             [curr-best (get-best fpop)];(argmin (lambda (x) (car x)) fpop)]
             [new-best (if (or (null? last-best) 
                               (better? last-best curr-best)) 
                         curr-best 
                         last-best)])
        (callback (car new-best) (cadr new-best) iter)
        (loop (add1 iter) new-best (cons (cadr new-best) (create-next-generation %mutation fpop popsize tourny-size))))))

  (let* ([initial-population (ramped-half-and-half popsize)])
    (displayln (length initial-population))
    (loop 1 null initial-population)))

