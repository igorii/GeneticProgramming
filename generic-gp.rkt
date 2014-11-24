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

;; ******************
;;    Structures
;; ******************

;; A 1-2 tree for storing programs containing functions up to arity 2
(struct leaf    (x)       #:transparent #:mutable)
(struct branch1 (n a1)    #:transparent #:mutable)
(struct branch2 (n a1 a2) #:transparent #:mutable)
(struct fn (sym arity proc))
(struct pathnode (path node) #:transparent)

;; ******************
;;      Helpers
;; ******************

;; Convert a file to a program
(define (file->program path)
  (string->program (file->value path)))

;; Convert a program to a string
(define (program->string program)
  (match program 
         [(leaf n) 
          (symbol->string n)]
         [(branch1 n a1) 
          (string-append "(" (symbol->string n) " " (program->string a1) ")")]
         [(branch2 n a1 a2) 
          (string-append "(" (symbol->string n) " " (program->string a1) " " (program->string a2) ")")]))

;; Convert a string to a program
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

;; Convert a tree of nodes to a list of nodes
(define (tree->list tree)
  (define (f path tree)
    (match tree 
           ;; Leaves are a list of one node
           [(leaf n)
            (list (pathnode path tree))]
           ;; Single branches are a list of the symbol and (tree->list arg)
           [(branch1 n a1)
            (append (list (pathnode path tree))
                    (f (append path (list 'd)) a1))]
           ;; Double branches are a list of the symbol and (tree->list arg) for both args
           [(branch2 n a1 a2) 
            (append (list (pathnode path tree))
                    (f (append path (list 'l)) a1)
                    (f (append path (list 'r)) a2))]))
  (f null tree))

;; Obtain a random subtree from the given tree
(define (random-subtree tree)
  (let ([nodes (tree->list tree)])
    (list-ref nodes (random (length nodes)))))

;; Copy a tree
(define (copy-tree tree)
  (match tree 
         [(leaf n) (leaf n)]
         [(branch1 n a1) (branch1 n (copy-tree a1))]
         [(branch2 n a1 a2) (branch2 n (copy-tree a1)
                                       (copy-tree a2))]))

;; Follow a zipper construct through a tree to obtain a subtree. A
;; zipper is a list of directions through a tree containing symbols
;; 'l and 'r for left and right flow.
(define (follow-zipper zip tree)
  (if (null? zip)
    null
    (if (null? (cdr zip))
      (pathnode (car zip) tree)
      (match tree 
             [(leaf x)        (raise "bad zipper")]
             [(branch1 n a1)  (follow-zipper (cdr zip) a1)]
             [(branch2 n a1 a2)
              (match (car zip)
                     ['l (follow-zipper (cdr zip) a1)]
                     ['r (follow-zipper (cdr zip) a2)])]))))

;; Retrieve the depth of a given tree
(define (tree-depth tree)
  (match tree 
         [(leaf _)          0]
         [(branch1 n a1)    (add1 (tree-depth a1))]
         [(branch2 n a1 a2) (add1 (max (tree-depth a1) 
                                       (tree-depth a2)))]))

;; Convert a symbol in an ftable to a procedure
(define (sym->proc sym ftable)
  (fn-proc (car (filter (lambda (x) (equal? (fn-sym x) sym)) ftable))))

;; Evaluate a symtree
(define (eval-symtree symtree arg ftable)
  (define (f subtree)
    (match subtree 
           [(leaf 'x)            arg]
           [(leaf  r)              r]
           [(branch1 sym a1)       ((sym->proc sym ftable) (f a1))]
           [(branch2 sym a1 a2)    ((sym->proc sym ftable) (f a1) (f a2))]))
  (f symtree))

;; Evaluate a form in a new namespace parameterized by the symbol 'x
;; This returns a function of a sinlge parameter than runs the given
;; form on the parameter
(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form) 
      (eval `(lambda (x) ,form) ns))))

;; Convert a symtree to a structural symbol of procedures
(define (symtree->proc symtree ftable)
  (match symtree 
         [(leaf 'x)           'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable))]
         [(branch2 sym a1 a2) `(,(sym->proc sym ftable) ,(symtree->proc a1 ftable) ,(symtree->proc a2 ftable))]))

;; Convert a symtree to a form that can be printed nicely (removing the internal
;; structure of the program)
(define (symtree->source symtree)
  (match symtree 
         [(leaf 'x)           'x]
         [(leaf  r)            r]
         [(branch1 sym a1)    `(,sym ,(symtree->source a1))]
         [(branch2 sym a1 a2) `(,sym ,(symtree->source a1) ,(symtree->source a2))]))


;; ******************
;;    GP Algorithm
;; ******************

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

  ;; Define a sorting function that sorts in the direction appropriate for
  ;; a minimizing or maximizing algorithm
  (define sortf
    (if minimizing
      (lambda (x y) (< (car x) (car y)))
      (lambda (x y) (> (car x) (car y)))))

  ;; Return the best (regardless of fitness direction) in a population
  (define (get-best fpop)
    (car (sort fpop sortf)))

  ;; Determine whether the second individual is better than the first
  (define (better? last new)
    (not (eq? last (get-best (list last new)))))

  ;; Regular tournament selection
  ;; The best individual in a tournament will be taken `prob-best`
  ;; percent of the time
  (define (selection-tournament fpop tourny-size prob-best)
    (let* ([tpop (take (shuffle fpop) tourny-size)]) ; Select the individuals for tournament
      (if (chance prob-best)
        (let ([best (car (sort tpop sortf))])
          (car (shuffle (filter (lambda (x) (= (car x) (car best))) tpop))))
        (car (shuffle tpop))))) ; Otherwise take a random one

  ;; Create the next generation from a current generation with the given rate
  ;; of mutation, using tournament selection
  (define (create-next-generation mutation% fpop size tourny-size)
    (map (lambda (x)
           (let* ([p1        (selection-tournament fpop tourny-size 1)] ; Get the two parents
                  [p2        (selection-tournament fpop tourny-size 1)]
                  [candidate (crossover (cadr p1) (cadr p2))]) ; Generate the child
             (if (chance mutation%) (mutation-point candidate max-init-tree-height) candidate)))
         (range 0 size))) ; Run generation once for each individual

  ;; Define how ERNs should be handled when encountered
  (define (interperetERN)
    (* 3 (random)))

  ;; Generate full trees of a given height
  (define (full level maxdepth)
    (if (= level maxdepth) ; If reached the height, select a random terminal
      (let ([t1 (random-term termlist)])
        (match t1
               ['x (leaf 'x)]              ; x denotes an argument
               ['R (leaf (interperetERN))] ; R denotes an ERN
               [a  (leaf a)]))             ; Otherwise any discovered terminal is used
      (let ([f (random-fn ftable)]) ; Otherwise choose a random function and recurse
        (match f
               [(fn sym 1 _) (branch1 sym (full (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (full (add1 level) maxdepth)
                                          (full (add1 level) maxdepth))]))))
  ;; Generate grow trees of a given height
  (define (grow level maxdepth)
    (if (= level maxdepth) ; If reached the height, select a random terminal
      (let ([t1 (random-term termlist)])
        (match t1 
               ['x (leaf 'x)]             ; x denotes an argument
               ['R (leaf (* 3 (random)))] ; R denotes an ERN
               [a  (leaf a)]))            ; Otherwise any discovered terminal is used
      (let ([e1 (random-item ftable termlist)]) ; Otherwise choose a random function *OR* terminal and recurse
        (match e1 
               [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                          (grow (add1 level) maxdepth))]
               ['x (leaf 'x)]
               ['R (leaf (random))]
               [a  (leaf a)]))))

  ;; Ramped half-and-half generation of a population
  (define (ramped-half-and-half popsize)
    (let ([count (/ (/ popsize (- (add1 max-init-tree-height) 2)) 2)])
      (apply append
             (map (lambda (maxdepth)
                    (append (intialize-sub-pop grow count maxdepth)
                            (intialize-sub-pop full count maxdepth)))
                  (range 2 (add1 max-init-tree-height))))))

  ;; Initialize a population of a given size using a specified generation function
  ;; (either grow or full). Individuals will be stored in a hash map to avoid creating
  ;; identical individuals, ensuring maximum genes in a pool.
  (define (intialize-sub-pop method n maxdepth)
    (define (loop n acc-dict)
      (if (= n 0)
        (map car (hash->list acc-dict))
        (let ([candidate (method 0 maxdepth)])
          (if (hash-has-key? acc-dict candidate)
            (loop n acc-dict) ; To ensure diversity in the initial population, skip any duplicates
            (loop (- n 1) (hash-set acc-dict candidate null))))))
    (loop n (make-immutable-hash '()))) ; Initialize all individuals

  ;; Perform single point crossover between two parent programs
  (define (crossover p1 p2)
    (let* ([p1cpy          (copy-tree p1)] ; Copy the two parent trees to avoid mutation
           [p2cpy          (copy-tree p2)]
           [subtree        (random-subtree p1cpy)]  ; Select a source and destination subtree
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
                      [(branch1? parent-node)
                       (set-branch1-a1! parent-node (pathnode-node subtree)) p2cpy]
                      [(branch2? parent-node)
                       (match (pathnode-path parent)
                              ['l (set-branch2-a1! parent-node (pathnode-node subtree))
                               (if (> (tree-depth p2cpy) max-run-tree-height) (crossover p1 p2) p2cpy)]
                              ['r (set-branch2-a2! parent-node (pathnode-node subtree))
                               (if (> (tree-depth p2cpy) max-run-tree-height) (crossover p1 p2) p2cpy)])]))))))))

  ;; Perform single point crossover by mutating a parent with a new grown tree
  (define (mutation-point symtree max-depth)
    (crossover symtree (grow 0 max-depth)))

  ;; MAIN GP LOOP
  (define (loop iter last-best last-pop)
    ;; If the generations allowed has been exceeded, then return the best
    (if (and (> generations 0) (> iter generations))
      last-best
      ;; Otherwise proceed in usual GA way
      (let* ([fits (map calc-fitness last-pop)]  ; Get the population fitness
             [fpop (map list fits last-pop)]     ; Zip the fitness with the population
             [curr-best (get-best fpop)]         ; Find the best in the current pop
             [new-best (if (or (null? last-best) ; Replace the last best if the new best is better
                               (better? last-best curr-best)) 
                         curr-best last-best)])
        (callback (car new-best) (cadr new-best) iter) ; Run the callback if assigned with the new best
        ;; If reached the minimum value in a minimizing GP, return the best
        (if (and minimizing (>= 0 (car new-best)))
          new-best
          ;; Otherwise recurse with the new population, using elitism
          (loop (add1 iter) new-best (cons (cadr new-best) (create-next-generation %mutation fpop popsize tourny-size)))))))

  ;; Create an initial population and begin the GP loop
  (let* ([initial-population (ramped-half-and-half popsize)])
    (loop 1 null initial-population)))

