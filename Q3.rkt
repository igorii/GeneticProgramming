#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "regression-drawing.rkt")
(require "datatypes.rkt")

(define *grid* (grid 60 6 (* 6 60)))
(define *nfood* 20)
(define *food-amt* 50)
(define *nants* 50)
(define *window* null)
(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 40))
(define *decay-amt* 5)
(define *drop-amt* 5)
(define *max-amt* 255)

(define *homept* (/ (grid-ncells *grid*) 2))
(define *init-world* (world 0
                            (pt *homept* *homept*) 
                            (map (lambda (_) (ant (pt *homept* *homept*) #f)) 
                                 (range 0 *nants*)) 
                            null
                            (make-cells (grid-ncells *grid*))
                            *max-amt*))

(define *pop-size* 1000)
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
(define *nfitcases* 100)
(define *terminals* (list 'move-random
                          'move-to-nest
                          'pick-up
                          'drop-phermn))

(define *func-table* (list (gp:fn 'if-food-here 2 null)
                           (gp:fn 'if-carrying-food 2 null)
                           (gp:fn 'move-to-adjacent-food-else 1 null)
                           (gp:fn 'move-to-adjacent-phermn-else 1 null)
                           (gp:fn 'progn 2 null)))

(define (make-fitness-fn iterations init-world)
  (lambda (program) 
    (define (loop iter w)
      (if (<= iter 0) 
        0
        (begin
          (for ([a (world-ants w)])
             (run-symtree a w program))
          (world-food-at-home w))))
    (loop iterations init-world)))

(define (make-callback)
  (lambda (fit individual)
    (displayln "--------------------------------------------------------------------------------")
    (displayln fit)
    (pretty-print individual)))

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form)
      (eval `(lambda (a w) ,form) ns))))

(define (r-move-random a w) a)
(define (r-move-to-nest a w) a)
(define (r-pick-up a w) a)
(define (r-drop-phermn a w) a)
(define (r-move-to-adjacent-food-else a w sym1) a)
(define (r-move-to-adjacent-phermn-else a w sym1) a)
(define (r-if-food-here a w sym1 sym2) a)
(define (r-if-carrying-food a w sym1 sym2) a)
(define (r-progn a w sym1 sym2) a)

(define (run-symtree a w symtree)
  (match symtree
         [(gp:leaf 'move-random)  (r-move-random a w)]
         [(gp:leaf 'move-to-nest) (r-move-to-nest a w)]
         [(gp:leaf 'pick-up)      (r-pick-up a w)]
         [(gp:leaf 'drop-phermn)  (r-drop-phermn a w)]
         [(gp:branch1 'move-to-adjacent-food-else a1)    (r-move-to-adjacent-food-else a w a1)]
         [(gp:branch1 'move-to-adjacent-phermn-else a1)  (r-move-to-adjacent-phermn-else a w a1)]
         [(gp:branch2 'if-food-here a1 a2)               (r-if-food-here a w a1 a2)]
         [(gp:branch2 'if-carrying-food a1 a2)           (r-if-carrying-food a w a1 a2)]
         [(gp:branch2 'progn a1 a2)                      (r-progn a w a1 a2)]))

(define (start-regression)
  (gp:generic-gp
    #:population-size *pop-size*
    #:max-init-tree-height *max-init-program-size*
    #:max-run-tree-height *max-run-program-size*
    #:function-table *func-table*
    #:terminals *terminals*
    #:mutation-rate *%mutation*
    #:tournament-size *tourny-size*
    #:fitness-fn (make-fitness-fn 51 *init-world*)
    #:callback (make-callback)))

(define (main) (start-regression))
(main)
