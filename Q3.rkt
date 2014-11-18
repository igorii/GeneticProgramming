#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "regression-drawing.rkt")
(require "datatypes.rkt")

(define *grid* (grid 50 6 (* 6 50)))
(define *nfood* 20)
(define *food-amt* 50)
(define *nants* 100)
(define *window* null)
(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 40))
(define *decay-amt* 5)
(define *drop-amt* 5)
(define *max-amt* 255)

(define *homept* (/ (grid-ncells *grid*) 2))

(define (make-world)
  (define w (world 0 (pt *homept* *homept*)
                   (map (lambda (_) (ant (pt *homept* *homept*) #f))
                        (range 0 *nants*))
                   null
                   (make-cells (grid-ncells *grid*))
                   *max-amt*))
  (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
  w)

(define *pop-size* 100)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.05)
(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *elitism* #t)
;(define *terminals* (list 'move-random
;                          'move-to-nest
;                          'pick-up
;                          'drop-phermn))
(define *terminals* (list 'pick-up 'move-random))

;(define *func-table* (list (gp:fn 'if-food-here 2 null)
;                           (gp:fn 'if-carrying-food 2 null)
;                           (gp:fn 'move-to-adjacent-food-else 1 null)
;                           (gp:fn 'move-to-adjacent-phermn-else 1 null)
;                           (gp:fn 'progn 2 null)))
(define *func-table* (list (gp:fn 'progn 2 null)))

(define (make-callback)
  (lambda (fit individual)
    (displayln "--------------------------------------------------------------------------------")
    (displayln fit)
    (pretty-print individual)))

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form)
      (eval `(lambda (a w) ,form) ns))))

(define (r-move-random a w)
  (set-ant-pt! a (random-move (ant-pt a) (grid-ncells *grid*))))

(define (r-move-to-nest a w)
  (set-ant-pt! a (gohome (ant-pt a) (world-home w))))

(define (r-pick-up a w)
  (let ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))])
    (when (and (not (ant-has-food a)) (not (null? (cell-food currcell))))
      (if (>= 1 (cell-food currcell))
        (set-cell-food! currcell null)
        (set-cell-food! currcell (- (cell-food currcell) 1)))
      (set-ant-has-food! a #t))))

(define (r-drop-phermn a w) 
  (drop-phermn! a *grid* w *drop-amt*))

(define (r-move-to-adjacent-food-else a w sym1)
  (let* ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
         [adjcells (adjacent-cells (ant-pt a) (world-cells w) (grid-ncells *grid*))]
         [cellswithfood (filter (lambda (x) (not (null? (cell-food x)))) adjcells)])
    (if (not (null? cellswithfood))
      (let ([foodcell (car cellswithfood)])
        (set-ant-pt! a (cell-pt foodcell)))
      (run-symtree! a w sym1))))

(define (r-move-to-adjacent-phermn-else a w sym1)
  (let* ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
         [adj-phermn-cells (adj-phermn-cells (ant-pt a) (world-cells w) (grid-ncells *grid*))])
    (if (null? adj-phermn-cells)
      (run-symtree! a w sym1)
      (let ([farther-pts
              (filter (lambda (x) (> (distance (cell-pt x) (world-home w))
                                     (distance (ant-pt a)  (world-home w))))
                      adj-phermn-cells)])
        (if (null? farther-pts)
          (set-ant-pt! a (random-move (ant-pt a) (grid-ncells *grid*)))
          (set-ant-pt! a (cell-pt (argmax (lambda (x) (cell-phermn x)) farther-pts))))))))

(define (r-if-food-here a w sym1 sym2)
  (let ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))])
    (if (not (null? (cell-food currcell)))
      (run-symtree! a w sym1)
      (run-symtree! a w sym2))))

(define (r-if-carrying-food a w sym1 sym2) 
  (if (ant-has-food a)
    (run-symtree! a w sym1)
    (run-symtree! a w sym2)))

(define (r-progn a w sym1 sym2)
  (begin 
    (run-symtree! a w sym1)
    (run-symtree! a w sym2)))

(define (run-symtree! a w symtree)
  (match symtree
         [(gp:leaf 'move-random)                         (r-move-random a w)]
         [(gp:leaf 'move-to-nest)                        (r-move-to-nest a w)]
         [(gp:leaf 'pick-up)                             (r-pick-up a w)]
         [(gp:leaf 'drop-phermn)                         (r-drop-phermn a w)]
         [(gp:branch1 'move-to-adjacent-food-else a1)    (r-move-to-adjacent-food-else a w a1)]
         [(gp:branch1 'move-to-adjacent-phermn-else a1)  (r-move-to-adjacent-phermn-else a w a1)]
         [(gp:branch2 'if-food-here a1 a2)               (r-if-food-here a w a1 a2)]
         [(gp:branch2 'if-carrying-food a1 a2)           (r-if-carrying-food a w a1 a2)]
         [(gp:branch2 'progn a1 a2)                      (r-progn a w a1 a2)]))

(define (make-fitness-fn iterations x)
  (lambda (program)
    (define (loop iter w)
      (if (<= iter 0)
        (begin 
          (displayln (list "  Fitness : " (world-food-at-home w)))
          (world-food-at-home w)
         )

        (begin
          (for ([a (world-ants w)])
               (run-symtree! a w program)
               (when (ant-has-food a) ;(equal? (ant-pt a) (world-home w)))
                 (inc-home-food! w)
                 (set-ant-has-food! a #f)))
          (loop (sub1 iter) w))))
    (loop iterations (make-world))))

(define (start-regression)
  (gp:generic-gp
    #:population-size *pop-size*
    #:minimizing #f
    #:max-init-tree-height *max-init-program-size*
    #:max-run-tree-height *max-run-program-size*
    #:function-table *func-table*
    #:terminals *terminals*
    #:mutation-rate *%mutation*
    #:tournament-size *tourny-size*
    #:fitness-fn (make-fitness-fn 500 null)
    #:callback (make-callback)))

(define (main) (start-regression))
(main)
