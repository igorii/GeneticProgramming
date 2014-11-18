#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "regression-drawing.rkt")
(require "datatypes.rkt")

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

(define (make-fitness-fn)
    (lambda (program) 0))

(define (make-callback)
  (lambda (fit individual)
    (displayln "--------------------------------------------------------------------------------")
    (displayln fit)
    (pretty-print individual)))

(define (start-regression)
    (gp:generic-gp
      #:population-size *pop-size*
      #:max-init-tree-height *max-init-program-size*
      #:max-run-tree-height *max-run-program-size*
      #:function-table *func-table*
      #:terminals *terminals*
      #:mutation-rate *%mutation*
      #:tournament-size *tourny-size*
      #:fitness-fn (make-fitness-fn)
      #:callback (make-callback)))

(define (main) (start-regression))
(main)
