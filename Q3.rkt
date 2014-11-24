#lang racket

;; ******************
;;     Requires
;; ******************

(require racket/date)
(require racket/pretty)
(require (prefix-in gp: "generic-gp.rkt"))
(require "ant-drawing.rkt")
(require "datatypes.rkt")
(require "ant-functions.rkt")

;; ******************
;; Global Parameters
;; ******************

;; GP params
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *pop-size* 100)
(define *max-generations* 51)
(define *%mutation* 0.05)
(define *tourny-size* 7)
(define *init-world* (blank-world *nants* *homept* (grid-ncells *grid*) *max-amt*))

;; The terminal set for the GP
;; Movement commands cause a time step (move-to-nest, move-random)
(define *terminals* (list 'move-random
                          'move-to-nest
                          'pick-up
                          'drop-phermn))

;; The function table for the GP. We will be interpreting the
;; programs ourselves, so we do not add the procedure arguments
(define *func-table* (list (gp:fn 'if-food-here 2 null)
                           (gp:fn 'if-carrying-food 2 null)
                           (gp:fn 'move-to-adjacent-food-else 1 null)
                           (gp:fn 'move-to-adjacent-phermn-else 1 null)
                           (gp:fn 'progn 2 null)))

;; ******************
;;     GP Defines
;; ******************

;; For each best strategy in each generation, display the fitness and the strategy
;; to stdout. Every 10 iterations, write the strategy to a file.
(define (make-callback)
  (lambda (fit individual iter)
    (displayln "")
    (displayln "--------------------------------------------------------------------------------")
    (displayln fit)
    (pretty-print individual)
    (when (= 0 (modulo iter 10))
      (write-to-file
        (gp:program->string individual)
        (string-append "out/GP-" (number->string fit) "-" (timestamp) ".s")))
    (displayln "--------------------------------------------------------------------------------")))

;; Given a strategy as a symbol, parameterized by an ant and a world,
;; evaluate the strategy, mutating both the given ant and world.
(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form)
      (eval `(lambda (a w) ,form) ns))))

;; Returns a function that when given a strategy, will calculate
;; the amount of food that the strategy is able to bring to a nest
;; in a certain number of steps
(define (make-fitness-fn iterations x)
  (lambda (program)
    (run-simulation (lambda ()   #f) ; Never pause in GP
                    (lambda (x i) x) ; noop
                    (lambda (w i)
                      (display (string-append (number->string (world-food-at-home w)) " "))
                      (flush-output)
                      (world-food-at-home w))
                    iterations program (make-world))))

;; Creates a timestamp string from the current time
(define (timestamp)
  (let ([d (current-date)])
    (string-append
      (number->string (date-year d))   "-"
      (number->string (date-month d))  "-"
      (number->string (date-day d))    "T"
      (number->string (date-hour d))   "-"
      (number->string (date-minute d)) "-"
      (number->string (date-second d)))))

;; Starts a GP using the function table and terminal set defined
;; above to find an ant strategy for collecting as much food in
;; a limited number of steps as possible.
(define (start-gp)
  (let ([best (gp:generic-gp
                #:population-size      *pop-size*
                #:minimizing           #f
                #:max-init-tree-height *max-init-program-size*
                #:max-run-tree-height  *max-run-program-size*
                #:function-table       *func-table*
                #:terminals            *terminals*
                #:mutation-rate        *%mutation*
                #:tournament-size      *tourny-size*
                #:fitness-fn           (make-fitness-fn *steps* null)
                #:generations          *max-generations*
                #:callback (make-callback))])
    ;; Write the best result to a file
    (write-to-file (gp:program->string (cadr best)) (string-append "GP-" (number->string (car best)) "-" (timestamp) ".s"))
    ;; And then open a GUI to present it
    (present-program (cadr best) *steps* draw-world)))

;; ******************
;;       Main
;; ******************

(define (main) (start-gp))

(main)
