#lang racket

(require racket/date)
(require racket/pretty)
(require racket/gui/base)
(require racket/match)

(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "ant-drawing.rkt")
(require "datatypes.rkt")
(require "ant-functions.rkt")

(define *window* null)
(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 40))

(define (make-world)
  (define w (blank-world *nants* *homept* (grid-ncells *grid*) *max-amt*))
  (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
  w)

(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *pop-size* 100)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.05)
(define *tourny-size* 7)
(define *elitism* #t)
(define *init-world* (blank-world *nants* *homept* (grid-ncells *grid*) *max-amt*))

(define *terminals* (list 'move-random
                          'move-to-nest
                          'pick-up
                          'drop-phermn))

(define *func-table* (list (gp:fn 'if-food-here 2 null)
                           (gp:fn 'if-carrying-food 2 null)
                           (gp:fn 'move-to-adjacent-food-else 1 null)
                           (gp:fn 'move-to-adjacent-phermn-else 1 null)
                           (gp:fn 'progn 2 null)))

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

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form)
      (eval `(lambda (a w) ,form) ns))))

(define (run-simulation pausefn callback iterations program w)
  (define (cb w i)
    (display (string-append (number->string (world-food-at-home w)) " "))
    (flush-output)
    (callback w i)
    (world-food-at-home w))

  (define (loop iter w)
    (if (pausefn)
      (loop iter w)
      (if (<= iter 0)
        (cb w iter)
        (begin
          (callback w iter)
          (decay-phermn! (world-cells w) (grid-ncells *grid*) *decay-amt*)
          (let ([finished #f])
            (for ([a (world-ants w)])
                 #:break finished
                 (run-symtree! a w program)
                 (when (and (ant-has-food a) (equal? (ant-pt a) (world-home w)))
                   (inc-home-food! w)
                   (set-ant-has-food! a #f))
                 (when (>= (ant-steps a) iterations) (set! finished #t)))
            (if finished 
              (cb w iter)
              (loop (sub1 iter) w)))))))
  (loop iterations w))

(define (make-fitness-fn iterations x)
  (lambda (program)
    (run-simulation (lambda () #f) (lambda (x i) x) 
                    iterations program (make-world))))

(define (timestamp)
  (let ([d (current-date)])
    (string-append
      (number->string (date-year d))   "-"
      (number->string (date-month d))  "-"
      (number->string (date-day d))    "T"
      (number->string (date-hour d))   "-"
      (number->string (date-minute d)) "-"
      (number->string (date-second d)))))

(define (present-program program iterations)
  (define (go)
    (run-simulation
      (lambda ()
        (if (> (current-milliseconds) *next-timestamp*)
          (begin 
            (set! *next-timestamp* 
              (+ *fps* (current-milliseconds)))
            #f)
          #t))
      (lambda (w i)
        (draw-world (window-canvas *window*) *grid* w i))
      iterations
      program
      (make-world)))

  (let ([app-window (create-window "Ant Colony" 700 
                                   (grid-dim *grid*) 
                                   (grid-dim *grid*) 
                                   (grid-dim *grid*)
                                   (lambda (e) null))])
    (write-to-file (gp:program->string program) (string-append "GP-" (timestamp) ".s"))
    (set! *window* app-window)
    (start-gui app-window)
    (thread go)))

(define (start-regression)
  (let ([best (gp:generic-gp
                #:population-size *pop-size*
                #:minimizing #f
                #:max-init-tree-height *max-init-program-size*
                #:max-run-tree-height *max-run-program-size*
                #:function-table *func-table*
                #:terminals *terminals*
                #:mutation-rate *%mutation*
                #:tournament-size *tourny-size*
                #:fitness-fn (make-fitness-fn *steps* null)
                #:generations *max-generations*
                #:callback (make-callback))])
    (present-program (cadr best) *steps*)))

(define (main) (start-regression))
(main)
