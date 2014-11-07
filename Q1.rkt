#lang racket

(require "window.rkt")
(require "datatypes.rkt")

(define *grid* (grid 100 4 (* 4 100)))
(define *window* null)

(define (update-world g w)
  (for ([a (world-ants w)])
       (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g))))
  ;(for ([x (range 0 (grid-ncells g))])
  ;     (for ([y (range 0 (grid-ncells g))])
  ;          (set-cell! (world-cells w) x y
  ;                     (struct-copy cell
  ;                                  (get-cell (world-cells w) x y)
  ;                                  [phermn (inexact->exact 
  ;                                            (floor (* 255 (random))))]))))
  w)

(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 25))
(define (main-loop grid w)
  (when (> (current-milliseconds) *next-timestamp*)
    (set! w (update-world grid w))
    (draw-world (window-canvas *window*) grid w)
    (set! *next-timestamp* (+ *fps* (current-milliseconds))))
  (main-loop grid w))

(define (start-colony)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (world (pt homept homept) 
                   (map (lambda (_) (ant (pt homept homept) #f)) 
                        (range 0 30)) 
                   null
                   (make-cells (grid-ncells *grid*)))])
    (place-food! 30 10 (grid-ncells *grid*) (world-cells w))
    (main-loop *grid* w)))

(define (main)
  (let ([app-window (create-window "Ant Colony" (grid-dim *grid*) (grid-dim *grid*))])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-colony)))

(main)
