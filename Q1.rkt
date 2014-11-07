#lang racket

(require "window.rkt")
(require "datatypes.rkt")

(define *grid* (grid 100 6 (* 6 100)))
(define *nfood* 50)
(define *nants* 50)
(define *window* null)
(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 25))

(define (update-ant! a g w)
  ; HF = has food
  ; SF = sense food
  ; SP = sense phermn
  ; AH = at home
  (let ([currcell (get-cell (world-cells w) 
                            (pt-x (ant-pt a)) 
                            (pt-y (ant-pt a)))])
    (cond 
      ; If HF and AH, drop food and move away
      [(and (ant-has-food a) 
            (equal? (ant-pt a) (world-home w)))
       (set-ant-has-food! a #f)]
      ; Else if HF and ~AH, drop phermn and move away
      [(and (ant-has-food a) 
            (not (equal? (ant-pt a) (world-home w))))
       (set-ant-pt! a (gohome (ant-pt a) (world-home w)))]
      ; Else if ~HF and SF, pick up food and go home
      [(and (not (ant-has-food a)) 
            (not (null? (cell-food currcell))))
       (if (>= 1 (cell-food currcell))
         (set-cell-food! currcell null)
         (set-cell-food! currcell (- (cell-food currcell) 1)))
       (set-ant-has-food! a #t)]
      ; Else if ~HF and SP, move to next position along trail (away from home)
      ; Else move random
      [else (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g)))])
    ))

(define (update-world g w)
  (for ([a (world-ants w)])
       (update-ant! a g w))
  w)

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
                        (range 0 *nants*)) 
                   null
                   (make-cells (grid-ncells *grid*)))])
    (place-food! *nfood* 10 (grid-ncells *grid*) (world-cells w))
    (main-loop *grid* w)))

(define (main)
  (let ([app-window (create-window "Ant Colony" (grid-dim *grid*) (grid-dim *grid*))])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-colony)))

(main)
