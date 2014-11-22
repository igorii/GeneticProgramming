#lang racket

(require racket/gui/base)
(require "generic-window.rkt")
(require "ant-drawing.rkt")
(require "datatypes.rkt")

;; GUI vars
(define *window*     null)
(define *gui-thread* null)
(define *gui-pause*    #t)
(define *nants*        50)
(define *nfood*        20)
(define *food-amt*     50)
(define *drop-amt*      5)
(define *decay-amt*     5)
(define *current-world* null)
(define *init-world*    null)

(define *grid* (grid 60 6 (* 6 60)))
(define *next-timestamp* (current-milliseconds))
(define *fps* (quotient 1000 40))
(define *max-amt* 255)

(define (update-ant! a g w)
  ;; HF = has food, SF = sense food, SP = sense phermn, AH = at home
  (let ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
        [adj-phermn-cells (adj-phermn-cells (ant-pt a) (world-cells w) (grid-ncells g))])
    (cond 
      [(and (ant-has-food a) (equal? (ant-pt a) (world-home w)))        ; If HF and AH, drop food and move away
       (inc-home-food! w)
       (set-ant-has-food! a #f)]
      [(and (ant-has-food a) (not (equal? (ant-pt a) (world-home w))))  ; Else if HF and ~AH, drop phermn and move away
       (drop-phermn! a g w *drop-amt*)
       (set-ant-pt! a (gohome (ant-pt a) (world-home w)))]
      [(and (not (ant-has-food a)) (not (null? (cell-food currcell))))  ; Else if ~HF and SF, pick up food and go home
       (if (>= 1 (cell-food currcell))
         (set-cell-food! currcell null)
         (set-cell-food! currcell (- (cell-food currcell) 1)))
       (set-ant-has-food! a #t)]
      [(and (not (ant-has-food a)) (not (null? adj-phermn-cells)))      ; Else if ~HF and SP, move to next position along trail (away from home)
       (let ([farther-pts 
               (filter (lambda (x) (> (distance (cell-pt x) (world-home w))
                                      (distance (ant-pt a) (world-home w)))) 
                       adj-phermn-cells)])
         (if (null? farther-pts)
           (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g)))
           (set-ant-pt! a (cell-pt (argmax (lambda (x) (cell-phermn x)) farther-pts)))))]

      [else  ; Else move random
        (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g)))])))

(define (update-world g w)
  (decay-phermn! (world-cells w) (grid-ncells g) *decay-amt*)
  (for ([a (world-ants w)])
       (update-ant! a g w))
  w)

(define (main-loop grid w)
  (when (> (current-milliseconds) *next-timestamp*)
    (draw-world (window-canvas *window*) grid *current-world* -1)
    (set! *next-timestamp* (+ *fps* (current-milliseconds)))
    (when (not *gui-pause*)
      (set! *current-world* (update-world grid *current-world*))))
  (main-loop grid w))

(define (start-colony)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (blank-world *nants* homept (grid-ncells *grid*) *max-amt*)])
    (set! *current-world* (copy-world w))
    (main-loop *grid* *current-world*)))

(define (new-world-with-food)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (blank-world *nants* homept (grid-ncells *grid*) *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    w))

(define (new-run-thread paused)
  (if (thread? *gui-thread*) (kill-thread *gui-thread*) null)
  (set! *gui-pause* paused)
  (set! *gui-thread* (thread start-colony)))

(define (main)
  (let* ([app-window (create-window "Ant Colony" 700 (grid-dim *grid*) (grid-dim *grid*) (grid-dim *grid*))]
         [option-panel (new vertical-panel% [parent (window-panel app-window)])])
    (define pause-btn (new button%
                           [parent option-panel]
                           [label "Play/Pause"]
                           [callback (lambda (button event) (set! *gui-pause* (not *gui-pause*)))]))
    (define restart (new button% 
                         [parent option-panel]
                         [label "Restart"]
                         [callback (lambda (button event) (set! *current-world* (copy-world *init-world*)))]));(new-run-thread #f))]))
    (define randomize (new button% 
                           [parent option-panel]
                           [label "Randomize"]
                           [callback (lambda (button event) 
                                       (set! *current-world* (new-world-with-food))
                                       (set! *init-world* (copy-world (new-world-with-food))))]))
    (define clear (new button% 
                           [parent option-panel]
                           [label "Clear"]
                           [callback (lambda (button event) 
                                       (let* ([homept (/ (grid-ncells *grid*) 2)])
                                         (set! *gui-pause* #t)
                                         (set! *current-world* 
                                           (blank-world *nants* homept 
                                                        (grid-ncells *grid*) *max-amt*))))]))
    (define ant-slider (new slider% 
                            [parent option-panel]
                            [label "# Ants"]
                            [min-value 0]
                            [max-value 100]
                            [init-value *nants*]
                            [callback (lambda (choice event) (set! *nants* (send choice get-value)))]))
    (define nfood-slider (new slider% 
                            [parent option-panel]
                            [label "# Random Food"]
                            [min-value 0]
                            [max-value 100]
                            [init-value *nfood*]
                            [callback (lambda (choice event) (set! *nfood* (send choice get-value)))]))
    (define food-size-slider (new slider% 
                                  [parent option-panel]
                                  [label "Size of Food"]
                                  [min-value 0]
                                  [max-value 100]
                                  [init-value *food-amt*]
                                  [callback (lambda (choice event) (set! *food-amt* (send choice get-value)))]))
    (define drop-slider (new slider% 
                             [parent option-panel]
                             [label "Drop rate"]
                             [min-value 0]
                             [max-value 100]
                             [init-value *drop-amt*]
                             [callback (lambda (choice event) (set! *drop-amt* (send choice get-value)))]))
    (define evap-slider (new slider% 
                             [parent option-panel]
                             [label "Evaporation rate"]
                             [min-value 0]
                             [max-value 100]
                             [init-value *decay-amt*]
                             [callback (lambda (choice event) (set! *decay-amt* (send choice get-value)))]))
    (set! *window* app-window)
    (start-gui app-window)
    (new-run-thread #t)))

(main)
