#lang racket

(require racket/gui/base)
(require "generic-window.rkt")
(require "ant-drawing.rkt")
(require "datatypes.rkt")

;; ****************
;;     Params
;; ****************

(define *window*         null)
(define *gui-thread*     null)
(define *gui-pause*      #t)
(define *current-world*  null)
(define *init-world*     null)
(define *next-timestamp* (current-milliseconds))
(define *fps*            (quotient 1000 40))

;; ****************
;;   World Update
;; ****************

(define (update-ant! with-phermn)
  ;; a : ant, g : grid, w : world
  (lambda (a g w)
    ;; HF = has food, SF = sense food, SP = sense phermn, AH = at home
    (let ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
          [adj-phermn-cells (adj-phermn-cells (ant-pt a) (world-cells w) (grid-ncells g))])
      (cond
        ;; If HF and AH, drop food and move away
        [(and (ant-has-food a) (equal? (ant-pt a) (world-home w)))
         (inc-home-food! w)
         (set-ant-has-food! a #f)]
        ;; If HF and ~AH, drop phermn and move away
        [(and (ant-has-food a) (not (equal? (ant-pt a) (world-home w))))
         (when with-phermn (drop-phermn! a g w *drop-amt*))
         (set-ant-pt! a (gohome (ant-pt a) (world-home w)))]
        ;; Else if ~HF and SF, pick up food and go home
        [(and (not (ant-has-food a)) (not (null? (cell-food currcell))))
         (if (>= 1 (cell-food currcell))
           (set-cell-food! currcell null)
           (set-cell-food! currcell (- (cell-food currcell) 1)))
         (set-ant-has-food! a #t)]
        ;; Else if ~HF and SP, move to next position along trail (away from home)
        [(and (not (ant-has-food a)) (not (null? adj-phermn-cells)))
         (let ([farther-pts (filter (lambda (x) (> (distance (cell-pt x) (world-home w))
                                                   (distance (ant-pt a)  (world-home w))))
                                    adj-phermn-cells)])
           (if (null? farther-pts)
             (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g)))
             (set-ant-pt! a (cell-pt (argmax (lambda (x) (cell-phermn x)) farther-pts)))))]
        ;; Else move random
        [else
          (set-ant-pt! a (random-move (ant-pt a) (grid-ncells g)))]))))

;; *****************
;;     Helpers
;; *****************

;; Run a simulation with the use of phermn specified by `with-phermn`
(define (main-loop with-phermn grid w)
  (when (> (current-milliseconds) *next-timestamp*)
    (draw-world (window-canvas *window*) grid *current-world* -1)
    (set! *next-timestamp* (+ *fps* (current-milliseconds)))
    (when (not *gui-pause*)
      (set! *current-world* (update-world! grid *current-world* *decay-amt* (update-ant! with-phermn)))))
  (main-loop with-phermn grid w))

;; Run a simulation with a specified number of iterations, and the use
;; of phermn specified by `with-phermn`
(define (main-loop-with-iters with-phermn iters grid w)
  (if (<= iters 0)
    (world-food-at-home w)
    (begin
      (set! *current-world* (update-world! grid *current-world* *decay-amt* (update-ant! with-phermn)))
      (main-loop-with-iters with-phermn (sub1 iters) grid w))))

;; Start a colony simulation
(define (start-colony)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (blank-world *nants* homept (grid-ncells *grid*) *max-amt*)])
    (set! *current-world* (copy-world w))
    (main-loop #t *grid* *current-world*)))

;; Start a colony comparison operation
(define (start-colony-comparison)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (blank-world *nants* homept (grid-ncells *grid*) *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    (for ([nants (list 10 20 30 40 50 60 70 80 90)])
         (for ([drop-amt (list 3 5 7 9 11)])
              (for ([decay-amt (list 3 5 7 9 11)])
                   (for ([use-phermn (list #t #f)])
                        (set! *current-world* (copy-world w))
                        (set-drop-amt! drop-amt)
                        (set-decay-amt! decay-amt)
                        (reset-ants! *current-world* nants)
                        (let ([rlt (main-loop-with-iters use-phermn 500 *grid* *current-world*)])
                          (printf "fit ~a | relfit ~a | nants: ~a | use-phermn ~a | drop-amt ~a | decay-amt ~a\n" 
                                  rlt (exact->inexact (/ rlt nants)) nants use-phermn drop-amt decay-amt))))))))

;; Create a new world with initialized food
(define (new-world-with-food)
  (let* ([homept (/ (grid-ncells *grid*) 2)]
         [w (blank-world *nants* homept (grid-ncells *grid*) *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    w))

;; Create a new simulation on the simulation thread
(define (new-run-thread paused)
  (if (thread? *gui-thread*) (kill-thread *gui-thread*) null)
  (set! *gui-pause* paused)
  (set! *gui-thread* (thread start-colony)))

;; MAIN
(define (main)
  (let* ([app-window (create-window 
                       "Ant Colony" 700 (grid-dim *grid*) (grid-dim *grid*) (grid-dim *grid*)
                       (lambda (e)
                         (when (eq? 'left-down (send e get-event-type))
                           (let* ([cellsz (grid-cellsz *grid*)]
                                  [ncells (grid-ncells *grid*)]
                                  [x (inexact->exact (floor (/ (send e get-x) cellsz)))]
                                  [y (inexact->exact (floor (/ (send e get-y) cellsz)))])
                             (when (and (< x ncells) (< y ncells))
                               (update-cell-food! (world-cells *current-world*) x y *food-amt*)
                               (set! *init-world* (copy-world *current-world*)))))))]
         [option-panel (new vertical-panel% [parent (window-panel app-window)])])

  ;; *****************
  ;;       GUI
  ;; *****************

    (define pause-btn (new button%
                           [parent option-panel]
                           [label "Start/Stop"]
                           [callback (lambda (button event) 
                                       (set! *gui-pause* (not *gui-pause*)))]))
    (define restart (new button% 
                         [parent option-panel]
                         [label "Restart"]
                         [callback (lambda (button event)
                                     (when (not (null? *init-world*))
                                       (set! *current-world* (copy-world *init-world*))
                                       (reset-ants! *current-world* *nants*)
                                       (set! *gui-pause* #f)))]))
    (define randomize (new button% 
                           [parent option-panel]
                           [label "Randomize"]
                           [callback (lambda (button event) 
                                       (set! *current-world* (new-world-with-food))
                                       (set! *init-world* (copy-world *current-world*)))]))
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
                            [callback (lambda (choice event) 
                                        (set-nants! (send choice get-value)))]))
    (define nfood-slider (new slider% 
                              [parent option-panel]
                              [label "# Random Food"]
                              [min-value 0]
                              [max-value 100]
                              [init-value *nfood*]
                              [callback (lambda (choice event) 
                                          (set-nfood! (send choice get-value)))]))
    (define food-size-slider (new slider% 
                                  [parent option-panel]
                                  [label "Size of Food"]
                                  [min-value 0]
                                  [max-value 200]
                                  [init-value *food-amt*]
                                  [callback (lambda (choice event) 
                                              (set-food-amt! (send choice get-value)))]))
    (define drop-slider (new slider% 
                             [parent option-panel]
                             [label "Drop rate"]
                             [min-value 0]
                             [max-value 100]
                             [init-value *drop-amt*]
                             [callback (lambda (choice event) 
                                         (set-drop-amt! (send choice get-value)))]))
    (define evap-slider (new slider% 
                             [parent option-panel]
                             [label "Evaporation rate"]
                             [min-value 0]
                             [max-value 100]
                             [init-value *decay-amt*]
                             [callback (lambda (choice event) 
                                         (set-decay-amt! (send choice get-value)))]))
    (set! *window* app-window)
    (start-gui app-window)
    (new-run-thread #t)))

(main)
