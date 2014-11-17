#lang racket

(require "generic-window.rkt")
(require "ant-drawing.rkt")
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

(define (idiv   x y) (exact->inexact (/ x y)))
(define (ridiv  x y) (round (idiv x y)))
(define (eridiv x y) (inexact->exact (ridiv x y)))

(define (drop-phermn! a g w)
  (let* ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
         [cs (adjacent-cells (ant-pt a) (world-cells w) (grid-ncells g))]
         [max-amt (world-max-phermn w)])
    (set-cell-phermn! currcell (min max-amt (+ *drop-amt* (cell-phermn currcell))))
    (for ([c cs])
         (set-cell-phermn! c (min max-amt (+ (eridiv *drop-amt* 2) (cell-phermn c))))
         (let ([cs (adjacent-cells (ant-pt a) (world-cells w) (grid-ncells g))])
           (for ([c cs])
                (set-cell-phermn! c (min max-amt (+ (eridiv *drop-amt* 4) (cell-phermn c)))))))))

(define (update-ant! a g w)
  ;; HF = has food, SF = sense food, SP = sense phermn, AH = at home
  (let ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
        [adj-phermn-cells (adj-phermn-cells (ant-pt a) (world-cells w) (grid-ncells g))])
    (cond 
      [(and (ant-has-food a) (equal? (ant-pt a) (world-home w)))        ; If HF and AH, drop food and move away
       (inc-home-food! w)
       (set-ant-has-food! a #f)]
      [(and (ant-has-food a) (not (equal? (ant-pt a) (world-home w))))  ; Else if HF and ~AH, drop phermn and move away
       (drop-phermn! a g w)
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

(define (distance p1 p2)
  (sqrt (+ (expt (- (pt-x p1) (pt-x p2)) 2)
           (expt (- (pt-y p1) (pt-y p2)) 2))))

(define (adjacent-cells p cells ncells)
  (let ([l '()])
    (for ([x (range (sub1 (pt-x p)) (add1 (add1 (pt-x p))))])
         (for ([y (range (sub1 (pt-y p)) (add1 (add1 (pt-y p))))])
              (set! l (cons (get-cell cells 
                                      (modulo x ncells) 
                                      (modulo y ncells))
                            l))))
    l))

(define (adj-phermn-cells p cells ncells)
  (let ([adjcells (adjacent-cells p cells ncells)])
    (filter (lambda (x) (not (= 0 (cell-phermn x)))) adjcells)))

(define (decay-phermn! matrix ncells)
  (for ([i (range 0 ncells)])
       (for ([j (range 0 ncells)])
            (let ([c (get-cell matrix i j)])
              (when (not (= 0 (cell-phermn c)))
                (set-cell-phermn! c (max 0 (- (cell-phermn c) *decay-amt*))))))))

(define (update-world g w)
  (decay-phermn! (world-cells w) (grid-ncells g))
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
         [w (world 0
                   (pt homept homept) 
                   (map (lambda (_) (ant (pt homept homept) #f)) 
                        (range 0 *nants*)) 
                   null
                   (make-cells (grid-ncells *grid*))
                   *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    ;(displayln (adjacent-cells (pt 5 5) (world-cells w) (grid-ncells *grid*)))))
    (main-loop *grid* w)))

(define (main)
  (let ([app-window (create-window "Ant Colony" (grid-dim *grid*) (grid-dim *grid*))])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-colony)))

(main)
