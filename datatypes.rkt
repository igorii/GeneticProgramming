#lang racket

(require racket/match)

(provide (struct-out grid))
(provide (struct-out food))
(provide (struct-out pt))
(provide (struct-out ant))
(provide (struct-out cell))
(provide (struct-out world))
(provide random-move)
(provide place-food! get-cell set-cell! make-cells)

;; grid :: ncells : int, cellsz : int, dim : int
(struct grid (ncells cellsz dim))

;; food :: avail : int
(struct food (avail) #:mutable)

;; pt :: x : float, y : float
(struct pt (x y) #:mutable)

;; ant :: pt : pt, has-food : bool
(struct ant (pt has-food) #:mutable)

;; cell :: pt : pt, phermn : int
(struct cell (pt phermn food) #:mutable)

;; world :: home : pt, ants : listof ant, foods : listof food, cells : ncells * ncells vector cell
(struct world (home ants foods cells))
(define (get-cell matrix x y)
  (vector-ref (vector-ref matrix y) x))
(define (set-cell! matrix x y new)
  (vector-set! (vector-ref matrix y) x new))
(define (update-cell-phermn matrix x y new)
  (set-cell-phermn! (get-cell matrix x y) new))
(define (update-cell-food! matrix x y new)
  (set-cell-food! (get-cell matrix x y) new))
(define (make-cells n)
  (let ([v1 (make-vector n null)])
    (for ([i (range 0 n)])
         (vector-set! v1 i (make-vector n null)))
    (for ([i (range 0 n)])
         (for ([j (range 0 n)])
              (set-cell! v1 i j 
                         (cell (pt i j) 
                               ;             (inexact->exact (floor (* 255 (random))))
                               0
                               null))))
    v1))

(define (place-food! num amt ncells matrix)
  (map (lambda (x) 
         (let ([x (inexact->exact (floor (* ncells (random))))]
               [y (inexact->exact (floor (* ncells (random))))])
           (update-cell-food! matrix x y amt)))
       (range 0 num)))

;; *************************
;;           Ant
;; *************************
(define adjacent-dirs (vector 'up 'down 'left 'right 'upleft 'upright 'downleft 'downright))
(define (random-direction) 
  (vector-ref adjacent-dirs 
              (inexact->exact (floor (* (vector-length adjacent-dirs) (random))))))
  (define (move-left!  pt ncells) (set-pt-x! pt (modulo (- (pt-x pt) 1) ncells)))
  (define (move-right! pt ncells) (set-pt-x! pt (modulo (+ (pt-x pt) 1) ncells)))
  (define (move-up!    pt ncells) (set-pt-y! pt (modulo (+ (pt-y pt) 1) ncells)))
  (define (move-down!  pt ncells) (set-pt-y! pt (modulo (- (pt-y pt) 1) ncells)))
  (define (move p dir ncells)
    (let ([pt (struct-copy pt p)])
      (match dir
             ['left  (move-left!  pt ncells)]
             ['right (move-right! pt ncells)]
             ['up    (move-up!    pt ncells)]
             ['down  (move-down!  pt ncells)]
             ['upleft    (begin (move-up! pt ncells)   (move-left! pt ncells))]
             ['upright   (begin (move-up! pt ncells)   (move-right! pt ncells))]
             ['downleft  (begin (move-down! pt ncells) (move-left! pt ncells))]
             ['downright (begin (move-down! pt ncells) (move-right! pt ncells))])
      pt))
  (define (random-move pt ncells)
    (move pt (random-direction) ncells))
