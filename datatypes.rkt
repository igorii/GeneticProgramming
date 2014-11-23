#lang racket

(require racket/match)

(provide (struct-out grid))
;(provide (struct-out food))
(provide (struct-out pt))
(provide (struct-out ant))
(provide (struct-out cell))
(provide (struct-out world))

; ants
(provide random-move gohome drop-phermn!)

; cell
(provide update-cell-food!)

; grid
(provide inc-home-food! place-food! get-cell set-cell! make-cells)

; world
(provide update-world! reset-ants! set-world-ants! blank-world make-world copy-world decay-phermn! adj-phermn-cells adjacent-cells)

;; utils
(provide distance eridiv)


(define (idiv   x y) (exact->inexact (/ x y)))
(define (ridiv  x y) (round (idiv x y)))
(define (eridiv x y) (inexact->exact (ridiv x y)))
(define (distance p1 p2)
  (sqrt (+ (expt (- (pt-x p1) (pt-x p2)) 2)
           (expt (- (pt-y p1) (pt-y p2)) 2))))

;; grid :: ncells : int, cellsz : int, dim : int
(struct grid (ncells cellsz dim))

;; food :: avail : int
;(struct food (avail) #:mutable #:transparent)

;; pt :: x : float, y : float
(struct pt (x y) #:mutable #:transparent)

;; ant :: pt : pt, has-food : bool
(struct ant (pt has-food [steps #:auto]) #:auto-value 0 #:mutable #:transparent)

;; cell :: pt : pt, phermn : int
(struct cell (pt phermn food) #:mutable)

;; world :: home : pt, ants : listof ant, foods : listof food, cells : ncells * ncells vector cell
(struct world (food-at-home home ants cells max-phermn) #:mutable)

(define *grid* (grid 70 6 (* 6 70)))
(define *nfood* 35)
(define *food-amt* 50)
(define *nants* 40)
(define *decay-amt* 5)
(define *drop-amt* 5)
(define *max-amt* 255)
(define *steps* 200)
(define *homept* (/ (grid-ncells *grid*) 2))

(provide *grid* *nfood* *food-amt* *nants* *decay-amt* *drop-amt* *max-amt* *steps* *homept*)
(provide set-nants! set-nfood! set-food-amt! set-drop-amt! set-decay-amt!)

(define (set-nants! nants)         (set! *nants* nants))
(define (set-nfood!  nfood)         (set! *nfood* nfood))
(define (set-food-amt!  food-amt)  (set! *food-amt* food-amt))
(define (set-drop-amt!  drop-amt)  (set! *drop-amt* drop-amt))
(define (set-decay-amt! decay-amt) (set! *decay-amt* decay-amt))


(define (update-world! g w decay-amt update-fn!)
  (decay-phermn! (world-cells w) (grid-ncells g) decay-amt)
  (for ([a (world-ants w)])
       (update-fn! a g w))
  w)

(define (reset-ants! w number)
  (set-world-ants! w (map (lambda (x) (ant (world-home w) #f)) (range 0 number))))

(define (copy-world w)
  (world (world-food-at-home w)
         (world-home w)
         (map (lambda (x) (ant (ant-pt x) (ant-has-food x))) (world-ants w))
         (list->vector (map (lambda (x) (list->vector (map (lambda (y) (struct-copy cell y)) (vector->list x))))
              (vector->list (world-cells w))))
         (world-max-phermn w)))

(define (blank-world nants homept ncells max-phermn)
  (world 0
         (pt homept homept) 
         (map (lambda (_) (ant (pt homept homept) #f))
              (range 0 nants)) 
         (make-cells ncells)
         max-phermn))

(define (make-world)
  (let ([w (blank-world *nants* *homept* (grid-ncells *grid*) *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    w))

;; *************************
;;       World/Grid
;; *************************

(define (decay-phermn! matrix ncells decay-amt)
  (for ([i (range 0 ncells)])
       (for ([j (range 0 ncells)])
            (let ([c (get-cell matrix i j)])
              (when (not (= 0 (cell-phermn c)))
                (set-cell-phermn! c (max 0 (- (cell-phermn c) decay-amt))))))))


(define (inc-home-food! w) (set-world-food-at-home! w (add1 (world-food-at-home w))))
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
              (set-cell! v1 i j (cell (pt i j) 0 null)))) 
    v1))

(define (place-food! num amt ncells matrix)
  (map (lambda (x) 
         (let ([x (inexact->exact (floor (* ncells (random))))]
               [y (inexact->exact (floor (* ncells (random))))])
           (update-cell-food! matrix x y amt)))
       (range 0 num)))

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

(define (drop-phermn! a g w drop-amt)
  (let* ([currcell (get-cell (world-cells w) (pt-x (ant-pt a)) (pt-y (ant-pt a)))]
         [cs (adjacent-cells (ant-pt a) (world-cells w) (grid-ncells g))]
         [max-amt (world-max-phermn w)])
    (set-cell-phermn! currcell (min max-amt (+ drop-amt (cell-phermn currcell))))
    (for ([c cs])
         (set-cell-phermn! c (min max-amt (+ (eridiv drop-amt 2) (cell-phermn c))))
         (let ([cs (adjacent-cells (ant-pt a) (world-cells w) (grid-ncells g))])
           (for ([c cs])
                (set-cell-phermn! c (min max-amt (+ (eridiv drop-amt 4) (cell-phermn c)))))))))

(define (gohome p home)
  (define (normal x) 
    (if (= 0 x) x (/ x (abs x))))
  (let ([x (- (pt-x p) (pt-x home))]
        [y (- (pt-y p) (pt-y home))])
    ;(if (< x y)
    (pt (- (pt-x p) (normal x)) (- (pt-y p) (normal y)))))
;(pt (pt-x p) (- (pt-y p) (normal y))))))

