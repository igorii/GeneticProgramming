#lang racket

(require racket/match)

;; **********************
;;       Provides
;; **********************

(provide (struct-out grid))
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

; utils
(provide distance eridiv)

; params
(provide *grid* *nfood* *food-amt* *nants* *decay-amt* *drop-amt* *max-amt* *steps* *homept*)
(provide set-max-amt! set-nants! set-nfood! set-food-amt! set-drop-amt! set-decay-amt!)

;; **********************
;;       Structs
;; **********************

;; grid :: ncells : int, cellsz : int, dim : int
(struct grid (ncells cellsz dim))

;; pt :: x : float, y : float
(struct pt (x y) #:mutable #:transparent)

;; ant :: pt : pt, has-food : bool
(struct ant (pt has-food [steps #:auto]) #:auto-value 0 #:mutable #:transparent)

;; cell :: pt : pt, phermn : int
(struct cell (pt phermn food) #:mutable)

;; world :: home : pt, ants : listof ant, foods : listof food, cells : ncells * ncells vector cell
(struct world (food-at-home home ants cells max-phermn) #:mutable)

;; **********************
;;       Params
;; **********************

(define *grid* (grid 70 6 (* 6 70)))
(define *nfood* 35)
(define *food-amt* 50)
(define *nants* 40)
(define *decay-amt* 5)
(define *drop-amt* 5)
(define *max-amt* 255)
(define *steps* 200)
(define *homept* (/ (grid-ncells *grid*) 2))

;; **********************
;;    Param Mutation
;; **********************

(define (set-nants! nants)         (set! *nants* nants))
(define (set-max-amt! max-amt)     (set! *max-amt* max-amt))
(define (set-nfood!  nfood)        (set! *nfood* nfood))
(define (set-food-amt!  food-amt)  (set! *food-amt* food-amt))
(define (set-drop-amt!  drop-amt)  (set! *drop-amt* drop-amt))
(define (set-decay-amt! decay-amt) (set! *decay-amt* decay-amt))

;; *************************
;;       World/Grid
;; *************************

;; Update a world with the given ant update function and phermn decay rate
(define (update-world! g w decay-amt update-fn!)
  (decay-phermn! (world-cells w) (grid-ncells g) decay-amt)
  (for ([a (world-ants w)])
       (update-fn! a g w))
  w)

;; Reset the ants in a world
(define (reset-ants! w number)
  (set-world-ants! w (map (lambda (x) (ant (world-home w) #f)) (range 0 number))))

;; Copy a world to avoid mutation
(define (copy-world w)
  (world (world-food-at-home w)
         (world-home w)
         (map (lambda (x) (ant (ant-pt x) (ant-has-food x))) (world-ants w))
         (list->vector (map (lambda (x) (list->vector (map (lambda (y) (struct-copy cell y)) (vector->list x))))
              (vector->list (world-cells w))))
         (world-max-phermn w)))

;; Create a new world with no food
(define (blank-world nants homept ncells max-phermn)
  (world 0 (pt homept homept)
         (map (lambda (_) (ant (pt homept homept) #f))
              (range 0 nants))
         (make-cells ncells)
         max-phermn))

;; Make a new world with food
(define (make-world)
  (let ([w (blank-world *nants* *homept* (grid-ncells *grid*) *max-amt*)])
    (place-food! *nfood* *food-amt* (grid-ncells *grid*) (world-cells w))
    w))

;; Decay the phermn by the given amount in a world
(define (decay-phermn! matrix ncells decay-amt)
  (for ([i (range 0 ncells)])
       (for ([j (range 0 ncells)])
            (let ([c (get-cell matrix i j)])
              (when (not (= 0 (cell-phermn c)))
                (set-cell-phermn! c (max 0 (- (cell-phermn c) decay-amt))))))))

;; Increment the amount of food stored at a nest in a world
(define (inc-home-food! w) 
  (set-world-food-at-home! w (add1 (world-food-at-home w))))

;; Retrieve the world cell specified by the given coordinate
(define (get-cell matrix x y)
  (vector-ref (vector-ref matrix y) x))

;; Set a world cell at a given coordinate
(define (set-cell! matrix x y new)
  (vector-set! (vector-ref matrix y) x new))

;; Update a world cell's phermn at a given coordinate
(define (update-cell-phermn matrix x y new)
  (set-cell-phermn! (get-cell matrix x y) new))

;; Update a world cell's food at a given coordinate
(define (update-cell-food! matrix x y new)
  (set-cell-food! (get-cell matrix x y) new))

;; Make a world grid of a cells as a 2D vector of a given
;; (square) dimension
(define (make-cells n)
  (let ([v1 (make-vector n null)])
    (for ([i (range 0 n)])
         (vector-set! v1 i (make-vector n null)))
    (for ([i (range 0 n)])
         (for ([j (range 0 n)])
              (set-cell! v1 i j (cell (pt i j) 0 null)))) 
    v1))

;; Place the specified number of food randomly throughout a world grid
(define (place-food! num amt ncells matrix)
  (map (lambda (x) 
         (let ([x (inexact->exact (floor (* ncells (random))))]
               [y (inexact->exact (floor (* ncells (random))))])
           (update-cell-food! matrix x y amt)))
       (range 0 num)))

;; Retrieve the adjacent (including diagonals) cells to a given coordinate
(define (adjacent-cells p cells ncells)
  (let ([l '()])
    (for ([x (range (sub1 (pt-x p)) (add1 (add1 (pt-x p))))])
         (for ([y (range (sub1 (pt-y p)) (add1 (add1 (pt-y p))))])
              (set! l (cons (get-cell cells 
                                      (modulo x ncells) 
                                      (modulo y ncells))
                            l))))
    l))

;; Retrieve the adjacent (including diagonals) cells that have some phermn
;; to a given coordinate
(define (adj-phermn-cells p cells ncells)
  (let ([adjcells (adjacent-cells p cells ncells)])
    (filter (lambda (x) (not (= 0 (cell-phermn x)))) adjcells)))

;; *************************
;;           Ant
;; *************************

;; Direction helpers
(define adjacent-dirs (vector 'up 'down 'left 'right 'upleft 'upright 'downleft 'downright))
(define (random-direction) 
  (vector-ref adjacent-dirs 
              (inexact->exact (floor (* (vector-length adjacent-dirs) (random))))))

;; Movement functions
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

;; Return a position that would be the result of a move in a random direction
(define (random-move pt ncells)
  (move pt (random-direction) ncells))

;; Update a world dby dropping phermn at the current location of an ant
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

;; Move in the direction of the nest
(define (gohome p home)
  (define (normal x) 
    (if (= 0 x) x (/ x (abs x))))
  (let ([x (- (pt-x p) (pt-x home))]
        [y (- (pt-y p) (pt-y home))])
    (pt (- (pt-x p) (normal x)) (- (pt-y p) (normal y)))))

;; Utils
(define (idiv   x y) (exact->inexact (/ x y)))
(define (ridiv  x y) (round (idiv x y)))
(define (eridiv x y) (inexact->exact (ridiv x y)))
(define (distance p1 p2)
  (sqrt (+ (expt (- (pt-x p1) (pt-x p2)) 2)
           (expt (- (pt-y p1) (pt-y p2)) 2))))
