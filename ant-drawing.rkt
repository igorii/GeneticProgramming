#lang racket

(provide draw-world)

(require "generic-window.rkt")
(require "datatypes.rkt")
(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

;; *******
;; Drawing
;; *******

;; Pens and brushes
(define no-pen      (make-object pen%   "BLACK" 1 'transparent))
(define black-pen   (make-object pen%   "BLACK" 1 'solid))
(define red-pen     (make-object pen%   "RED"   1 'solid))
(define no-brush    (make-object brush% "BLACK"   'transparent))
(define black-brush (make-object brush% "BLACK"   'solid))
(define blue-brush  (make-object brush% "BLUE"   'solid))
(define yellow-brush  (make-object brush% "YELLOW"   'solid))
(define brown-brush  (make-object brush% "BROWN"   'solid))
(define green-brush  (make-object brush% "GREEN"   'solid))
(define red-brush   (make-object brush% "RED"     'solid))

(define (float->color f m) 
  (let ([c (floor (- 255 (* 254 (/ f m))))])
    (make-color c c c)))

(define (draw-cells dc n sz cs m)
  (for ([x (range 0 n)])
       (for ([y (range 0 n)])
            (let ([c (get-cell cs x y)])
              (when (not (and (= 0 (cell-phermn c)) 
                              (null? (cell-food c))))
                (if (not (null? (cell-food c)))
                  (send dc set-brush blue-brush)
                  (send dc set-brush (float->color (cell-phermn c) m) 'solid))
                (send dc draw-rectangle
                      (* sz (pt-x (cell-pt c)))
                      (* sz (pt-y (cell-pt c)))
                      sz sz))))))

(define (draw-home dc sz home)
  (send dc set-brush red-brush)
  (send dc draw-rectangle 
        (* sz (pt-x home)) 
        (* sz (pt-y home))
        sz sz))

(define (draw-ants dc sz as)
  (for ([a as])
       (if (ant-has-food a)
         (send dc set-brush green-brush)
         (send dc set-brush brown-brush))
       (send dc draw-ellipse 
             (+ (/ sz 2) (* sz (pt-x (ant-pt a))))
             (+ (/ sz 2) (* sz (pt-y (ant-pt a))))
             (/ sz 2) (/ sz 2))))

(define (draw-stats dc w i)
  (send dc draw-text (number->string (world-food-at-home w)) 0 0)
  (send dc draw-text (number->string i) 0 20))

(define (draw-world canvas grid world i)
  (let [(dc (send canvas get-dc))]
    (send dc erase)
    (send dc set-pen no-pen)
    (draw-cells dc (grid-ncells grid) (grid-cellsz grid) (world-cells world) (world-max-phermn world))
    (draw-ants dc (grid-cellsz grid) (world-ants world))
    (draw-stats dc world i)
    (draw-home dc (grid-cellsz grid) (world-home world)))
  (send canvas swap-bitmaps))
