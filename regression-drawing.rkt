#lang racket

(require plot)
;(plot-new-window? #t)
(require "generic-window.rkt")
(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(provide update-canvas)

(define black-pen   (make-object pen%   "BLACK" 1 'solid))
(define black-brush (make-object brush% "BLACK"   'solid))

(define (rounderror error)
  (/ (round (* error 100)) 100))

(define (create-label error)
  (string-append "Regression (" (number->string (rounderror error)) ")"))

(define (update-canvas canvas fn target xmin xmax w h error)
  (let [(dc (send canvas get-dc))]
    (send dc erase)
    (plot/dc (list (function fn xmin xmax #:color "RED" #:label (create-label error))
                   (function target xmin xmax #:color "BLUE" #:label "Target"))
             dc 0 0 w h))
  (send canvas swap-bitmaps))
