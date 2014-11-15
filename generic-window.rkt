#lang racket

(provide start-gui create-window)
(provide (struct-out window))

(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(struct window (frame panel canvas) #:transparent)

;; ****************
;; Add GUI elements
;; ****************

(define (create-window alabel awidth aheight)
  (printf "Creating window of size (~a, ~a)\n" awidth aheight)
  (let* ([frame      (new frame% [label alabel] [width awidth] [height aheight])]
         [main-panel (new horizontal-panel%
                          [parent frame]
                          [min-width awidth]
                          [min-height aheight])]
         [canvas     (instantiate animated-canvas% (main-panel) 
                                  [style '(border)]
                                  [min-width awidth]
                                  [min-height aheight])])
    ;(send canvas set-canvas-background (make-color 0 0 0))
    (window frame main-panel canvas)))

(define (start-gui awindow)
  (send (window-frame awindow) show #t))
