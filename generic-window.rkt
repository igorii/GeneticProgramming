#lang racket

(provide start-gui create-window)
(provide (struct-out window))

(require racket/gui/base)
(require (planet williams/animated-canvas/animated-canvas))

(struct window (frame panel canvas) #:transparent)

;; ****************
;; Add GUI elements
;; ****************

(define (create-window alabel fwidth fheight cwidth cheight)
  (let* ([frame      (new frame% [label alabel] [width fwidth] [height fheight])]
         [main-panel (new horizontal-panel%
                          [parent frame]
                          [min-width fwidth]
                          [min-height fheight])]
         [canvas     (instantiate animated-canvas% (main-panel) 
                                  [style '(border)]
                                  [min-width cwidth]
                                  [min-height cheight])])
    ;(send canvas set-canvas-background (make-color 0 0 0))
    (window frame main-panel canvas)))

(define (start-gui awindow)
  (send (window-frame awindow) show #t))
