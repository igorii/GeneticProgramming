#lang racket

(require racket/date)
(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "ant-drawing.rkt")
(require "datatypes.rkt")
(require "ant-functions.rkt")

(define file 
  (command-line #:args file (car file)))

(define (main) 
  (present-program (gp:file->program file) *steps* draw-world))

(main)
