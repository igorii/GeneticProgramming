#lang racket

(require racket/pretty)
(require racket/gui/base)
(require racket/match)
(require (prefix-in gp: "generic-gp.rkt"))
(require "generic-window.rkt")
(require "regression-drawing.rkt")

;; Restricted functions
(define (r-add a b) (+ a b))
(define (r-sub a b) (- a b))
(define (r-mul a b) (* a b))
(define (r-div a b) (if (= b 0) 0 (/ a b)))
(define (r-sin a)   (sin a))
(define (r-cos a)   (cos a))
(define (r-exp a b)
  (if (and (= a 0) (< b 0)) 0
    (let ([t (expt a b)])
      (if (not (complex? t)) t 1))))

;; Target Functions
;(define *target* (lambda (x) (+ 1 (+ x (+ (expt (* 2 x) 2) (expt (* 3 x) 3))))))
(define *target* (lambda (x) (+ (cos x) (* 3 (sin (expt x 2))))))

(define *pop-size* 1000)
(define *success* 20)
(define *precision* 0.001)
(define *max-generations* 51)
(define *%mutation* 0.05)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *nfitcases* 100)

(define *terminals*  (list 'x 'R))
(define *func-table* (list (gp:fn 'add 2 r-add)
                           (gp:fn 'sub 2 r-sub)
                           (gp:fn 'mul 2 r-mul)
                           (gp:fn 'div 2 r-div)
                           (gp:fn 'sin 1 r-sin)
                           (gp:fn 'cos 1 r-cos)
                           (gp:fn 'exp 2 r-exp)))

(struct fitcase (input output) #:transparent)
(define (create-fitness-cases fn minp maxp n)
  (map (lambda (x) (fitcase x (fn x)))
       (map (lambda (x) (+ minp (* (- maxp minp) (/ x n))))
            (range 0 n))))

(define (calc-fit-diffs fitcases symtree ftable)
  (map (lambda (x)
         (let ([rslt (gp:eval-symtree symtree (fitcase-input x) ftable)])
           (fitcase (fitcase-input x) 
                    (sqr (- (fitcase-output x) rslt)))))
       fitcases))

(define (calc-fitness-from-diffs fitdiffs nfitcases successes precision)
  ;; If we've passed the correct number of cases, just return 0, otherwise
  ;; get the normalized difference
  (let ([npassed (length (filter (lambda (x) (< x precision)) (map fitcase-output fitdiffs)))])
    (if (<= successes npassed)
      0
      (let ([sum (foldl + 0 (map (lambda (x) (fitcase-output x)) fitdiffs))])
        (sqrt (/ sum nfitcases))))))

(define (make-fitness-fn fn ftable xmin xmax nfitcases successes precision)
  (let* ([fitcases (create-fitness-cases fn xmin xmax nfitcases)])
    (lambda (program)
      (calc-fitness-from-diffs (calc-fit-diffs fitcases program ftable) nfitcases successes precision))))

(define eval-form
  (let ((ns (make-base-namespace)))
    (lambda (form) 
      (eval `(lambda (x) ,form) ns))))

(define (make-callback target xmin xmax height width ftable)
  (lambda (fit individual iter)
    (displayln "--------------------------------------------------------------------------------")
    (displayln fit)
    (pretty-print (gp:symtree->source individual))
    (update-canvas (window-canvas *window*)
                   (eval-form (gp:symtree->proc individual ftable))
                   target
                   xmin xmax height width fit)))

(define (start-regression)
  (gp:generic-gp
    #:population-size *pop-size*
    #:max-init-tree-height *max-init-program-size*
    #:max-run-tree-height *max-run-program-size*
    #:function-table *func-table*
    #:terminals *terminals*
    #:mutation-rate *%mutation*
    #:tournament-size *tourny-size*
    #:fitness-fn (make-fitness-fn *target* *func-table* -5 5 *nfitcases* *success* *precision*)
    #:callback (make-callback *target* -5 5 600 600 *func-table*)))

(define *window* null)
(define (main)
  (let ([app-window (create-window "Symbolic Regression" 600 600 600 600 (lambda (e) null))])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-regression)))

(main)
