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

(define *pop-size* 500)
(define *success* 20)
(define *precision* 0.001)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.05)
(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *elitism* #t)
(define *terminals* (list 'x 'R))
(define *nfitcases* 100)

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
            (range 0 (+ n 1)))))

(define (calc-fit-diffs fitcases symtree ftable)
  (map (lambda (x)
         (let ([rslt (gp:eval-symtree symtree (fitcase-input x) ftable)])
           (fitcase (fitcase-input x) 
                    (sqr (- (fitcase-output x) rslt)))))
       fitcases))

(define (calc-fitness-from-diffs fitdiffs nfitcases)
  (let ([sum (foldl + 0 (map (lambda (x) (fitcase-output x)) fitdiffs))])
    (sqrt (/ sum nfitcases))))

(define (make-fitness-fn fn ftable xmin xmax nfitcases)
  (let* ([fitcases (create-fitness-cases fn xmin xmax nfitcases)])
    (lambda (program)
      (calc-fitness-from-diffs (calc-fit-diffs fitcases program ftable) nfitcases))))

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
  (let* (;[fn (lambda (x) (+ 1 (+ x (+ (expt (* 2 x) 2) (expt (* 3 x) 3)))))])
         [fn (lambda (x) (+ (cos x) (* 3 (sin (expt x 2)))))])
    (gp:generic-gp
      #:population-size *pop-size*
      #:max-init-tree-height *max-init-program-size*
      #:max-run-tree-height *max-run-program-size*
      #:function-table *func-table*
      #:terminals *terminals*
      #:mutation-rate *%mutation*
      #:tournament-size *tourny-size*
      #:fitness-fn (make-fitness-fn fn *func-table* -5 5 *nfitcases*)
      #:callback (make-callback fn -5 5 600 600 *func-table*))))

(define *window* null)
(define (main)
  (let ([app-window (create-window "Symbolic Regression" 600 600 600 600 (lambda (e) null))])
    (set! *window* app-window)
    (start-gui app-window)
    (thread start-regression)))

(main)
