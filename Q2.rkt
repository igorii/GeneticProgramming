#lang racket

(require racket/match)

(provide ramped-half-and-half)

;; Restricted functions
(define (r-add a b) (+ a b))
(define (r-sub a b) (- a b))
(define (r-mul a b) (* a b))
(define (r-div a b) (if (= b 0) 0 (/ a b)))
(define (r-sin a)   (sin a))
(define (r-cos a)   (cos a))
(define (r-exp a b) (if (and (= a 0) (< b 0)) 0 (expt a b)))

(struct fn (sym arity proc))
(define *func-table* (list (fn 'add 2 r-add)
                           (fn 'sub 2 r-sub)
                           (fn 'mul 2 r-mul)
                           (fn 'div 2 r-div)
                           (fn 'sin 1 r-sin)
                           (fn 'cos 2 r-cos)
                           (fn 'exp 2 r-exp)))
(define *terminals* (list 'x 'R))
(define *pop-size* 1000)
(define *success* 20)
(define *precision* 0.001)
(define *max-generations* 51)
(define *%crossover* 0.9)
(define *%mutation* 0.01)
(define *%reproduction* 0.09)
(define *%crossover-point* 0.9)
(define *max-init-program-size* 6)
(define *max-run-program-size* 17)
(define *tourny-size* 7)
(define *elitism* #t)

(define (random-fn ftable)  (car (shuffle ftable)))
(define (random-term terms) (car (shuffle terms)))
(define (random-item ftable terms) (car (shuffle (append ftable terms))))

(struct leaf (x) #:transparent)
(struct branch1 (n a1) #:transparent)
(struct branch2 (n a1 a2) #:transparent)

;; Ramped Half-and-Half
;; ====================
(define (ramped-half-and-half popsize maxheight ftable termlist)
  (define (full level maxdepth)
    (if (= level maxdepth)
      (leaf (random-term termlist))
      (let ([f (random-fn ftable)])
        (match f [(fn sym 1 _) (branch1 sym (full (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (full (add1 level) maxdepth)
                                      (full (add1 level) maxdepth))]))))

  (define (grow level maxdepth)
    (if (= level maxdepth)
      (leaf (random-term termlist))
      (let ([e1 (random-item ftable termlist)])
        (match e1 [(fn sym 1 _) (branch1 sym (grow (add1 level) maxdepth))]
               [(fn sym 2 _) (branch2 sym (grow (add1 level) maxdepth)
                                      (grow (add1 level) maxdepth))]
               [t1 (leaf t1)]))))


  (let ([count (/ (/ popsize (- (add1 maxheight) 2)) 2)])
    (apply append (map (lambda (maxdepth)
                         (append (map (lambda (x) (full 0 maxdepth)) (range 0 count))
                                 (map (lambda (x) (grow 0 maxdepth)) (range 0 count))))
                       (range 2 (add1 maxheight))))))

;; GP Algorithm
;; ============
;; Generate initial population of random programs
;; Repeat until termination condition
;;     Execute each program and assign it a fitness
;;     Create a new population by applying the following operations to programs selected with fitness based on probability:
;;         Reproduce a progam by copying it into the new generation
;;         create 2 new programs by crossover
;;     Designate best program so far

(define (main)
  (length
  (ramped-half-and-half *pop-size* *max-init-program-size* *func-table* *terminals*)))

(main)
