#lang racket

(struct monad  (wrap bind))

(define (just x) (cons 'just x))

(define none 'none)

(define (maybe-bind mv mf)
  (if (eq? mv none)
      none
      (mf (cdr mv))))


(define maybe-monad (monad just maybe-bind))

(define (f1 x)
  (just (* 2 x)))

(define (f2 x)
  (just (* 3 x)))

(define (g x)
  (maybe-bind 
   (maybe-bind 
    (just x)
    f1)
   f2))

(define (f3 x)
  none)

(define (h x)
  (maybe-bind 
   (maybe-bind 
    (just x)
    f1)
   f3))