#lang racket
(require "util.rkt" "model.rkt" (planet neil/csv:2:0) srfi/13)

(define make-titanic-csv-reader
  (make-csv-reader-maker
   '(
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row (make-titanic-csv-reader 
                  (open-input-file "/Users/fsearwar/downloads/train.csv")))

(next-row) ;; skip headers
(define rows (in-producer next-row '()))


;
;survival        Survival
;                (0 = No; 1 = Yes)
;pclass          Passenger Class
;                (1 = 1st; 2 = 2nd; 3 = 3rd)
;name            Name
;sex             Sex
;age             Age
;sibsp           Number of Siblings/Spouses Aboard
;parch           Number of Parents/Children Aboard
;ticket          Ticket Number
;fare            Passenger Fare
;cabin           Cabin
;embarked        Port of Embarkation
;                (C = Cherbourg; Q = Queenstown; S = Southampton)


(define (make-generic-feature-parser col-index id type transform-fn)
  (lambda (column-vals)
    (feature id type (transform-fn (list-ref column-vals col-index)))))

(define survival (make-generic-feature-parser 0 'survival 'discrete identity))
(define pc-class (make-generic-feature-parser 1 'pc-class 'discrete identity))
(define sex (make-generic-feature-parser 3 'sex 'discrete identity))

;;a function to discretize age values into symbols
(define age
  (make-generic-feature-parser
   4 'age 'discrete 
   (discretize-num-from-string 
    (< 18 '<18)
    (18 35 '18-to-35)
    (else '>35))))

;;discretize counts of sibs or spouses into 'none or 'some
(define sibsp
  (make-generic-feature-parser
   5 'sibsp 'discrete
   (discretize-num-from-string
    (< 1 'none)
    (else 'some))))

;;make embarked feature
(define embarked
  (make-generic-feature-parser
   9 'embarked 'discrete identity))

(define feature-fns (list survival pc-class sex age sibsp embarked))

;;perhaps overkill; I want to isolate the side effect
;;of incrementing the id so this function makes
;;a new instance maker every time, where the new
;;instance maker's first id is always 1
(define (make-instance-maker feature-fns)
  (let ([index 0])
    (lambda (vals)
      (set! index (add1 index))
      (instance index 
                (map (λ (feature-fn)
                             (feature-fn vals))
                     feature-fns)))))

(define instance-maker (make-instance-maker feature-fns))

;(define ages
;  (for/list ([row rows])
;    (string->number (list-ref row 3))))

(define training-instances 
  (for/list ([row rows])
    (instance-maker row)))


