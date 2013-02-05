#lang racket/base

(require racket/list racket/match "model.rkt" "util.rkt")

(provide (all-defined-out))

(define (log10 n) (/ (log n) (log 10)))

(define (log2 n) (/ (log n) (log 2)))

(define (sum nums)
  (foldl + 0 nums))

;; ListOf Number -> Number
;;turns out the following implementation is slower than 
;;uncommented one; fractions are slow...who knew?
;(define (entropy nums)
;  (let ([total (sum nums)])
;    (* -1 
;       (for/sum ([x nums])
;                (* (/ x total) (log2 (/ x total)))))))
                
;; ListOf Number -> Number
;; Calculates the information value (or entropy) for a list of numbers
;; (entrop 2 3) => 0.971
(define (entropy nums)
  (let* ([total (sum nums)]
         [final-term (* total (log2 total))] )
    (/ (+ final-term
          (for/sum ([x nums])
                   (* -1 x (log2 x))))
       total)))


;;---------------------------------------------------------------------------------------------------




;(feature 'age 'discrete 23)

;; Symbol ListOf instance -> ListOf (PairOf 'symbol number)
;; Finds all the features in the list of instances identified
;; by feat-id.  Features are then grouped by value.  The result
;; is a list of pairs where the first element in the pair is a
;; feature value and the second element is the number of instances
;; with that value.
(define (feature-value-counts feat-id instances)
  (let ([grouped-features (group-by-feature-value feat-id instances)] )
    (map (λ (grouped-pair)
           (list (first grouped-pair)
                 (length (second grouped-pair))))
         grouped-features)))



;; Calculates information value for a set of instances
;; using the values of the specified feature.
;; Symbol Listof instance -> Number 
(define (info-value feat-id instances)
  (entropy 
   (map (λ (value+count)
          (second value+count))
        (feature-value-counts feat-id instances))) )

;; Symbol Symbol Listof instance -> Number 
;; Calculates the info gain of using feature identified by split-feat-id
;; to given that the values for feature identified by result-feat-id
;; are the ones that are to be predicted
(define (info-gain result-feat-id split-feat-id instances)
  ;;used later for weighting of info-value by ratio
  (define total-insts (length instances))
  
  ;;produces weighted info-values for all the feature-values
  ;;of the feature we're considering splitting on
  (define (info-vals-for-split-feature-attrs val+instances)
      (match val+instances
        [(list _ instances-per-feature-value)
         (define attr-info-value (info-value result-feat-id instances-per-feature-value))
         (* attr-info-value (/ (length instances-per-feature-value)
                               total-insts))]))
            
  ;;the info value of the feature we're considering splitting on
  (define split-feat-info-value
    (--> _ in
         (group-by-feature-value split-feat-id instances)
         (map info-vals-for-split-feature-attrs _)
         (sum _)) )
  
  ;;info gain is info value of result (or predicted) feature 
  ;;minus info value of feature we're thinking of splitting on
  (- (info-value result-feat-id instances)
     split-feat-info-value  ))



;; Calculates the gain ratio by splitting the instances
;; using the values of the feature identified by split-feat-id
;; when predicting values taken from the feature identified
;; by result-feat-id
;; Symbol Symbol ListOf instance -> Number
(define (gain-ratio result-feat-id split-feat-id instances)
  (/ (info-gain result-feat-id split-feat-id instances)
     (info-value split-feat-id instances)))

;; Symbol Any -> Listof Instance
;; Filters instances using feat-value of feature identified by feat-id
(define (find-instances feat-id feat-value instances)
  (filter (λ (inst)
            (eq? feat-value
                 (feature-value (find-feature-by-id feat-id inst))))
          instances))


(module+ 
 test
 
 (require "weather-data.rkt" rackunit)
 
 ;;create some instances and features for testing
 (define instances
   (for/list ([i (in-range (length data))]
              [row data])
     (instance 
      i
      (for/list ([j (in-range (length headers))]
                 [header headers])
        (feature header 'discrete (list-ref row j))))))


 ;;sample data and answers taken from Data Mining -- Witten, Frank and Hall
 (check-= 0.24674981977443888 (info-gain 'play 'outlook instances) 0.001)
 (check-= 0.029222565658954314 (info-gain 'play 'temp instances) 0.001)
 (check-= 0.15183550136234125 (info-gain 'play 'humidity instances) 0.001)
 (check-= 0.048127030408269045 (info-gain 'play 'windy instances) 0.001)
 
 
 (define sunny-insts (find-instances 'outlook 'sunny instances))
 (check-= 0.9709505944546682 (info-value 'play sunny-insts) 0.001)
 (check-= 0.5709505944546682 (info-gain 'play 'temp sunny-insts) 0.001)
 (check-= 0.9709505944546682 (info-gain 'play 'humidity sunny-insts) 0.001)
 (check-= 0.019973094021974447 (info-gain 'play 'windy sunny-insts) 0.001)
 
 )
 

