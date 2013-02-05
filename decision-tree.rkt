#lang racket/base

(require racket/list racket/match "info-value.rkt" "model.rkt" "util.rkt")

(provide (all-defined-out))

;; Symbol Listof instance -> (instance -> symbol :: predicted-feature-value) 
;; Creates a decision tree for the predicted-feature given the list of instances
;; decision tree is represented by a function which takes an instance
;; and returns a valid value of the predicted feature
(define (make-decision-tree predicted-feature-id instances-list)
  (define feature-ids 
    (let-> _ (unique-feature-ids (first instances-list))
           (filter (λ (id) (not (eq? id predicted-feature-id))) _)))
  
  (make-dt-helper predicted-feature-id feature-ids instances-list))


(define (make-dt-helper predicted-feature-id feature-ids instances)
  ;; calculate gain ratio for every candidate feature
  (define ids-and-ratios
    (map (λ (id)
           (cons id (gain-ratio predicted-feature-id id instances)))
         feature-ids))
  
  ;;calculate average gain ratio
  (define avg-gain-ratio 
    (/ (for/sum ([a-pair ids-and-ratios])
                (cdr a-pair))
       (length ids-and-ratios)))
  
  ;;only consider features where gain ratio > 0 and gain-ratio >= avg
  (define filtered-candidates
    (filter (λ (id-and-ratio)
              (define ratio (cdr id-and-ratio))
              (and (>= ratio avg-gain-ratio)
                   (> ratio 0)))
            ids-and-ratios))
  
  (if (empty? filtered-candidates)
      ;; if no suitable candidates make decision based on values of predicted feature
      (make-decision-function-from-predicted-values 
       (map feature-value (features-by-id predicted-feature-id instances)))
      
      ;;make the decision function based off of the feature values
      ;;of the feature with the biggest gain ratio
      (make-decision-function-from-feature-values 
       (let-> _ filtered-candidates
              (sort _ > #:key cdr)  ;;
              (car (first _))) ;takes the id of the feature with the biggest gain ratio
       predicted-feature-id
       feature-ids
       instances) ))
       
;; Listof symbol -> (instance -> symbol)
;; given a list of feature values, returns a function which when given any
;; input value will return the feature value that occurs most often
(define (make-decision-function-from-predicted-values feature-values)
  (define majority-value
    (let-> _ (group-by feature-values)
           (map (match-lambda [(list val items) (list val (length items))]) _)
           (sort _ > #:key second)
           (first (first _))))  
  (λ (instance)
    majority-value))

;; Symbol Symbol (Listof Symbol) (Listof instance) -> 
;;                                              (instance -> symbol :: value of predicted feature)
;; Creates a function which takes an instance and returns a value which
;; is one of the allowable values of the predicted feature
;; the function is built by getting all the distinct values of 'feature-id',
;; figuring out the subsets of instances that are associated with each distinct value
;; and then calling make-dt-helper to generate functions for each of these instance subsets
(define (make-decision-function-from-feature-values feature-id predicted-id feature-ids instances)
  (define grouped-instances (group-by-feature-value feature-id instances))
  (define other-feature-ids (filter (λ (id) (not (eq? id feature-id))) feature-ids))
  (define value-fn-pairs
    (map (match-lambda 
           [(list value instances)
            (cons value (make-dt-helper predicted-id other-feature-ids instances))])
         grouped-instances))
  
  (define value-fn-hash (make-hash value-fn-pairs))
  
  (λ (instance)
    (define val (feature-value (find-feature-by-id feature-id instance)))
    (define fn (hash-ref value-fn-hash val))
    (fn instance)))




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
 
 (define fn (make-decision-tree 'play instances))
 
 (fn (first instances)))
 
 
  

  