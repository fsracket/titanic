#lang racket

(require "util.rkt")

(provide (all-defined-out))

;;id of feature is its name
;;type is either 'discrete or 'numeric (currently only use 'discrete in my code)
;;value is the feature's value
(struct feature (id type value))

;; id will likely be some numeric surrogate key (instance 1, 2...)
;; features is a list of feature structs
(struct instance (id features))

(module+ test
         (require "weather-data.rkt" rackunit)
         
         ;;create some instances and features for testing
         (define instances
           (for/list ([i (in-range (length data))]
                      [row data])
             (instance 
              i
              (for/list ([j (in-range (length headers))]
                         [header headers])
                (feature header 'discrete (list-ref row j)))))) )

(define (unique-feature-ids inst)
  (map feature-id (instance-features inst)))

;; Symbol instance -> feature
;; Finds the feature of instance identified by feat-id
(define (find-feature-by-id feat-id inst)
  (for/first ([feat (instance-features inst)] 
              #:when (equal? feat-id (feature-id feat)))
    feat))

;; tests for find-feature-by-id
(module+ test
         (check-eq? (feature-value (find-feature-by-id 'outlook (first instances))) 
                    'sunny)
         (check-eq? (feature-value (find-feature-by-id 'windy (second instances))) 
                    'true)
         (check-eq? (find-feature-by-id 'foo (second instances)) 
                    #f))
         

;;given a list of instances and a feature id
;;returns a list of features with that id
;;where each feature is obtained from every instance (if it exists)
(define (features-by-id feat-id instances) 
  (for*/list ([inst instances]
             [f (list (find-feature-by-id feat-id inst))]
             #:unless (false? f))
     f))

;; some unconvincing tests for features-by-id
(module+ test
         (check-eq? (length (features-by-id 'outlook instances)) 14)
         (check-eq? (length (features-by-id 'temp instances)) 14) )

;; Symbol -> ( instance -> T) => where T is some value of a feature identified by Symbol
;; Produces a function that takes an instance, finds its feature identified by id
;; and returns that feauture's value
(define (feature-key-fn id)
  (λ (instance)
    (for/first ([f (instance-features instance)]
                #:when (eq? (feature-id f) id))
      (feature-value f))))

;; test feature-key-fn
(module+
 test
 (define outlook-value (feature-key-fn 'outlook))
 (check-eq? 'sunny (outlook-value (first instances)))
 (check-eq? 'rainy (outlook-value (list-ref instances 13))))

;; Symbol Listof instance -> Listof (Pairof Symbol (Listof instance))
;; Groups the sequence of instances by the distinct values
;; of the feature identified by feat-id
(define (group-by-feature-value feat-id instances)
  (group-by #:key (feature-key-fn feat-id) 
                   instances))

(module+
 test
 (check-eq? 3 (length (group-by-feature-value 'outlook instances))))