#lang racket/base

(require racket/list)

(provide (all-defined-out) )

(module+ test (require rackunit))

;; (U U -> Bool) (T -> U) Sequenceof T -> Listof (PairOf U (Sequenceof T)
;; Given a sequence of type T, a key producing function (T -> U)  and a
;; comparison function, group-by will produce a List of pairs where
;; the first element of the pair is a key produced by the key-fn and
;; the second element is a Sequence of T taken from the original
;; input where the key-fn applied to T produced the key.
;; the comparison function and key function are both optional
;; default for comparison function is #:eq, for key-fn identity
(define (group-by #:eq [comp-fn equal?] #:key [key-fn values] seq)
  (let ([ht (make-hash)])
        
        ;;iterate through the sequence
        ;;add entries to hash keyed by the key from the item
        (for ([item seq])
          (let ([k (key-fn item)])
            (if (hash-has-key? ht k)
                (hash-set! ht k (cons item (hash-ref ht k)))
                (hash-set! ht k (list item))) ))
    
        ;;return list of the pairs; key with all its values
        (for/list ([(k v) (in-hash ht)])
          (cons k (list v)))) )
 
;;simple test of group-by
(module+ test
         (define group-by-input '((a 2) (a 3) (b 1) (a 4) (c 4) (b 3) (c 12)))
         (define result (group-by #:key first group-by-input))
         (check-eq? (length result) 3 "number of groups produced is not correct")
         (define a-s (findf (λ (p)
                              (eq? (first p) 'a))
                            result) )
         (check-eq? 
          (length (second a-s)) 3 "number of grouped elements is not correct")) 
         

(define-syntax fn 
  (syntax-rules ()
    [(_ body ...) (lambda body ...)]))

(define-syntax -->  
  (syntax-rules (in)
    [(_ it in exp ) exp]
    [(_ it in one two ...) (let ((it one)) (--> it in two ...))]))


(define-syntax let->
  (syntax-rules ()
    [(_  tmp  body) body]
    [(_ tmp a b ... body) (let ([tmp a]) (let-> tmp  b ... body))]))



(define-syntax range-check
  (syntax-rules ()
    [(_ range-defn ... )
     (lambda (var)
     (cond
       [(range-clause-condition var range-defn) 
        (range-clause-action range-defn)]
       ...)) ]))
   

(define-syntax range-clause-condition
  (syntax-rules (< > else)
    [(_ var (< lower _)) (< var lower) ]
    [(_ var (> upper _)) (> var upper)]
    [(_ _ (else _)) 'else]
    [(_ var (lower upper _)) 
     (and
      (> var lower)
      (< var upper))]  ) )

(define-syntax range-clause-action
  (syntax-rules ()
    [(_ (_ _ tag)) tag ]
    [(_ (_ tag)) tag]))




(define-syntax-rule (discretize-num-from-string clause ...)
  (let ([range-fn (range-check clause ...)])
    (lambda (val)
      (let ([num (string->number val)] )
        (if num
        (range-fn (string->number val))
        'missing)))))