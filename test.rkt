#lang typed/racket/base



(: add-blaster : Number [#:y Number] [#:eq (Number -> Number)] -> Number)
(define (add-blaster x #:y [y 5] #:eq [fn values]) (+ x y))


(: foo ( [#:f (Number -> Number)]  -> Number))
(define (foo #:f [fn add1])
  (fn 5))

(: id (All (A) A -> Symbol))
(define (id x) 'x)

(: group-by ( All (A B) (  (A -> B) (Sequenceof A) 
                                           -> (HashTable B (Listof A)))))
(define (group-by key-fn seq)
  (let: ([ht : (HashTable B (Listof A)) (make-hash)])
        
        ;;iterate through the sequence
        ;;add entries to hash keyed by the key from the item
        (for ([item seq])
          (let ([k (key-fn item)])
            (if (hash-has-key? ht k)
                (hash-set! ht k (cons item (hash-ref ht k)))
                (hash-set! ht k (list item))) ))
    ht))
