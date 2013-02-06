
#lang racket
(require scribble/srcdoc
         (for-doc racket/base scribble/manual))
(provide
 (thing-doc
  fish (listof number?)
  ("Our fish, each represented as a number.")))
(define fish '(1 2))
(provide
 (proc-doc/names
  feed (number? . -> . number?) (n)
  ("Feed 1 pound of food to the fish " (racket n) ".")))
(define (feed n) (+ n 1))