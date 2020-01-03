#lang racket
(require (for-syntax racket/match))

(define-syntax (myMacro stx)
  (match (syntax->list stx)
    [(list name l1 l2)
     (datum->syntax stx `(append ,l2 ,l1))]))


(myMacro `(1 2 3 4) `(3 5 6 7))
