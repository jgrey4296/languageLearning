#lang racket

(define-syntax (foo stx)
  (print stx)
  #'"blah")

(foo 5)
