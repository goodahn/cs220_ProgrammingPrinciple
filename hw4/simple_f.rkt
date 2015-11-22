#lang racket
(define table -1)
(define (f x)
  (cond ((eq? table -1)
         (set! table x)
         x)
        (else
         (set! table -1)
         0)))