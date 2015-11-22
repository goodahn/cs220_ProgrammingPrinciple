#lang racket
(define prev
  (let ((x 'first-call))
    (lambda (n)
      (display x)
      (set! x n))))

