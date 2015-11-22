#lang racket
(define (make-prev first)
  (let ((x first))
    (lambda (n)
      (display x)
      (set! x n))))
    
  
(define prev (make-prev 'first-call))