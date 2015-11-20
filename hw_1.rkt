#lang racket
(define (cube-root x)
  (define (better y)
    (+ (/ x (* y y 3)) (/ (* 2 y) 3)))
  (define (try y)
    (if (< (abs (- (* y y y) x)) 0.001)
        y
        (try (better y))))
  (try 1))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(define (term n) n)
(define (square n) (* n n))
(define (next n) (+ n 1))
(define (product term a next b)
 (if (> a b)
  1
  (* (term a) (product term (next a) next b))))
(define (product-2 a b) (iter-product 1 a b)) 
(define (iter-product prod a b)
  (if (> a b)
      prod
      (iter-product (* prod  a) (+ a 1) b)))
(define (factorial n) (product term 1 next n))
(define (pi-approxi n)
  (define upper
    (* (square (* (A 1 n) (factorial n))) (* 2 (+ n 1)) 2))
  (define lower
    (square (/ (factorial (+ (* 2 n) 1)) (* (A 1 n) (factorial n)))))
  (/ upper lower))
(define (nmult n)
  (if (< (/ n 2) 1)
      0
      (cond ((= (remainder n 2) 1) (+ 2 (nmult (/ (- n 1) 2))))
            (else (+ 1 (nmult (/ n 2)))))))

(cube-root 27)

(A 1 10); (define (f n) (A 0 n))  => 2n   , (define (g n) (A 1 n))  =>  2**n ,  (define (h n) (A 2 n)) => 2**(n-1) 
(A 2 4)
(A 3 3)
(factorial 10)
(pi-approxi 100)
(product-2 1 10)
(nmult 10)