;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file hw03skel_2015.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define false #f)
(define nil '())

;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U 
;;;        ({complex} X RepCom) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (repnum->reprat x) (apply-generic 'repnum->reprat x))


;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))


(define (equ? x y) (apply-generic 'equ? x y))

;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))

;;; the ordinary  number package

(define (install-number-package)
  (define (tag x)
    (attach-tag 'number x))
  (define (make-number x) (tag x))
  (define (negate x) (tag (- x)))
  (define (zero? x) (= x 0))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  ; modify if x%y!=0 return rational
  (define (div x y)
    (if (eqv? (remainder x y) 0)
        (tag (/ x y))
        (cons 'rational (cons (cons 'number x) (cons 'number y)))))
  

  
  ;------------------------------------------
  ;=number?   <procedure>
  (define (=number? x y)
    (if (eqv? x y)
        #t
        #f))
    
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)
  ;add equ?
  (put 'equ? '(number number) =number?)
  (put 'repnum->reprat '(number) repnum->reprat)
  'done)

;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) 
  ((get 'make 'number) x))

;;; the rational number package
(define (install-rational-package)
  (define (make-rat n d) (cons n d))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;;; repnum->reprat
  (define (repnum->reprat x) (cons (cons 'number x) (cons 'number 1)))
  ;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
  (define (RRmethod->NRmethod method)
    (lambda (num rat)
      (method
       (repnum->reprat num)
       rat)))

  (define (RRmethod->RNmethod method)
    (lambda (rat num)
      (method
       rat
       (repnum->reprat num))))
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  ; negate-rat <procedure>
  (define (negate-rat x) (tag (make-rat (negate (numer x)) (denom x))))
  
  ;=zero-rat? <procedure>
  (define (zero-rat? x)
    (if (=zero? (numer x))
        #t
        #f))
  ; =rational? <procedure>
  (define (=rational? x y)
    (if (=zero? (numer x))
        (if (=zero? (numer y))
            #t
            #f)
        (if (=zero? (numer y))
            #f
            (if (equ? (mul (numer x) (denom y)) (mul (denom x) (numer y)))
                #t
                #f))))
    
  (define (make-rational n d) (tag (make-rat n d)))
  (define (add-rational x y) (tag (add-rat x y)))
  (define (sub-rational x y) (tag (sub-rat x y)))
  (define (mul-rational x y) (tag (mul-rat x y)))
  (define (div-rational x y) (tag (div-rat x y)))
  (put 'make 'rational make-rational)
  (put 'add '(rational rational) add-rational)
  (put 'sub '(rational rational) sub-rational)
  (put 'mul '(rational rational) mul-rational)  
  (put 'div '(rational rational) div-rational)
  ; add RR->NR , RR->RN
  (put 'add '(number rational) (RRmethod->NRmethod add-rational))
  (put 'add '(rational number) (RRmethod->RNmethod add-rational))
  (put 'sub '(number rational) (RRmethod->NRmethod sub-rational))
  (put 'sub '(rational number) (RRmethod->RNmethod sub-rational))
  (put 'mul '(number rational) (RRmethod->NRmethod mul-rational))
  (put 'mul '(rational number) (RRmethod->RNmethod mul-rational))
  (put 'div '(number rational) (RRmethod->NRmethod div-rational))
  (put 'div '(rational number) (RRmethod->RNmethod div-rational))
  ; additional part negate-rat, =zero-rat?, =rational?
  (put 'negate '(rational) negate-rat)
  (put '=zero? '(rational) zero-rat?)
  (put 'equ? '(rational rational) =rational?)
  (put 'equ? '(rational number) (RRmethod->RNmethod =rational?))
  (put 'equ? '(number rational) (RRmethod->NRmethod =rational?))
  
  'done)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define  (create-rational n d)
  ((get 'make 'rational) n d))





;;; Complex Number Package, rectangular form a+bi

(define (install-complex-package)
  (define (make-com r i) (cons r i))
  (define (real x) (car x))
  (define (imag x) (cdr x))
  (define (add-com x y)
    (make-com (add (real x) (real y))
		  (add (imag x) (imag y))))
  (define (sub-com x y)
    (make-com (sub (real x) (real y))
		  (sub (imag x) (imag y))))
  (define (mul-com x y) 
    (make-com (sub (mul (real x) (real y)) 
		       (mul (imag x) (imag y)))
		  (add (mul (real x) (imag y))
		       (mul (real y) (imag x)))))
  (define (div-com x y)  
    (let ((com-conj (complex-conjugate y)))
       (let ((x-times-com-conj (mul-com x com-conj))
             (y-times-com-conj (mul-com y com-conj)))
	 (make-com (div (real x-times-com-conj) (real y-times-com-conj))
		   (div (imag x-times-com-conj) (real y-times-com-conj))))))
  (define (complex-conjugate x)
    (make-com (real x) 
	      (negate (imag x))))
  (define (=complex? x y)
    (if (equ? (real x) (real y))
        (if (equ? (imag x) (imag y))
            #t
            #f)
        #f))
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex n d) (tag (make-com n d)))
  (define (add-complex x y) (tag (add-com x y)))
  (define (sub-complex x y) (tag (sub-com x y)))
  (define (mul-complex x y) (tag (mul-com x y)))
  (define (div-complex x y) (tag (div-com x y)))
  (put 'make 'complex make-complex)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)  
  (put 'div '(complex complex) div-complex)
  ; equ? <procedure>
  (put 'equ? '(complex complex) =complex?)
  'done)


(define (create-complex r i)
  ((get 'make 'complex) r i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       This is the file type.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE-TAG" datum)
      ))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
		 (list op type-tags))
          ))))
 

;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(install-number-package)
(install-rational-package)
(install-complex-package)
(define a (create-number 1))
(define b (create-complex (create-rational (create-number 3) (create-number 5)) (create-rational (create-number 2) (create-number 5))))
(define c (create-complex (create-number 3) (create-number 2)))
(define d (div c (create-complex (create-number 5) (create-number 0))))
(define r1/1 (create-rational (create-number 1) (create-number 1)))
(define r1/2 (create-rational (create-number 1) (create-number 2)))
(define r1/3 (create-rational (create-number 1) (create-number 3)))
(define r2/7 (create-rational (create-number 2) (create-number 7)))
(define n2 (create-number 2))
(define n3 (create-number 3))
(define n4 (create-number 4))
(define n6 (create-number 6))

;
;ADD YOUR PROGRAM FROM HERE
;

; PUT YOUR COMMON ROUTINES HERE


; EXERCISE 1
;(1) (define (square x) (mul x x)) 에서 mul이 procedure이므로 square은 procedure이다.
;(2) square은 모든 type에 쓸 수 있는 general한 함수인데 저렇게 구현하면 모든 package에 square을 만들어야 하므로 비효율적입니다.
; EXERCISE 2
;(define (make-number x) (tag x)) 에서 tag 가 (define (tag x) (attach-tag 'number x)) 로 procedure 이므로 make-number는 procedure이다.
;(define (negate x) (tag (- x))) negate도 make-number와 마찬가지로 procedure이다.
;(define (zero? x) (= x 0)) = procedure이다.
; EXERCISE 3
;assoc 를 이용해서 다른 tag 들을 구분하고 있습니다. Assoc procedure 은 target list에서 
;특정 object를 포함하고 있는 list 를 찾아주는 procedure입니다. 그래서 tag들을 다 list로 저장하고 있습니다.
; EXERCISE 4
;(1) (define (=number? x y)
;    (if (eqv? x y)
;        #t
;        #f))
;(2)
; 다른 종이에 그림 첨부하였습니다.
; EXERCISE 5
;(1) first-try 가 옳지 않습니다. add를 하면 rational number 가 나오지 않고, (rational #<void> . #<void>) 나옵니다.
;    add-rat 은 (apply-generic ‘mul ~)을 실행하는데  이 때 tag가 없으므로 연산이 실행되지 않기 때문이다.
;(2) 다른 종이에 그림 첨부하였습니다.
; EXERCISE 6
;add-rat 에서 연산에 쓰이는 numerator 와 denominator 의 type이 generic-number 이기
;때문에 add로 해버리면 generic-arithmetic operation의 add와 이름이 똑같아서 에러가 생긴다.
; EXERCISE 7
;(1)
; negate-rat <procedure>
;  (define (negate-rat x) (tag (make-rat (negate (numer x)) (denom x)))) 
  ;=zero-rat? <procedure>
;  (define (zero-rat? x)
;    (if (=zero? (numer x))
;        #t
;        #f))
;     =rational? <procedure>
;  (define (=rational? x y)
;    (if (=zero? (numer x))
;        (if (=zero? (numer y))
;            #t
;            #f)
;       (if (=zero? (numer y))
;            #f
;            (if (equ? (mul (numer x) (denom y)) (mul (denom x) (numer y)))
;                #t
;                #f))))
;(2)
;다른 종이에 그림을 첨부하였습니다.
; EXERCISE 8
;(define (repnum->reprat x) (cons (cons 'number x) (cons 'number 1)))
; EXERCISE 9
;(define (RRmethod->RNmethod method)
;    (lambda (rat num)
;      (method
;       rat
;       (repnum->reprat num))))
; EXERCISE 10
;(1)
; (put 'add '(number rational) (RRmethod->NRmethod add-rational))
; (put 'add '(rational number) (RRmethod->RNmethod add-rational))
; (put 'sub '(number rational) (RRmethod->NRmethod sub-rational))
; (put 'sub '(rational number) (RRmethod->RNmethod sub-rational))
; (put 'mul '(number rational) (RRmethod->NRmethod mul-rational))
; (put 'mul '(rational number) (RRmethod->RNmethod mul-rational))
; (put 'div '(number rational) (RRmethod->NRmethod div-rational))
; (put 'div '(rational number) (RRmethod->RNmethod div-rational))
;(2)
; 다른 종이에 그림 첨부
; EXERCISE 11
; 1+3i 를 5로 나누면 0.2 + 0.6i가 될 것입니다. number / number 이 rational 이 되는 것이 아니라 number 이 되므로
; (‘complex  (‘number.1/5) ‘number.3/5)  가 될 것입니다.
; EXERCISE 12
; 우선 type-tag가 (‘complex ‘complex) 이므로 complex-package의 div 가 진행될 것이다. 
;그러면div -> div-complex -> div-com 으로 들어가는데 div-com에 있는
;mul-com 을 계산할 때 denominator 와 numerator 의 type이 number 이므로 number-package에 있는 연산들이 진행된다.
;그래서 real, imag전부 number type인 것이다.
; EXERCISE 13
;(div ‘number  ‘number ) 의 나머지가 존재할 때 결과값을 rational 로 바꿔주면 된다.
; 장점은 만들어 놓은 number, rational, complex 의 체계를 제대로 활용한다는 것입니다.
;(‘number . 1.2 ) 같은 rational이면서 number로 표현되는 경우가 없어집니다. 
; EXERCISE 14
; complex-package 에 두 complex 의 real 과 imag가 같은지 검사하는 equ? Procedure 이 추가되어야 한다. 
; (define (=complex? x y)
;    (if (equ? (real x) (real y))
;        (if (equ? (imag x) (imag y))
;            #t
;            #f)
;        #f))