;;; Test cases for Scheme.
;;;
;;; In order to run only a prefix of these examples, add the line
;;;
;;; (exit)
;;;
;;; after the last test you wish to run.

;;; ********************************
;;; *** Add your own tests here! ***
;;; ********************************
; BEGIN PROBLEM 0

;Q6
(define x 0 0)
; expect Error

(define x 1)
;expect x
x
; expect 1

define(x 0)
; expect Error

((define x (+ x 1)) 2)
; expect Error

;Q7

''''hi
; expect (quote (quote (quote hi)))

'(1 2)
; expect (1 2)

'(1 (2 god (0 5)))
; expect (1 (2 god (0 5)))

(car '(car cdr))
; expect car

(eval (car '(car cdr)))
; expect #[car]

`(1 ,(+ 1 1) 3)
; expect (1 2 3)

;Q8

(begin (+ 1 4) (+ 22 10))
; expect 32

(begin (+ 2 3) (+ 5 6) (begin (+ 2 3) (+ 5 60)))
; expect 65

(define x (begin (display 3) (newline) (+ 2 3)))
; expect x

(+ x 3)
; expect 8

(define x (begin (display 3) (newline) (+ 2 3) (define y (begin (+ 2 3)))))
; expect x

x
; expect y

y
; expect 5

;Q9

(lambda (x) (define y x) y)
; expect (lambda (x) (define y x) y)

(lambda (x) (define y x) (lambda (x) (define y x) y))
; expect (lambda (x) (define y x) (lambda (x) (define y x) y))

;Q10

(define (g x) (* x 10))
; expect g

g
; expect (lambda (x) (* x 10))

(define (g x) (define (f y) (g (f (f y)))))
; expect g

g
; expect (lambda (x) (define (f y) (g (f (f y)))))

f
; expect Error

;Q11&Q12
(define (enumerate s)

  (define (indexes s)
    (cond 
      ((null? s) ())
      (else (cons (- (length s) 1) (indexes (cdr s))))
    )
    )

  (define (reverse lst)
    (cond 
      ((null? lst) ())
      ((null? (cdr lst)) lst)
      (else (append (reverse (cdr lst)) (list (car lst))))
    )
  )

  (define ind (reverse (indexes s)))

  (define (zip s ind)
    (cond
      ((null? s) ())
      (else (cons (cons (car ind) (cons (car s) nil)) (zip (cdr s) (cdr ind)) ))
    )
  )
  (zip s ind)
)
; expect enumerate

(enumerate '(3 4 5 6))
; expect ((0 3) (1 4) (2 5) (3 6))

(define (cons-all first rests)
  (cond 
    ((null? rests) nil)
    (else (cons (append (list first) (car rests)) (cons-all first (cdr rests))))
  )
)

; expect cons-all

(define (list-change total denoms)
    (cond 
      ((null? denoms) ())
      ((< (car denoms) total) (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
      ((= (car denoms) total) (append (list (list (car denoms))) (list-change total (cdr denoms)))) 
      (else (list-change total (cdr denoms)))
    )
)
; expect list-change

(list-change 20 '(20 10 5))
; expect ((20) (10 10) (10 5 5) (5 5 5 5))

(list-change 10 '(25 10 5 1))
; expect ((10) (5 5) (5 1 1 1 1 1) (1 1 1 1 1 1 1 1 1 1))

(define (sign x)
  (cond
    ((> x 0) 1)
    ((< x 0) -1)
    (else 0)
    )
)

; expect sign

(sign 0)
; expect 0

(sign 1)
; expect 1

(sign -12)
; expect -1

(define (square x) (* x x))
; expect square

(square 3)
; expect 9

(define (pow b n)
  (cond 
    ((= n 0) 1) 
    ((even? n) (square (pow b (/ n 2))))
    (else (* b (pow b (- n 1))))
    )
)
; expect pow

(pow 2 3)
; expect 8

(define (ordered? s)
  (cond
    ((null? (cdr s)) #t)
    ((<= (car s) (car (cdr s))) (ordered? (cdr s)))
    (else #f)
    )
)
; expect ordered?

(ordered? '(1 2 3))
; expect #t

(ordered? '(1 2 4 3))
; expect #f

(define (larger-values x lst)
  (cond
    ((null? lst) ())
    (else (filter (lambda (y) (> y x)) lst))
  )
)
; expect larger-values

(larger-values 7 '(1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9))
; expect (8 9 8 9)

(define (longest-increasing-subsequence lst)
    ; the following skeleton is optional, remove if you like
    (if (null? lst)
        nil
        (begin
            (define first (car lst))
            (define rest (cdr lst))

            (define large-values-rest
                (larger-values first rest))

            (define with-first
                (append (list first) (longest-increasing-subsequence large-values-rest)))

            (define without-first
                (longest-increasing-subsequence (cdr lst)))

            (if (> (length with-first) (length without-first) )
                with-first
                without-first)
        )
    )
)
; expect longest-increasing-subsequence

(longest-increasing-subsequence '(1 3 2 4 6 5 7 9 8))
; expect (1 2 4 5 7 8)

(define-macro (list-of map-expr for var in lst if filter-expr)
  `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)
; expect list-of 

(list-of (* x x) for x in '(1 2 3 4 5 6 7 8 9) if (odd? x))
; expect (1 9 25 49 81)

(define (remove item lst)
  (cond 
    ((null? lst) () )
    ((= item (car lst)) (remove item (cdr lst))) 
    (else (cons (car lst) (remove item (cdr lst))))
   )
)
; expect remove

(remove 3 nil)
; expect ()
(remove 3 '(1 3 5))
; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))
; expect (3 1 4 4)

(define (repeatedly-cube n x)
    (if (zero? n)
        x
        (let
            ((y (repeatedly-cube (- n 1) x)))
            (* y y y))))
; expect repeatedly-cube

(repeatedly-cube 5 1.1)
; expect 11439906988.063293

 (repeatedly-cube 5 2)
 ; expect 14134776518227074636666380005943348126619871175004951664972849610340958208


(car '(1 2))
; expect 1

(cdr '(1 2))
; expect (2)

(define (cadr s) (car (cdr s)))
; expect cadr

(cadr '(1 2))
; expect 2

(define (cddr s) (cdr (cdr s)))
; expect cddr

(cddr '(1 2))
; expect ()

(define (caddr s) (car (cdr (cdr s))))
; expect caddr

(caddr '(1 2 3))
; expect 3

(+ 42 a)
; expect Error

(define (dividebyzero x) (/ x 0))
; expect dividebyzero

(dividebyzero 2)
; expect Error

'a
; expect a

(quote a)
; expect a

(unquote (quote a))
; expect Error

'(42 is the answer to life the universe and everything!)
; expect (42 is the answer to life the universe and everything!)

(begin 1)
; expect 1

(begin 1 2)
; expect 2

(begin a)
; expect Error

(begin 'a)
; expect a

(begin 42 'fortytwo)
; expect fortytwo

(lambda (x y) (* x y))
; expect (lambda (x y) (* x y))

(lambda (12) (* 4 3))
; expect Error

((lambda (y) (* y 2) 42) 5)
; expect 42


; END PROBLEM 0

;;; These are examples from several sections of "The Structure
;;; and Interpretation of Computer Programs" by Abelson and Sussman.

;;; License: Creative Commons share alike with attribution

;;; 1.1.1

10
; expect 10

(+ 137 349)
; expect 486

(- 1000 334)
; expect 666

(* 5 99)
; expect 495

(/ 10 5)
; expect 2

(+ 2.7 10)
; expect 12.7

(+ 21 35 12 7)
; expect 75

(* 25 4 12)
; expect 1200

(+ (* 3 5) (- 10 6))
; expect 19

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
; expect 57

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
; expect 57

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move the following (exit) line down the file to run additional tests. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 1.1.2

(define size 2)
; expect size
size
; expect 2

(* 5 size)
; expect 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
; expect 314.159

(define circumference (* 2 pi radius))
circumference
; expect 62.8318

;;; 1.1.4



(define (square x) (* x x))
; expect square
(square 21)
; expect 441

(define square (lambda (x) (* x x))) ; See Section 1.3.2
(square 21)
; expect 441

(square (+ 2 5))
; expect 49

(square (square 3))
; expect 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4)
; expect 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5)
; expect 136

;;; 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(abs -3)
; expect 3

(abs 0)
; expect 0

(abs 3)
; expect 3

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -2)
; expect 5

;;; 1.1.7

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.1.8

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(sqrt 9)
; expect 3.00009155413138

(sqrt (+ 100 37))
; expect 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; expect 1.7739279023207892

(square (sqrt 1000))
; expect 1000.000369924366

;;; 1.3.1

(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
; expect 3025

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
; expect 55

;;; 1.3.2

((lambda (x y z) (+ x y (square z))) 1 2 3)
; expect 12

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 3 4)
; expect 456

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)
; expect 38

(let ((x 3)
      (y (+ x 2)))
  (* x y))
; expect 21

;;; 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 (cons 2 nil)))
(car x)
; expect 1

(cdr x)
; expect (2)

(define x (list 1 2))
(define y (list 3 4))
(define z (cons x y))
(car (car z))
; expect 1

(car (cdr z))
; expect 3

z
; expect ((1 2) 3 4)

(define (make-rat n d) (list n d))
(define (numer x) (car x))
(define (denom x) (car (cdr x)))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define one-half (make-rat 1 2))
(print-rat one-half)
; expect 1/2

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
; expect 5/6

(print-rat (mul-rat one-half one-third))
; expect 1/6

(print-rat (add-rat one-third one-third))
; expect 6/9

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))
; expect 2/3

(define one-through-four (list 1 2 3 4))
one-through-four
; expect (1 2 3 4)

(car one-through-four)
; expect 1

(cdr one-through-four)
; expect (2 3 4)

(car (cdr one-through-four))
; expect 2

(cons 10 one-through-four)
; expect (10 1 2 3 4)

(cons 5 one-through-four)
; expect (5 1 2 3 4)

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
; expect (10 2.5 11.6 17)

(map (lambda (x) (* x x))
     (list 1 2 3 4))
; expect (1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(scale-list (list 1 2 3 4 5) 10)
; expect (10 20 30 40 50)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
; expect 4

(count-leaves (list x x))
; expect 8

;;; 2.2.3

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))
; expect (1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
; expect 15

(accumulate * 1 (list 1 2 3 4 5))
; expect 120

(accumulate cons nil (list 1 2 3 4 5))
; expect (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
; expect (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; expect (1 2 3 4 5)

;;; 2.3.1

(define a 1)

(define b 2)

(list a b)
; expect (1 2)

(list 'a 'b)
; expect (a b)

(list 'a b)
; expect (a 2)

(car '(a b c))
; expect a

(cdr '(a b c))
; expect (b c)

(define (memq item x)
  (cond ((null? x) #f)
        ((equal? item (car x)) x)
        (else (memq item (cdr x)))))
(memq 'apple '(pear banana prune))
; expect #f

(memq 'apple '(x (apple sauce) y apple pear))
; expect (apple pear)

(define (my-equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (my-equal? (car x) (car y))
                        (my-equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (equal? x y))))
(my-equal? '(1 2 (three)) '(1 2 (three)))
; expect #t

(my-equal? '(1 2 (three)) '(1 2 three))
; expect #f

(my-equal? '(1 2 three) '(1 2 (three)))
; expect #f

;;; Peter Norvig tests (http://norvig.com/lispy2.html)

(define double (lambda (x) (* 2 x)))
(double 5)
; expect 10

(define compose (lambda (f g) (lambda (x) (f (g x)))))
((compose list double) 5)
; expect (10)

(define apply-twice (lambda (f) (compose f f)))
((apply-twice double) 5)
; expect 20

((apply-twice (apply-twice double)) 5)
; expect 80

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(fact 3)
; expect 6

(fact 50)
; expect 30414093201713378043612608166064768844377641568960512000000000000

(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))
(zip (list 1 2 3 4) (list 5 6 7 8))
; expect ((1 5) (2 6) (3 7) (4 8))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
(riff-shuffle (list 1 2 3 4 5 6 7 8))
; expect (1 5 2 6 3 7 4 8)

((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))
; expect (1 3 5 7 2 4 6 8)

(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))
; expect (1 2 3 4 5 6 7 8)

;;; Additional tests

(apply square '(2))
; expect 4

(apply + '(1 2 3 4))
; expect 10

(apply (if #f + append) '((1 2) (3 4)))
; expect (1 2 3 4)

(if 0 1 2)
; expect 1

(if '() 1 2)
; expect 1

(or #f #t)
; expect #t

(or)
; expect #f

(and)
; expect #t

(or 1 2 3)
; expect 1

(and 1 2 3)
; expect 3

(and #f (/ 1 0))
; expect #f

(and #t (/ 1 0))
; expect Error

(or 3 (/ 1 0))
; expect 3

(or #f (/ 1 0))
; expect Error

(or (quote hello) (quote world))
; expect hello

(if nil 1 2)
; expect 1

(if 0 1 2)
; expect 1

(if (or #f #f #f) 1 2)
; expect 2

(define (loop) (loop))
(cond (#f (loop))
      (12))
; expect 12

((lambda (x) (display x) (newline) x) 2)
; expect 2 ; 2

(define g (mu () x))
(define (high f x)
  (f))

(high g 2)
; expect 2

(define (print-and-square x)
  (print x)
  (square x))
(print-and-square 12)
; expect 12 ; 144

(/ 1 0)
; expect Error

(define addx (mu (x) (+ x y)))
(define add2xy (lambda (x y) (addx (+ x x))))
(add2xy 3 7)
; expect 13

(let ((x 2)) ((begin (define x (+ x 1)) +) 3 (begin (define x (+ x 1)) x)))
; expect 7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 1 (len (cdr s)))))
(len '(1 2 3 4))
; expect 4


;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;


; Tail call optimization tests

(define (sum n total)
  (if (zero? n) total
    (sum (- n 1) (+ n total))))
(sum 1001 0)
; expect 501501

(define (sum n total)
  (cond ((zero? n) total)
        (else (sum (- n 1) (+ n total)))))
(sum 1001 0)
; expect 501501

(define (sum n total)
  (begin 2 3
    (if (zero? n) total
      (and 2 3
        (or #f
          (begin 2 3
            (let ((m n))
              (sum (- m 1) (+ m total)))))))))
(sum 1001 0)
; expect 501501



; macro tests

(define (map f lst)
    (if (null? lst)
        nil
        (cons
            (f (car lst))
            (map f (cdr lst)))))

(define-macro (for formal iterable body)
         (list 'map (list 'lambda (list formal) body) iterable))

(for i '(1 2 3)
    (if (= i 1)
        0
        i))
; expect (0 2 3)

(define (cadr s) (car (cdr s)))
(define (cars s) (map car s))
(define (cadrs s) (map cadr s))

(define-macro (leet bindings expr)
    (cons
        (list 'lambda (cars bindings) expr)
        (cadrs bindings)))

(define (square x) (* x x))
(define (hyp a b)
  (leet ((a2 (square a)) (b2 (square b))) (sqrt (+ a2 b2))))

(hyp 3 4)
; expect 5.000023178253949
