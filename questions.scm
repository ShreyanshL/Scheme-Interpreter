(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (cond 
    ((null? rests) nil)
    (else (cons (append (list first) (car rests)) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  (define (firsts lst) (car lst))
  (define (rests lst) (cdr lst))
  (cond 
    ((null? (car pairs)) ())
    (else (append (list (map firsts pairs)) (zip (map rests pairs))))
    )
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17

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
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
    (cond 
      ((null? denoms) ())
      ((< (car denoms) total) (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) (list-change total (cdr denoms))))
      ((= (car denoms) total) (append (list (list (car denoms))) (list-change total (cdr denoms)))) 
      (else (list-change total (cdr denoms)))
    )
  ; END PROBLEM 18
)

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ; (cond
           ;  ((= (length (cdr body)) 1) (append `(,form ,params) (list (let-to-lambda (car body))) (list (let-to-lambda (cadr body)))))
           ;  (else (append `(,form ,params) (let-to-lambda body)))
           ;  )
            (append `(,form ,params) (let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cond 
            ((and (list? values) (list? body)) 
              (append (list (append (list 'lambda) (list (let-to-lambda (car (zip values)))) (let-to-lambda body) )) (let-to-lambda (cadr (zip values)))))
            (else expr)
            )
           ; 
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (append (list (let-to-lambda (car expr))) (let-to-lambda (cdr expr)))
         ; END PROBLEM 19
         )))
