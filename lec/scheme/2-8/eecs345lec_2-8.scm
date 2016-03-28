#lang racket

(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; tail recursion with accumulator-passing style
(define factorial-acc
  (lambda (n acc)
    (if (zero? n)
        acc
        (factorial-acc (- n 1) (* n acc)))))

(define factorial2
  (lambda (n)
    (factorial-acc n 1)))

; another example of tail recursion via accumulator-passing
(define sumnumbers-acc
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((not (number? (car l))) (sumnumbers-acc (cdr l) acc))
      (else (sumnumbers-acc (cdr l) (+ acc (car l)))))))

(define sumnumbers
  (lambda (l)
     (sumnumbers-acc l 0)))

; tail recursion via accumulator-passing
(define myappend-acc
  (lambda (l1 l2 acc)
    (cond
      ((null? l2) l1)
      ((null? l1) acc)
      (else (myappend-acc (cdr l1) l2 (cons (car l1) acc))))))

(define myappend
  (lambda (l1 l2)
    (myappend-acc l1 l2 l2)))
      
; tail recursion via continuation-passing style
(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* v n)))))))

(define factorial3
  (lambda (n)
    (factorial-cps n (lambda (v) v))))

; myappend with continuation-passing style
(define myappend-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        l2
        (myappend-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))