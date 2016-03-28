#lang racket
; member?
; input: a symbol and a list
; returns: whether the symbol is in the list
(define member?
  (lambda (a lis)
    (if (null? lis)
        #f ; false
        (if (eq? a (car lis))
            #t
            (member? a (cdr lis))))))

; illustrates use of (cond ...) in stead of nested (if ...) statements
(define member2?
  (lambda (a lis)
    (cond
      ((null? lis) #f)
      ((eq? a (car lis)) #t)
      (else (member2? a (cdr lis))))))

(define factorial
  (lambda (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))

; calculate the sum of the numbers in a list
(define sumn
  (lambda (lis)
        (if (null? lis)
            0
            (if (number? (car lis))
                (+ (car lis) (sumn (cdr lis)))
                (sumn (cdr lis))))))

; count the number of times a symbol appears in a list
(define countn
  (lambda (val lis)
    (cond
      ((null? lis) 0)
      ((eq? val (car lis)) (+ 1 (countn val (cdr lis))))
      (else (countn val (cdr lis))))))