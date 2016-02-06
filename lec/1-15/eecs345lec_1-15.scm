#lang racket
(define maximum
  (lambda (x y)
    (if (> x y)
        x
        y)))

; take a list and number and return that element of the list
(define getIndex
  (lambda (lis n)
    (cond
      ((null? lis) #f) ; empty list-- should not happen!
      ((zero? n) (car lis))
      (else (getIndex (cdr lis) (- n 1))))))

; (cons elem lis) returns a new list with elem as the new car of lis

; create a list that repeats a given element e, n times
(define repeat
  (lambda (e n)
    (cond
      ((zero? n) '())
      (else (cons e (repeat e (- n 1)))))))

; remove first occurence of a given element in a list
; return the list with the element removed
; (myremove 'apple '(orange apple pear apple banana))
(define myremove
  (lambda (a l)
    (cond
      ((null? l) l)
      ((eq? a (car l)) (cdr l)) ; eq is for ATOMS (==). use '=' for anything else
      (else (cons (car l) (myremove a (cdr l)))))))

