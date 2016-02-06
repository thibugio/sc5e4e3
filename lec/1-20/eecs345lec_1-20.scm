#lang racket
; recursive removeAll
; remove all occurrences of a given element from a list
(define removeAll
  (lambda (a l)
    (cond
      ((null? l) l)
      ((null? (car l)) l)
      ((list? (car l)) (cons (removeAll a (car l)) (removeAll a (cdr l))))
      ((eq? (car l) a) (removeAll a (cdr l)))
      (else (cons (car l) (removeAll a (cdr l)))))))

; test if two lists are equal in content and structure
; (eq-list* '(a (v)) '(a (v))) => #t
; (eq-list* '((a) v) '(a (v))) => #f
(define eq-list*
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      ((and (list? (car l1)) (list? (car l2))) (and (eq-list* (car l1) (car l2))
                                                    (eq-list* (cdr l1) (cdr l2))))
      ((or (list? (car l1)) (list? (car l2))) #f)
      (else (and (eq? (car l1) (car l2))
                 (eq-list* (cdr l1) (cdr l2)))))))
      

; determine whether an element appears in a list
(define member?*
  (lambda (e l)
    (cond
      ((null? l) (null? e))
      ((list? (car l)) (or (member?* e (car l))
                           (member?* e (cdr l))))
      (else (or (eq? (car l) e)
                (member?* e (cdr l)))))))

; sum the numbers in a list
(define sum*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l) (sum* (cdr l))))
      ((list? (car l)) (+ (sum* (car l)) (sum* (cdr l))))
      (else (+ (car l) (sum* (cdr l)))))))

; remove all elements from a list, but preserve structure
(define emptyall
  (lambda (l)
    (cond
      ((null? l) l)
      ((list? (car l)) (cons (emptyall (car l)) (emptyall (cdr l))))
      (else (emptyall (cdr l))))))

; move all elements of a list into top-level list
(define flatten
  (lambda (l)
    (cond
      ((null? l) l)
      ((null? (car l)) (flatten (cdr l)))
      ((pair? (car l)) (cons (car (flatten (car l)))
                             (flatten (cons (cdr (car l))
                                            (cdr l)))))
      (else (cons (car l) (flatten (cdr l)))))))
;; more elegant method:
(define flatten2
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))