#lang racket

; my-append will append two lists
(define my-append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (my-append (cdr l1) l2)))))

; continuation passing style
(define my-append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (my-append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

; (length-cps '(a b c d) (lambda (v) v)) => 4
(define length-cps
  (lambda (l return)
    (if (null? l)
        (return 0)
        (length-cps (cdr l) (lambda (v) (return (+ v 1)))))))

; (sumnumbers*-cps '( a 2 (3 4) ((5))) (lambda (v) v)) => 14
(define sumnumbers*-cps
  (lambda (l return)
    (cond
      ((null? l) 0)
      ((number? (car l)) (sumnumbers*-cps (cdr l) (lambda (v) (return (+ v (car l))))))
      ((list? (car l)) (sumnumbers*-cps (cdr l) (lambda (v) (sumnumbers*-cps (car l) (lambda (w) (return (+ w v)))))))
      (else (sumnumbers*-cps (cdr l) return))))) ; (lambda (v) (return v))

; (flatten-cps '(((a)) b ((c d)) e) (lambda (v) v)) => '(a b c d e)
(define flatten-cps
  (lambda (l return)
    (cond
      ((null? l) (return '()))
      ((list? (car l)) (flatten-cps (car l) (lambda (v) (flatten-cps (cdr l) (lambda (w) (return (my-append-cps v w return)))))))
      (else (flatten-cps (cdr l) (lambda (v) (return (cons (car l) v))))))))

; multiply
(define multiply
  (lambda (l)
    (cond
      ((null? l) 1)
      ((zero? (car l)) 0) ; still have frames on the stack
      (else (* (car l) (multiply (cdr l)))))))

; (multiply-cps '(1 3 0 4) (lambda (v) v) (lambda (v) v)) => 0
(define multiply-cps
  (lambda (l return break)
    (cond
      ((null? l) (return 1))
      ((zero? (car l)) (break 0))
      (else (multiply-cps (cdr l) (lambda (v) (return (* v (car l)))))))))
