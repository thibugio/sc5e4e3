;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rebecca Frederick (rmf61) ;;
;; EECS 345 - HW 1           ;;
;; 01-30-2016                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;;;;;;;; 1. (inorder? l) ;;;;;;;; 

; (inorder? l): test whether the (numeric) elements in a list are in non-decreasing order
(define inorder?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((null? (cdr l)) #t)
      ((not (number? (car l))) #f)
      (else (and (<= (car l) (car (cdr l)))
                 (inorder? (cdr l)))))))

;;;;;;;; 2. (dotproduct v1 v2) ;;;;;;;;

; (dotproduct v1 v2): compute the dot product of two lists of numbers
(define dotproduct
  (lambda (v1 v2)
    (cond
      ((or (null? v1) (null? v2)) 0)
      ((not (and (number? (car v1)) (number? (car v2)))) 0)
      (else (+ (* (car v1) (car v2))
               (dotproduct (cdr v1) (cdr v2)))))))

;;;;;;;; (squareroot n i) ;;;;;;;;

; (squareroot n i): compute sqrt(n) with 'i' iterations of newton's method
; Newton's method is new = old - ((old * old) - value) / (2 * old)
(define squareroot
  (lambda (n i)
    (if (and (number? n) (number? i) (> i 0))
        (- (squareroot n (- i 1))
           (/ (- (* (squareroot n (- i 1)) (squareroot n (- i 1)))
                 n)
              (* 2 (squareroot n (- i 1)))))
        n)))

;;;;;;;; (removesubsequence seq l) ;;;;;;;;

; (removesubsequence seq l): take 2 lists of atoms and remove the first occurences of atoms in the first list from the second
(define removesubsequence
  (lambda (seq l)
    (cond
      ((or (null? l) (null? seq) ) l)
      ((or (list? (car l)) (list? (car seq))) l) ;list or sequence does not contain only atoms
      ((eq? (car seq) (car l)) (removesubsequence (cdr seq) (cdr l)))
      (else (cons (car l) (removesubsequence seq (cdr l)))))))

;;;;;;; (reverse* l) ;;;;;;;;

; (reverse* l): reverse the contents of the list and all nested lists
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (reverse* (cdr l))
                               (cons (reverse* (car l)) '()))) ; nested lists
      ((null? (cdr l)) l)                         ; single-element list
      (else (append (reverse* (cdr l))
                    (cons (car l) '()))))))

;;;;;;;; (first* l) ;;;;;;;;

; (first* l): return the first atom in a list
(define first*
  (lambda (l)
    (cond
      ((null? l) l)
      ((atom? (car l)) (car l))
      (else (first* (car l))))))

; (atom? a): test whether the argument is an atom
(define atom?
  (lambda (a)
    (if (null? a)
        #t
        (not (list? a)))))

;;;;;;;; (last* l) ;;;;;;;;

; (last* l): return the last atom in a list
(define last*
  (lambda (l)
    (cond
      ((null? l) l)
      (else (first* (reverse* l))))))

;;;;;;;; (numorder*? l) ;;;;;;;;

; (numorder*? l): test whether the values of the (possibly nested) elements of a list are in non-decreasing order
; value of a number is the number
; value of a list is the sum of the values in the list
(define numorder*?
  (lambda (l)
    (if (null? (cdr l))
        #t
        (and (<= (value (car l)) (value (car (cdr l))))
                 (numorder*? (cdr l))))))

; calculate the value of an item
(define value
  (lambda (e)
    (cond
      ((null? e) 0)   ; empty list      
      ((number? e) e) ; number
      ((atom? e) 0)   ; other atomic symbol
      (else (+ (value (car e)) (value (cdr e)))))))

;;;;;;;; (vectormult v m) ;;;;;;;;

; (vectormult vec mat): multiply a row vector and a matrix
; assume the length of the vector matches the number of rows of the matrix
(define vectormult
  (lambda (v m)
    (if (null? (car m))
        '()
        (append (cons (dotproduct v (cars m)) '())
                (vectormult v (decar m))))))

; return the heads of a list of lists
(define cars
  (lambda (l)
    (if (null? l)
        '()
        (append (cons (car (car l)) '())
                    (cars (cdr l))))))

; remove the first element in each list
(define decar
  (lambda (l)
    (if (null? l)
        '()
        (cons (cdr (car l))
                  (decar (cdr l))))))

;;;;;;;; (matrixmultiply m1 m2) ;;;;;;;;

; (matrixmultiply m1 m2): multiply two matrices
; assume the number of columns of m1 equals the number of rows of m2
(define matrixmultiply
  (lambda (m1 m2)
    (if (null? m1)
        '()
        (cons (vectormult (car m1) m2)
              (matrixmultiply (cdr m1) m2)))))

