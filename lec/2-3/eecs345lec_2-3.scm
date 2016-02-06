#lang racket
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

; memoization: create a new function
(define squarerootm
  (lambda (n k)
    (if (zero? k)
        n
        ((lambda (old)
          (- old (/ (- (* old old) n) (* 2 old)))
         (squarerootm n (- k 1)))))))


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

; accumulator-passing style:
; the accumulator builds up the answer
;  list    acc
; (a b c) '()
; (b c)   (a)
; (c)     (b a)
; ()      (c b a)

; tail recursive: scheme eliminates the return stack (reuses
; the stack frame instead of creating a new one) since it does
; not need to save the current values for when the recursive
; call returns
(define reverse-acc
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((pair? (car l)) (reverse-acc (cdr l) (cons (reverse-acc (car l) '()) acc)))
      ; move first item to end of answer
      (else (reverse-acc (cdr l) (cons (car l) acc))))))


; abstraction
(define sumlist*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((list? (car l)) (+ (sumlist* (car l)) (sumlist* (cdr l))))
      (else (+ (car l) (sumlist* (cdr l)))))))

; abstract the idea of the value of an element
(define valueof
  (lambda (e)
    (if (list? e)
        (sumlist* e)
        e)))

(define numorder*?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((and (list? (car l)) (null? (cdr l))) (numorder*? (car l)))
      ((null? (cdr l)) #t) ;already in order
      ((> (valueof (car l)) (valueof (cadr l))) #f)
      ((list? (car l)) (and (numorder*? (car l)) (numorder*? (cdr l))))
      (else (numorder*? (cdr l))))))

; another abstraction example
(define dotproduct
  (lambda (v1 v2)
    (cond
      ((or (null? v1) (null? v2)) 0)
      (else (+ (* (car v1) (car v2))
               (dotproduct (cdr v1) (cdr v2)))))))

(define firstColumn
  (lambda (m)
    (if (null? m)
        '()
        (cons (car (car m)) (firstColumn (cdr m))))))

(define restOfColumns
  (lambda (m)
    (if (null? m)
        '()
        (cons (cdr (car m)) (restOfColumns (cdr m))))))

(define vectormult
  (lambda (v m)
    (if (null? (car m))
        '()
        (cons (dotproduct v (firstColumn m)) (vectormult v (restOfColumns m))))))

