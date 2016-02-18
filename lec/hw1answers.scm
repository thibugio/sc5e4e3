; 1. inorder?
(define inorder?
  (lambda (l)
    (cond
      ((or (null? l) (null? (cdr l))) #t)
      ((> (car l) (cadr l)) #f)
      (else (inorder? (cdr l))))))

; 2. dotproduct
(define dotproduct
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        0
        (+ (* (car v1) (car v2)) (dotproduct (cdr v1) (cdr v2))))))

; 3. squareroot  new = old - (old^2 - value) / (2 old)
;    We have to think of the problem recursively instead of iteratively
;    The result is an exponential algorithm
(define squareroot
  (lambda (n k)
    (if (zero? k)
        n
        (- (squareroot n (- k 1))
           (/ (- (* (squareroot n (- k 1)) (squareroot n (- k 1))) n)
              (* 2 (squareroot n (- k 1))))))))

; Another version.  Here we create a function that takes the old value as input.
;   The result is a linear time algorithm.
(define squareroot
  (lambda (n k)
    (if (zero? k)
        n
        ((lambda (old)
           (- old (/ (- (* old old) n) (* 2 old))))
         (squareroot n (- k 1))))))

; 5. reverse*: the code is very much like the reverse example from lecture
(define reverse*
  (lambda (l)
    (cond
      ((null? l) '())
      ((pair? (car l)) (append (reverse* (cdr l)) (cons (reverse* (car l)) '())))
      (else (append (reverse* (cdr l)) (cons (car l) '()))))))

; reverse using an accumulator: the method is now linear time instead of quadratic time
;   accumulator-style coding is where we have an accumulator variable "collect" the 
;     the result of the computation.  Call this with (reverse-acc l '())
(define reverse-acc
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((pair? (car l)) (reverse-acc (cdr l) (cons (reverse-acc (car l) '()) acc)))
      (else (reverse-acc (cdr l) (cons (car l) acc))))))


; 6. first*
(define first*
  (lambda (l)
    (if (pair? (car l))
        (first* (car l))
        (car l))))

; 7. last*
(define last*
  (lambda (l)
    (cond
      ((not (null? (cdr l))) (last* (cdr l)))
      ((pair? (car l)) (last* (car l)))
      (else (car l)))))

; 8. numorder*? We need the helper function sumlist*, but we will also abstract out
;     the idea of the value of an element of a list.  This will simplify the number
;     of cases for numorder*?
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
      ((null? (cdr l)) #t)
      ((> (valueof (car l)) (valueof (cadr l))) #f)
      ((list? (car l)) (and (numorder*? (car l)) (numorder*? (cdr l))))
      (else (numorder*? (cdr l))))))

; 9. vector multiply.  We will first use abstraction to create functions that identify
;      the first column of a matrix and return all but the first column of the matrix
(define firstcolumn
  (lambda (m)
    (if (null? m)
        '()
        (cons (car (car m)) (firstcolumn (cdr m))))))

(define restOfColumns
  (lambda (m)
    (if (null? m)
        '()
        (cons (cdr (car m)) (restOfColumns (cdr m))))))
      
(define vectormult
  (lambda (v m)
    (if (or (null? m) (null? (car m)))
        '()
        (cons (dotproduct v (firstcolumn m)) (vectormult v (restOfColumns m))))))

; 10. matrix multiply
(define matrixmultiply
  (lambda (m1 m2)
    (if (null? m1)
        '()
        (cons (vectormult (car m1) m2) (matrixmultiply (cdr m1) m2)))))