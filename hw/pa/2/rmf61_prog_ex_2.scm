; Rebecca Frederick
; EECS 345
; Programming Exercise 2
; 02/21/2015

; todo:
; suffix
; suffix2

; dotproduct takes a two vectors (lists of numbers) and computes the dot product
; of the vectors. If one list is longer than the other, you can ignore the extra
; numbers of the longer list.

; initial return function: (lambda (v) v)
(define dotproduct-cps
  (lambda (l1 l2 return)
    (if (or (null? l1) (null? l2)) 
        (return 0)
        (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (return (+ v (* (car l1) (car l2)))))))))


; removesubsequence takes two lists of atoms. The first list is a subsequence of
; the second list. The method should return the second list with the first
; occurence of the subsequence removed. So, if the first list is '(a b c), the
; first a if the second list is removed, the first b that appears after the
; removed a is removed, and the first c that appears after the removed b is
; removed.

; initial return function: (lambda (v) v)
(define removesubsequence-cps
  (lambda (seq list return)
    (cond
      ((or (null? list) (null? seq)) (return list))
      ((eq? (car list) (car seq)) (removesubsequence-cps (cdr seq) (cdr list) return))
      (else (removesubsequence-cps seq (cdr list) (lambda (v) (return (cons (car list) v))))))))


; squareroot takes two numbers, a value and an iteration. The iteration will be
; an integer greater than or equal to 0. The method will compute the squareroot
; of the value using iteration rounds of Newton's method, starting with an
; initial value equal to the input value.
; Newton's method is new = old - ((old * old) - value) / (2 * old)

; initial return function: (lambda (v) v)
(define squareroot-cps
  (lambda (n k return)
    (if (zero? k)
        (return n)
        (squareroot-cps n (- k 1) (lambda (v) (return (- v (/ (- (* v v) n) (* 2 v)))))))))


; replaceall* takes two atoms and a nested list and replaces every occurrence of
; the first atom with the second

; initial return function: (lambda (v) v)
(define replaceall*-cps
  (lambda (a b l return)
    (cond
      ((null? l) (return '())) ;v1: before and after (car l); v2: before (car l) and (car l)
      ((list? (car l)) (replaceall*-cps a b (car l) (lambda (vcar) (return (replaceall*-cps a b (cdr l) (lambda (vcdr) (append-cps (cons vcar '()) vcdr (lambda (v) v))))))))
      ((eq? (car l) a) (replaceall*-cps a b (cdr l) (lambda (v) (return (cons b v)))))
      (else (replaceall*-cps a b (cdr l) (lambda (v) (return (cons (car l) v))))))))

(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))


; reverse* takes a nested list and reverses the contents of the list and all
; nested lists

; initial return function: (lambda (v) v)
(define reverse*-cps
  (lambda (l return)
    (cond
      ((null? l) (return '()))
      ((pair? (car l)) (reverse*-cps (cdr l) (lambda (vcdr)
                                               (return (reverse*-cps (car l) (lambda (vcar)
                                                                               (append-cps vcdr (cons vcar '()) (lambda (v) v))))))))
      (else (reverse*-cps (cdr l) (lambda (v)
                                    (return (append-cps v (cons (car l) '()) (lambda (v) v)))))))))
        

; vectormult takes a row vector (a list of numbers) and matrix (a list of lists
; of numbers) and multiplies the vector times the matrix. The result is a vector
; where the ith element of the result is the dotproduct of the input vector and
; the ith column of the matrix. You can assume that the length of the vector
; matches the number of rows of the matrix.

; initial return function: (lambda (v) v)
(define vectormult-cps
  (lambda (vec m return)
    (if (null? (car m))
        (return '())
        (vectormult-cps vec
                        (rests-cps m (lambda (v1) v1))
                        (lambda (v) (dotproduct-cps vec (firsts-cps m (lambda (v2) v2))
                                                    (lambda (v3) (return (append-cps (cons v3 '()) v (lambda (v4) v4))))))))))

(define firsts-cps
  (lambda (l return)
    (if (null? l)
        (return '())
        (firsts-cps (cdr l) (lambda (v) (return (append-cps (cons (car (car l)) '()) v (lambda (v2) v2))))))))
        
(define rests-cps
  (lambda (l return)
    (if (null? l)
        (return '())
        (rests-cps (cdr l) (lambda (v) (return (cons (cdr (car l)) v)))))))


; matrixmultiply takes two matrices (a list of lists of numbers) and multiplies
; them. You can assume the number of columns of the first matrix is equal to the
; number of rows of the second matrix.
; in the same sublist

; initial return function: (lambda (v) v)
(define matrixmultiply-cps
  (lambda (m1 m2 return)
    (if (null? m1)
        (return '())
        (matrixmultiply-cps (cdr m1) m2 (lambda (v)
                                          (return (vectormult-cps (car m1) m2
                                                          (lambda (v2) (cons v2 v)))))))))


; removesubsequence* takes a list of atoms and a general list. The first list is
; a subsequence of the second list. The method should return the second list with
; the first occurence of the subsequence removed. So, if the first list is '(a b
; c), the first a if the second list is removed, the first b that appears after
; the removed a is removed, and the first c that appears after the removed b is
; removed - no matter how deep the atoms are nested.

; initial return function: (lambda (v1 v2) v2) to keep track of both the
; lat-sequence and the list between recursive calls
(define removesubsequence*-cps
  (lambda (lat l return)
    (cond
      ((or (null? l) (null? lat)) (return lat l))
      ((list? (car l)) (removesubsequence*-cps lat (car l) (lambda (v1 v2)
                                                            (return v1 (removesubsequence*-cps v1 (cdr l) (lambda (v3 v4) (cons v2 v4)))))))
      ((eq? (car l) (car lat)) (removesubsequence*-cps (cdr lat) (cdr l) return))
      (else (removesubsequence*-cps lat (cdr l) (lambda (v1 v2) (return v1 (cons (car l) v2))))))))


; The function suffix takes an atom and a list and returns a list containing all
; elements that occur after the last occurrence of the atom.

(define suffix
  (lambda (a l) ))