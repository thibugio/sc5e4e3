; higher-order functions: functions that manipulate other functions

; examples:
; 1. continuations
; 2. map: takes a function and applies it to multiple inputs
(map (lambda (x) (+ 1 x)) '(1 2 3 4 5))
(map (lambda (x y) (+ x y)) '(1 2 3 4 5) '(10 11 12 13 14))
; 3. apply: applies the function to everything in the list (left to right).
; Apply is similar to fold-right, but without an initial value.
(apply + '(1 2 3 4 5))
; 4. fold-right: takes a function, an initial value, and a list.
; takes the operations and moves them left: (1 - (2 - (3 - 0))) == (- 1 (- 2 (- 3 0)))
(foldr - 0 '(1 10))
(foldr cons '() '(a b c d))
; 5. fold-left (nb: these are opposite of Haskell)
; takes the operation and moves it right: (- (- (- 0 1) 2) 3)
(foldl - 0 '(1 10))


(define vectormultiply
  (lambda (v m)
    (if (or (null? m) (null? (car m)))
        '()
        (cons (dotproduct v (firstcolumn m)) (vectormultiply v (restcolumns m))))))
(define dotproduct
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        0
        (+ (* (car v1) (car v2)) (dotproduct (cdr v1) (cdr v2))))))
(define firstcolumn
  (lambda (m)
    (if (null? m)
        '()
        (cons (caar m) (firstcolumn (cdr m))))))
(define restcolumns
  (lambda (m)
    (if (null? m)
        '()
        (cons (cdr (car m)) (restcolumns (cdr m))))))

; re-write firstcolumn and restcolumns using map:
(map (lambda (m) (car m)) '((1 2 3) (4 5 6) (7 8 9))); equivalently, (map car m)
(map (lambda (m) (cdr m)) '((1 2 3) (4 5 6) (7 8 9)))
(define firstcolumn
  (lambda (m) (map car m)))
(define restcolumns
  (lambda (m) (map cdr m)))
(define dotproduct
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        0
        (apply + (map * v1 v2))))) ; multiply elements pair-wise, then take the sum

; implementing fold-left
; (foldleft - 0 '(1 2 3)) => (- (- (- 0 1) 2) 3)
(define foldleft
  (lambda (f i l)
    (if (null? l)
        i
        (f i (foldleft f (car l) (cdr l))))))
; equivalently:
(define foldleft2
  (lambda (f i l)
    (if (null? l)
        i
        (foldleft2 f (f (car l) i) (cdr l)))))

; implementing fold-right
; (foldright - 0 '(1 2 3)) => (- 1 (- 2 (- 3 0)))
(define foldright
  (lambda (f i l)
    (if (null? l)
        i
        (f (car l) (foldright f i (cdr l))))))

; return the largest number in the list.
; assume at least 1 element in the list.
(define maximum
  (lambda (l)
    (foldl (lambda (n1 n2) (if (> n1 n2) n1 n2)) (car l) (cdr l))))

(define qsort
  (lambda (l)
    (if (or (null? l) (null? (cdr l)))
        l
        (append (qsort (filter (lambda (x) (< x (car l))) l))
                (filter (lambda (x) (= x (car l))) l)
                (qsort (filter (lambda (x) (> x (car l))) l))))))