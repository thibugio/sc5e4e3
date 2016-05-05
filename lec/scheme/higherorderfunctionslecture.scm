(define vectorMultiply
  (lambda (v m)
    (if (or (null? m) (null? (car m)))
        '()
        (cons (dotproduct v (firstColumn m)) (vectorMultiply v (restOfColumns m))))))

(define dotproduct
  (lambda (v1 v2)
    (if (or (null? v1) (null? v2))
        0
        (+ (* (car v1) (car v2)) (dotproduct (cdr v1) (cdr v2))))))

(define dotproduct
  (lambda (v1 v2)
    (apply + (map * v1 v2))))

(define firstColumn
  (lambda (m)
    (map car m)))

(define restOfColumns
  (lambda (m)
    (map cdr m)))

(define foldright
  (lambda (f i l)
    (if (null? l)
        i
        (f (car l) (foldright f i (cdr l))))))

(define foldleft
  (lambda (f i l)
    (if (null? l)
        i
        (foldleft f (f (car l) i) (cdr l)))))

(define foldleft-real
  (lambda (f i l)
    (if (null? l)
        i
        (foldleft-real f (f i (car l)) (cdr l)))))

(define maximum
  (lambda (l)
    (foldl (lambda (x y) (if (> x y) x y)) (car l) (cdr l))))

(define qsort
  (lambda (l)
    (if (or (null? l) (null? (cdr l)))
        l
        (append (qsort (filter (lambda (x) (< x (car l))) l))
                (filter (lambda (x) (= x (car l))) l)
                (qsort (filter (lambda (x) (> x (car l))) l))))))
