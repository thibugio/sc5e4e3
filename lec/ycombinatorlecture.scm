; rewrite some of our favorite functions using the idea of the Y-combinator
;  (this is a generalization of recursion where we do not need to have a recursive
;  binding of the function body to the function name)

; factorial
(define factorial
  ((lambda (m)
     (m m))
   (lambda (f)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((f f) (- n 1))))))))

; dotproduct
(define dotproduct
  ((lambda (m)
     (m m))
   (lambda (f)
     (lambda (v1 v2)
       (cond
         ((null? v1) 0)
         ((null? v2) 0)
         (else (+ (* (car v1) (car v2)) ((f f) (cdr v1) (cdr v2)))))))))

; append
(define myappend
  ((lambda (m)
     (m m))
   (lambda (f)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((f f) (cdr l1) l2)))))))

; sumnumbers*
(define sumnumbers*
  ((lambda (m)
     (m m))
   (lambda (f)
     (lambda (l)
       (cond
         ((null? l) 0)
         ((number? (car l)) (+ (car l) ((f f) (cdr l))))
         ((list? (car l)) (+ ((f f) (car l)) ((f f) (cdr l))))
         (else ((f f) (cdr l))))))))

; flatten
(define flatten
  ((lambda (m n)
     (m m n))
   (lambda (f a)
     (lambda (l)
       (cond
         ((null? l) '())
         ((list? (car l)) ((a a) ((f f a) (car l)) ((f f a) (cdr l))))
         (else (cons (car l) ((f f a) (cdr l)))))))
   (lambda (a)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((a a) (cdr l1) l2)))))))

; flatten (cps)
(define flatten
  (lambda (l)
    (((lambda (m n)
        (m m n))
      (...)  ; here is the cps version of the above flatten
      ) l (lambda (v) v))))

; vectormultiply
(define vectormultiply
  ((lambda (m n o p)
     (m m n o p))
   (lambda ...   ) ; the vector multiply function (m above)
   (lambda ...   ) ; the dot product function (n above)
   (lambda ...   ) ; the first column function (o above)
   (lambda ...   ) ; the rest of columns function (p above)
    ))