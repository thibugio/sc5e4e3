; Y-combinators

(define dotproduct
  ((lambda (m) (m m))
   (lambda (func)
     (lambda (v1 v2)
       (if (or (null? v1) (null? v2))
           0
           (+ (* (car v1) (car v2)) ((func func) (cdr v1) (cdr v2))))))))


(define myappend
  ((lambda (m) (m m))
   (lambda (func)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((func func) (cdr l1) l2)))))))


(define sumnumbers*
  ((lambda (m) (m m))
   (lambda (f)
     (lambda (l ret)
       (cond
         ((null? l) (ret 0))
         ((number? (car l)) ((f f) (cdr l) (lambda (v) (ret (+ v (car l))))))
         ((list? (car l)) ((f f) (cdr l) (lambda (v) ((f f) (car l) (lambda (w) (ret (+ w v)))))))
         (else ((f f) (cdr l) ret)))))))



(define flatten
  ((lambda (m n)
     (m m n))
   (lambda (f g)
     (lambda (l)
       (cond
         ((null? l) '())
         ((list? (car l)) ((g g) ((f f g) (car l)) ((f f g) (cdr l))))
         (else (cons (car l) ((f f g) (cdr l)))))))
   (lambda (f)
     (lambda (l1 l2)
       (if (null? l1)
           l2
           (cons (car l1) ((f f) (cdr l1) l2)))))))


(define flatten-cps
  (lambda (l)
    (((lambda (m n) (m m n))
(lambda (f a)
  (lambda (l ret)
    (cond
      ((null? l) (ret '()))
      ((list? (car l)) ((f f a) (cdr l) (lambda (v1)
                                          ((f f a) (car l) (lambda (v2)
                                                             (ret ((a a) v2 v1)))))))
      (else ())
                  ))
      ) l (lambda (v) v))))


(define vectormultiply
  ((lambda (m n o p q)
     (m m n o p q))
   (lambda (v a d h r)
     (lambda (vec mat)
       (if (null? (car mat))
           '()
           