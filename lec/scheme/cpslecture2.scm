; my-append will append two lists
(define my-append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (my-append (cdr l1) l2)))))

(define my-append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (my-append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

; (length-cps '(a b c d) (lambda (v) v))  => 4
(define mylength
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (mylength (cdr l))))))

(define length-cps
  (lambda (l return)
    (if (null? l)
        (return 0)
        (length-cps (cdr l) (lambda (v) (return (+ 1 v)))))))

; (sumnumbers*-cps '(a 2 (3 4) ((5))) (lambda (v) v))  => 14
(define sumnumbers*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l) (sumnumbers* (cdr l))))
      ((list? (car l)) (+ (sumnumbers* (car l)) (sumnumbers* (cdr l))))
      (else (sumnumbers* (cdr l))))))

(define sumnumbers*-cps
  (lambda (l return)
    (cond
      ((null? l) (return 0))
      ((number? (car l)) (sumnumbers*-cps (cdr l) (lambda (v) (return (+ v (car l))))))
      ((list? (car l)) 
       (sumnumbers*-cps (car l) 
                        (lambda (v1) (sumnumbers*-cps (cdr l)
                                                      (lambda (v2) (return (+ v1 v2)))))))
      (else (sumnumbers*-cps (cdr l) return)))))

; (flatten-cps '(((a)) b ((c d)) e) (lambda (v) v))  => '(a b c d e) 
(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))

(define flatten-cps
  (lambda (l return)
    (cond
      ((null? l) (return '()))
      ((list? (car l)) (flatten-cps (car l) (lambda (v1) (flatten-cps (cdr l) (lambda (v2) (my-append-cps v1 v2 return))))))
      (else (flatten-cps (cdr l) (lambda (v) (return (cons (car l) v))))))))

; multiply.  An example of using continuations to immediately jump out of a calculation
(define multiply
  (lambda (l)
    (cond 
      ((null? l) 1)
      ((zero? (car l)) 0)
      (else (* (car l) (multiply (cdr l)))))))

(define multiply-cps
  (lambda (l return break)
    (cond
      ((null? l) (return 1))
      ((zero? (car l)) (break 0))
      (else (multiply-cps (cdr l) (lambda (v) (return (* v (car l)))) break)))))
