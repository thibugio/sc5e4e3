(define dotproduct-cps
  (lambda (v1 v2 return)
    (if (or (null? v1) (null? v2))
        (return 0)
        (dotproduct-cps (cdr v1) (cdr v2) (lambda (v) (return (+ (* (car v1) (car v2)) v)))))))

; replace every occurrence of old with new
(define replace*
  (lambda (old new list return)
    (cond
      ((null? list) (return '()))
      ((pair? (car list)) (replace* old new (car list) 
                                    (lambda (v1) (replace* old new (cdr list)
                                                           (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car list) old) (replace* old new (cdr list)
                                      (lambda (v) (return (cons new v)))))
      (else (replace* old new (cdr list) (lambda (v) (return (cons (car list) v))))))))

; reverse* needs append
(define append-cps
  (lambda (list1 list2 return)
    (if (null? list1)
        (return list2)
        (append-cps (cdr list1) list2 (lambda (v) (return (cons (car list1) v)))))))

(define reverse*-cps
  (lambda (list return)
    (cond 
      ((null? list) (return list))
      ((pair? (car list)) (reverse*-cps (car list)
                                        (lambda (v1) (reverse*-cps (cdr list)
                                                                   (lambda (v2) (append-cps v2 (cons v1 '()) return))))))
      (else (reverse*-cps (cdr list) 
                          (lambda (v) (append-cps v (cons (car list) '()) return)))))))

; vectormult: needs firstcolumn needs restcolumns
(define firstcolumn-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (firstcolumn-cps (cdr m) (lambda (v) (return (cons (caar m) v)))))))

(define restcolumns-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (restcolumns-cps (cdr m) (lambda (v) (return (cons (cdar m) v)))))))

(define vectormult-cps
  (lambda (v m return)
    (if (or (null? m) (null? (car m)))
        (return '())
        (firstcolumn-cps m 
                         (lambda (v1)
                           (dotproduct-cps v v1
                                           (lambda (v2)
                                             (restcolumns-cps m
                                                          (lambda (v3)
                                                            (vectormult-cps v v3
                                                                         (lambda (v4)
                                                                           (return (cons v2 v4)))))))))))))
   ;     (cons (dotproduct v (firstcolumn m)) (vectormult v (restcolumns m))))))

(define matrixmultiply-cps
  (lambda (m1 m2 return)
    (if (null? m1)
        (return '())
        (vectormult-cps (car m1) m2
                        (lambda (v1)
                          (matrixmultiply-cps (cdr m1) m2
                                              (lambda (v2)
                                                (return (cons v1 v2)))))))))

; removesubsequence* must be called with (lambda (v1 v2) v2)
;  v1 is the subsequence v2 is the list
(define removesubsequence*-cps
  (lambda (seq list return)
    (cond
      ((or (null? list) (null? seq)) (return seq list))
      ((pair? (car list)) (removesubsequence*-cps seq (car list)
                                                  (lambda (s1 l1)
                                                    (removesubsequence*-cps s1 (cdr list)
                                                                            (lambda (s2 l2) (return s2 (cons l1 l2)))))))
      ((eq? (car seq) (car list)) (removesubsequence*-cps (cdr seq) (cdr list) return))
      (else (removesubsequence*-cps seq (cdr list) 
                                    (lambda (v1 v2) 
                                      (return v1 (cons (car list) v2))))))))

; suffix
(define suffix2
  (lambda (x list)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (l)
                        (cond
                          ((null? l) '())
                          ((eq? (car l) x) (break (loop (cdr l))))
                          (else (cons (car l) (loop (cdr l))))))))
         (loop list))))
    