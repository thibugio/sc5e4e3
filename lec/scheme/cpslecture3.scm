; The multiply from last lecture.  Two continuations, the normal return and an immediate break
; Call with multiply-cps list (lambda (v) v) (lambda (v) v))
(define multiply-cps
  (lambda (l return break)
    (cond
      ((null? l) (return 1))
      ((zero? (car l)) (break 0))
      (else (multiply-cps (cdr l) (lambda (v) (return (* (car l) v))) break)))))

; A "normal" recursive multiply that includes a break continuation
(define multiply
  (lambda (l break)
    (cond 
      ((null? l) 1)
      ((zero? (car l)) (break 0))
      (else (+ (car l) (multiply (cdr l) break))))))

; Here we create the break continuation for multiply so that it returns immediately
; to be the return value of call-with-current-continuation
(define call-multiply-with-break
  (lambda (l)
    (call-with-current-continuation
     (lambda (break)
       (multiply l break)))))

; an incorrect use of call/cc.  A new break continuation is created with each recursive
; call so that the break just returns to the same point in the code.
(define badmultiply
  (lambda (l)
    (call/cc
     (lambda (break)
       (cond
         ((null? l) 1)
         ((zero? (car l)) (break 0))
         (else (+ (car l) (badmultiply (cdr l)))))))))

; a better use of call/cc.  Now there is a single break that is outside of loop.
(define goodmultiply
  (lambda (l)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (l)
                        (cond
                          ((null? l) 1)
                          ((zero? (car l)) (break 0))
                          (else (+ (car l) (loop (cdr l))))))))
         (loop l))))))

; pull-out-numbers separates the numbers from the non-numbers in a list.
; An example of the power of continuation passing style!
; (pull-out-numbers '(a 1 2 b 3 c 4 5) (lambda (v1 v2) (cons v1 (list v2)))
;                                 => '((1 2 3 4 5) (a b c))
; (pull-out-numbers '(a 1 2 b 3 c 4 5) (lambda (v1 v2) (append v1 v2))) 
;                                 => '(1 2 3 4 5 a b c)
(define pull-out-numbers
  (lambda (l return)
    (cond
      ((null? l) (return '() '()))
      ((number? (car l)) (pull-out-numbers (cdr l) 
                                           (lambda (v1 v2) 
                                             (return (cons (car l) v1) v2))))
      (else (pull-out-numbers (cdr l) 
                              (lambda (v1 v2)
                                (return v1 (cons (car l) v2))))))))