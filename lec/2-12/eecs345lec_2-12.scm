(define multiply-cps
  (lambda (l return break) ;return immediately on '0'
    (cond
      ((null? l) (return 1))
      ((zero? (car l)) (break 0))
      (else (multiply-cps (cdr l) (lambda (v) (return (* (car l) v))) break)))))

; call-with-current-continuation (built-in function, abbrev. call/cc)
; 'break' is a continuation to the execution at exactly the spot that c-w-c-c is invoked
; pass in a continuation that can jump out of the function execution

(define call-multiply-with-break
  (lambda (l)
    ; when break is called, jump right to this spot
    (call-with-current-continuation ;pass the continuation as input to this function
     (lambda (break)
       (multiply l break)))))

(define multiply
  (lambda (l break)
    (cond
      ((null? l) 1)
      ((zero? (car l)) (break 0)) 
      (else (* (car l) (multiply (cdr l) break))))))

; bad way to do call/cc
(define badmultiply
  (lambda (l)
    (call/cc
     (lambda (break)
       (cond
         ((null? l) 1)
         ; creates a new continuation to each spot in the call stack,
         ; so returns '0' to current spot in the code and still has to run down the call stack,
         ; so it's as if there was no call/cc
         ((zero? (car l)) (break 0)) 
         (else (+ (car l) (badmultiply (cdr l)))))))))

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

; continuations facilitate doing things with recursion that normally aren't easy
; (pull-out-numbers '(a 1 2 b 3 c) return) => '((1 2 3) (a b c))
; initial return: (lambda (v) v)
(define pull-out-numbers
  (lambda (l return)
    (cond
      ((null? l) (return '(()()) ))
      ((number? (car l)) (pull-out-numbers (cdr l) (lambda (v) (return (cons (cons (car l) (car v))
                                                                             (cdr v))))))
      (else (pull-out-numbers (cdr l) (lambda (v) (return (cons (car v)
                                                                (cons (cons (car l) (car (cdr v))) '())))))))))

; (pull-out-numbers2 '(a 1 2 b 3) return) => '(1 2 3 a b)
; initial return: (lambda (v1 v2) (cons v1 (cons v2 '()))) for pull-out-numbers1, or
;                 (lambda (v1 v2) (append v1 v2)) for pull-out-numbers2
(define pull-out-numbers2
  (lambda (l return)
    (cond
      ((null? l) (return '() '()))
      ((number? (car l)) (pull-out-numbers2 (cdr l) (lambda (v1 v2) (return (cons (car l) v1) v2))))
      (else (pull-out-numbers2 (cdr l) (lambda (v1 v2) (return v1 (cons (car l) v2))))))))

; benefits of continuation-passing style:
;1. arbitrary number of recursive calls
;2. can break out of execution and throw away call stack
;3. more flexibility with function