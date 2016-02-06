#lang racket

(define M_value
  (lambda (exp)
    (cond
      ((number? exp) exp)
      ((eq? '+ (operator exp)) (+ (M_value (operand1 exp)) (M_value (operand2 exp))))
      ((eq? '- (operator exp)) (- (M_value (operand1 exp)) (M_value (operand2 exp))))
      ((eq? '* (operator exp)) (* (M_value (operand1 exp)) (M_value (operand2 exp))))
      ((eq? '/ (operator exp)) (quotient (M_value (operand1 exp)) (M_value (operand2 exp))))
      ((eq? '% (operator exp)) (remainder (M_value (operand1 exp)) (M_value (operand2 exp))))
      (else (error 'unknown "unknown expression")))))

;; prefix notation
(define operand1 cadr)

(define operand2 caddr)

(define operator car)

; evaluate a single expression
(define evaluate
  (lambda (exp)
    (cond
    ((null? exp) exp)
    ((eq? '+ (car (cdr exp))) (+ (car exp) (car (cdr (cdr exp)))))
    ((eq? '- (car (cdr exp))) (- (car exp) (car (cdr (cdr exp)))))
    ((eq? '* (car (cdr exp))) (* (car exp) (car (cdr (cdr exp)))))
    ((eq? '/ (car (cdr exp))) (/ (car exp) (car (cdr (cdr exp)))))
    ((eq? '% (car (cdr exp))) (mod (car exp) (car (cdr (cdr exp)))))
    (else (error 'error "unknown expression")))))

(define mod
  (lambda (a b)
    (cond
      ((eq? a b) 0)
      ((< a b) a)
      (else (mod (- a b) b)))))

; denotational semantics
; Mstate (while <cond> <stmt>, S) = {
;   if Mbool (<cond>, S) = true
;     Mstate (while <cond> <stmt>, Mstate (<stmt>, S))
;   else
;     S
;}

(define M_state_while
  (lambda (condition statement state)
    (if (M_bool condition state)
        (M_state_while condition statement (M_state statement state))
        state)))

(define M_state
  (lambda (statement state) ))

(define M_bool
  (lambda (condition state) ))