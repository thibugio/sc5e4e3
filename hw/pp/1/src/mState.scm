; collection of functions used for M_state

; M_state function
; takes a statment and a state and updates the state by evaluating the statement
; we can assume that the statement will not be empty
(define m-state
    (lambda (statement state)
        (cond
            ((eq? 'var (statement-type statement)) (m-state-var statement state))
            ((eq? '= (statement-type statement)) (m-state-assign statement state))
            ((eq? 'if (statement-type statement)) (m-state-if statement state))
            ((eq? 'while (statement-type statement)) (m-state-while statement state))
            ((eq? 'return (statement-type statement)) (m-state-return statement state))
            (else (error 'unknown "Unknown Statement Type")))))


; denotational semantics of variable-assignment
; Mstate (= variable expression) = {
; 
; }
(define m-state-assign
    (lambda (variable expression state)
        (cond )))

; denotational semantics of if-statement
; Mstate (if condition then-statement optional-else-statement) = {
;
; }
(define m-state-if
    (lambda (condition then-statement optional-else-statement)
        (cond )))

; denotational semantics of return-expression
; Mstate (return expression) = {
; 
; }
(define m-state-return
    (lambda (expression) 
        (cond )))

; denotational semantics of while-loop
; Mstate (while <cond> <stmt>, S) = {
;   if Mbool (<cond>, S) = true
;     Mstate (while <cond> <stmt>, Mstate (<stmt>, S))
;   else
;     S
; }
(define m-state-while
  (lambda (condition statement state)
    (if (m-bool condition state)
        (m-state-while condition statement (m-state statement state))
        state)))

; denotational semantics of variable-declaration
; Mstate(var <variable>, S) = {
;   if (lookup <variable>, S) = true
;     return 'error
;   return (add (<variable>, 'Undefined), S) // a new state
; }
; 
; Mstate(var <variable> <value>) = {
;   if (lookup <variable>, S) = true
;     return 'error
;   return (add (<variable>, (Mvalue <value>, S)), S) // a new state
; }
(define m-state-var
    (lambda (var)
        (cond )))
(define m-state-var-value
    (lambda (var value)
        (cond )))


