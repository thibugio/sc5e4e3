; collection of functions used for M_state
; decision: side effects will be allowed

(load "mStateCore.scm")
(load "mValue.scm")
(load "mBool.scm")
(load "mName.scm")

(define get-operator car)
(define get-operand1 cadr)
(define get-operand2 caddr)

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
; Mstate ('= variable expression', state) = {
;       if lookup(variable, state) = true
;           return add(Mname(variable, state), Mvalue(expression, state), remove(variable, Mstate(expression, state)))
;       else
;           error: 'variable has not been declared'
; }
(define m-state-assign
    (lambda (variable expression state)
        (if (state-lookup? variable state)
           (state-add variable (m-value expression state) (state-remove variable (m-state expression state)))
           (error 'Unknown "Mstate_assign: variable has not been declared"))))

; denotational semantics of if-statement
; Mstate (if condition then-statement optional-else-statement) = {
;       if Mbool(condition, Mstate(condition, state))
;           return Mstate(then-statement, Mstate(condition, state))
;       else
;           if optional-else-statement != '()
;               return Mstate(optional-else-statement, Mstate(condition, state))
;           else
;               return state
; }
(define m-state-if
    (lambda (condition then-statement optional-else-statement state)
        (cond 
            ((m-bool condition (m-state condition state)) (m-state then-statement (m-state condition state)))
            ((null? optional-else-statement) state)
            (else (m-state optional-else-statement (m-state condition state))))))

; denotational semantics of return-expression
; Mstate ('return <expression>', state) = {
;   return Mstate(<expression>, state)
;}
(define m-state-return
    (lambda (statement state) 
        (state-add 'RetVal (m-value (return-expr statement) state) state)))
(define return-expr cdr)

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

; denotational semantics of variable-declaration option 1
; Mstate(var <variable>, S) = {
;   if (lookup <variable>, S) = true
;     return 'error
;   return (add (<variable>, 'Undefined), S) // a new state
; }
(define m-state-var
    (lambda (variable state)
        (if (state-lookup? variable state)
            (error 'Mstate_var "Variable has already been declared")
            (state-add variable varInitVal state))))
 
; denotational semantics of variable-declaration option 2
; Mstate(var <variable> = <value>) = {
;   if (lookup <variable>, S) = true
;     return 'error
;   return (add (<variable>, (Mvalue <value>, S)), S) // a new state
; }
(define m-state-var-value
    (lambda (variable value state)
        (if (state-lookup? variable state)
            (error 'Mstate_var_value "Variable has already been declared")
            (state-add variable (m-value value state) (m-state value state)))))


