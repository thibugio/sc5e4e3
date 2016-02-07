; collection of functions used for M_value

; M_value function
; takes a statment and a state and returns the value of executing the statement
(define m-value
    (lambda (statement state)
        (cond )))

; evaluate a variable declaration
(define m-value-var 
    (lambda (statement state)
        (cond )))

; evaluate a variable assignment
(define m-value-assign 
    (lambda (statement state)
        (cond )))

; evaluate a conditional (if-else) statement
(define m-value-if
    (lambda (statement state)
        (cond )))

; evaluate a loop (while) statement
(define m-value-while
    (lambda (statement state) 
        (cond )))

; evaluate a return expression
(define m-value-return
    (lambda (statement state)
        (cond )))

; evaulate a primitive (mathematical) operation
; input: a list (<op> <value1> <value2>)
(define m-value-primitive-op
    (lambda (statement state)
        (cond )))


