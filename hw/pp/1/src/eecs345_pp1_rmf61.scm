; Rebecca Frederick
; EECS 345
; Programming Project 1

(load "../external/simpleParser.scm")

; takes a filename, calls parser with the filename, evaluates the parse tree
; returned by parser, and returns the proper value of the program state 
; (binding pairs)
(define interpret
    (lambda (f)
        (state (parser f))))

; takes a parse tree and returns the program state
; Formally, a parse tree is a list where each sublist corresponds to a statement.
(define state
    (lambda (tree)
        (state-acc tree '() )))

; accumulate the program state
(define state-acc
    (lambda (tree state)
        (cond
            ((null? (car tree)) state )
            (else (cons (m-state (car tree) state) (state-acc (cdr tree state)))))))

; M_state function
; takes a statment and a state and updates the state by evaluating the statement
; we can assume that the statement will not be empty
(define m-state
    (lambda (statement state)
        (cond
            ((eq? 'var (statement-type statement)) (eval-var statement state))
            ((eq? '= (statement-type statement)) (eval-assign statement state))
            ((eq? 'if (statement-tyep statement)) (eval-cond statement state))
            ((eq? 'while (statement-type statement)) (eval-loop statement state))
            ((eq? 'return (statement-type statement)) (eval-return statement state))
            (else (error 'unknown "Unknown Statement Type")))))

; M_value function
; takes a statment and a state and returns the value of executing the statement
(define m-value
    (lambda (statement state)
        (cond )))

; evaluate a variable declaration
(define eval-var 
    (lambda (statement state)
        (cond )))

; evaluate a variable assignment
(define eval-assign 
    (lambda (statement state)
        (cond )))

; evaluate a conditional (if-else) statement
(define eval-cond
    (lambda (statement state)
        (cond )))

; evaluate a loop (while) statement
(define eval-loop
    (lambda (statement state) 
        (cond )))

; evaluate a return expression
(define eval-return
    (lambda (statement state)
        (cond )))

; evaulate a primitive (mathematical) operation
; input: a list (<op> <value1> <value2>)
(define eval-primitive-op
    (lambda (statement state)
        (cond )))

; test whether a symbol is a primitive (mathematical) operator
(define is-primitive-op?
    (lambda (sym)
        (cond
            ((eq? '+ sym) #t)
            ((eq? '- sym) #t)
            ((eq? '* sym) #t)
            ((eq? '/ sym) #t)
            ((eq? '% sym) #t)
            (else #f))))

; test whether a symbol is a comparison operator
(define is-comparison-op?
    (lambda (s)
        (cond
            ((eq? '== s) #t)
            ((eq? '!= s) #t)
            ((eq? '>= s) #t)
            ((eq? '<= s) #t)
            ((eq? '> s) #t)
            ((eq? '< s) #t)
            (else #f))))

; test whether a symbol is a boolean operator
(define is-boolean-op?
    (lambda (s)
        ((eq? '&& s) #t)
        ((eq? '|| s) #t)
        ((eq? '! s) #t)
        (else #f)))

; extract the type of the statement (i.e., 'while', 'if',...)
(define statement-type car)
