; core functions for M_state

; define program state as S = ((x, y, z, ...) (1, 2, 'Undefined, ...)) => x:1, y:2, z:'Undefined
; where the first sublist records all variables declared in the program (no support for scoping), 
; and the second sublist records the value of the declared variables (in-order element-wise correspondence)

(define empty-state '())

; return the first variable in the state
(define state-peek-var caar)
; return the first value in the state
(define state-peek-val cadr)
; return all but the first variable in the state
(define state-rest-vars cdar)
; return all but the first value in the state
(define state-rest-vals cddr)
; return all variables in the state
(define state-all-vars car)
; return all values in the state
(define state-all-vals cdr)

; remove the first (variable, value) in the state and return the new state
(define state-pop 
    (lambda (state)
        (cond
            ((eq? state empty-state) empty-state)
            ((null? (state-rest-vars state)) empty-state)
            (else (cons (state-rest-vars state)
                        (state-rest-vals state))))))

; add a new (variable, value) to the state and return the new state
(define state-push 
    (lambda (variable value state)
        (cond 
            ((eq? state empty-state) (cons (cons variable '()) (cons value '())))
            (else (cons (cons variable (state-all-vars state)) (cons value (state-all-vals state)))))))


; add a variable binding to the given state
; return the new state
(define state-add
    (lambda (variable value state)    
        (cond
            ((state-lookup? variable state) (state-add variable value (state-remove variable state)))
            (else (state-push variable value state)))))

; remove a variable binding from the given state
; return the new state
(define state-remove
    (lambda (variable state)
        (cond
            ((eq? state empty-state) empty-state)
            ((eq? variable (state-peek-var state)) (state-pop state))
            (else (state-add (state-peek-var state) (state-peek-val state) (state-remove variable (state-pop state)))))))

; check whether a given variable is already recorded in the given state
; return true/false
(define state-lookup?
    (lambda (variable state)
        (cond
            ((eq? state empty-state) #f)
            (else (or (eq? variable (state-peek-var state))
                       (state-lookup? variable (state-pop state)))))))

