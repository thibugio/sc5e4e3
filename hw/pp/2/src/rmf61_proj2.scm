; Rebecca Frederick
; EECS 345
; Programming Project 2


(load "simpleParser.scm")


;======================================
; main interpreter function

; takes a filename, calls parser with the filename, evaluates the parse tree
; returned by parser, and returns the program state 
(define interpret
    (lambda (f)
        (run-state (parser f) empty-state)))

; takes a parse tree 't' and initial state 's' and returns the program state
; Formally, a parse tree is a list where each sublist corresponds to a statement.
(define run-state
    (lambda (t s)
        (if (null? t)
            s
            (run-state (cdr t) (m-state (car t) s)))))

(define empty-state '())
(define varInitVal 'Undefined)
(define progRetVal '$v0)

(define parsetree-pop cdr)
(define parsetree-peek car)

(define get-operator car)
(define get-operand1 cadr)
(define get-operand2 caddr)

;===================================
; basic state manipulation functions

; define program state as S = (((z, ...) (varInitVal, ...)) ((x, y, ...) (1, 2, ...))) =>
;   x:1:0, y:2:0, z:'Undefined:1,
; where the first inner list records all variables declared in the program, 
; the second inner list records the value of the declared variables (in-order correspondence),
; and the outer list represents scope

; return the first scope in the state ( ->( (x,y,z) (1,2,3) )  ( (q,r,s) (4,5,6) )  )
(define state-peek-scope car)
; return the first variable in the state
(define state-peek-var (lambda (s) (caar (state-peek-scope s))))
; return the first value in the state
(define state-peek-val (lambda (s) (caadr (state-peek-scope s))))

; return all but the first scope in the state
(define state-rest-scope cdr) ; (cons empty-state '()) in null case?
; return a list of all but the first variable in the first scope of the state
(define state-rest-vars (lambda (s) (cdar (state-peek-scope s))))
; return a list of all but the first value in the first scope of the state
(define state-rest-vals (lambda (s) (cdadr (state-peek-scope s))))

; return a list of all variables in the first scope of the state
(define state-all-vars (lambda (s) (car (state-peek-scope s))))
; return a list of all values in the first scope of the state
(define state-all-vals (lambda (s) (cadr (state-peek-scope s))))

; remove the first (variable, value) in the first scope of the state and return the new state
(define state-pop-binding 
    (lambda (state)
        (cond
            ((eq? state empty-state) empty-state)
            ((null? (state-rest-vars state)) (state-rest-scope state))
            (else (cons (cons (state-rest-vars state)
                              (cons (state-rest-vals state) '()))
                        (state-rest-scope state))))))

; add a new (variable, value) to the first scope of the state and return the new state
(define state-push-binding 
    (lambda (variable value state)
        (cond 
            ((eq? state empty-state) (cons (cons (cons variable '()) (cons (cons value '()) '())) '()))
            (else (cons (cons (cons variable (state-all-vars state))
                              (cons (cons value (state-all-vals state)) '()))
                        (state-rest-scope state))))))

; add a new scope to the state and return the new state
(define state-add-scope
  (lambda (state)
    (cons '() state)))

; add a variable binding to the first scope of the state and return the new state
(define state-add-binding
    (lambda (variable value state)    
        (cond
            ((state-lookup? variable state) (state-add-binding variable value (state-rem-binding variable state)))
            (else (state-push-binding variable value state)))))

; remove a variable binding from the first scope of the state and return the new state
(define state-rem-binding
    (lambda (variable state)
        (cond
            ((eq? state empty-state) empty-state)
            ((eq? variable (state-peek-var state)) (state-pop-binding state))
            (else (state-add-binding (state-peek-var state) (state-peek-val state) (state-rem-binding variable (state-pop-binding state)))))))

; check whether a given variable is already recorded in the state
; return true/false
(define state-lookup?
    (lambda (variable state)
        (cond
            ((eq? state empty-state) #f)
            ((eq? variable (state-peek-var state)) #t)
            (else (state-lookup? variable (state-pop-binding state))))))

; find the value associated with a variable in the state (starting with inner-most scope)
; return the value of the variable, or an error condition if the variable cannot be found
(define state-get-value
    (lambda (variable state)
        (cond
            ((eq? state empty-state) (error 'state-get-value "Variable not in State"))
            ((eq? variable (state-peek-var state)) (state-peek-val state))
            (else (state-get-value variable (state-pop-binding state))))))

;=====================================
; Mstate 

; main Mstate function
; takes a statment and a state and updates the state by evaluating the statement
; we can assume that the statement will not be empty
(define m-state
    (lambda (statement state)
        (cond
            ((eq? 'var (get-operator statement)) (m-state-var statement state))
            ((eq? '= (get-operator statement)) (m-state-assign statement state))
            ((eq? 'if (get-operator statement)) (m-state-if statement state))
            ((eq? 'while (get-operator statement)) (m-state-while statement state))
            ((eq? 'return (get-operator statement)) (m-state-return statement state))
            ((eq? 'begin (get-operator statement)) (m-state-begin-scope statement state))
            ((eq? 'break (get-operator statement)) (m-state-break statement state))
            ((eq? 'continue (get-operator statement)) (m-state-continue statement state))
            ((eq? 'throw (get-operator statement)) (m-state-throw statement state))
            ((eq? 'try (get-operator statement)) (m-state-tcf statement state))
            (else state))))

(define m-state-break
  (lambda (statement state)
    ))

(define m-state-continue
  (lambda (statement state)
    ))

(define m-state throw
  (lambda (statement state)
    ))

(define m-state-tcf
  (lambda (statement state)
    ))

(define m-state-begin-scope
  (lambda (statement state)
    (m-state (cdr statement) (state-add-scope state))))
    
; denotational semantics of variable-assignment
; Mstate ('= variable expression', state) = {
;       if lookup(variable, state) = true
;           return add(Mname(variable, state), Mvalue(expression, state), remove(variable, Mstate(expression, state)))
;       else
;           error: 'variable has not been declared'
; }
(define m-state-assign
    (lambda (statement state)
        (if (state-lookup? (assign-var statement) state)
           (state-add-binding (assign-var statement)
                      (m-value (assign-expr statement) state)
                      (state-rem-binding (assign-var statement) state))
           (error 'Mstate_assign  "variable has not been declared"))))
(define assign-var cadr)
(define assign-expr caddr)

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
    (lambda (statement state)
        (cond 
            ((m-bool (if-cond statement) (m-state (if-cond statement) state)) (m-state (if-then statement) (m-state (if-cond statement) state)))
            ((eq? 3 (len statement)) state) ; no optional-else-statement
            (else (m-state (if-optelse statement) (m-state (if-cond statement) state))))))
(define if-cond cadr)
(define if-then caddr)
(define if-optelse cadddr)

; denotational semantics of return-expression
; Mstate ('return <expression>', state) = {
;   if (is boolean expression)
;     return Mbool(<expression>, state)
;   else
;     return Mvalue(<expression>, state)
;}
(define m-state-return
    (lambda (statement state) 
        ;(state-add-binding progRetVal (m-value (return-expr statement) state) state)))
        (if (and (list? (return-expr statement))
                 (or (bool-op? (get-operator (return-expr statement)))
                     (comp-op? (get-operator (return-expr statement)))))
            (bool2sym (m-bool (return-expr statement) state))
            (m-value (return-expr statement) state))))
(define return-expr cadr)
(define bool2sym
  (lambda (b)
    (if b
        'true
        'false)))
(define bool-op?
  (lambda (sym)
    (cond
      ((eq? '! sym) #t)
      ((eq? '&& sym) #t)
      ((eq? '|| sym) #t)
      (else #f))))
(define comp-op?
  (lambda (sym)
    (cond
      ((eq? '== sym) #t)
      ((eq? '!= sym) #t)
      ((eq? '>= sym) #t)
      ((eq? '<= sym) #t)
      ((eq? '> sym) #t)
      ((eq? '< sym) #t)
      (else #f))))

; denotational semantics of while-loop
; Mstate(while <cond> <stmt>, S) = {
;   if Mbool(<cond>, Mstate(<cond>, S)) = true
;     Mstate(while <cond> <stmt>, Mstate (<stmt>, Mstate(<cond>, S)))
;   else
;     Mstate(<cond>, S)
; }
(define m-state-while
  (lambda (statement state)
    (if (m-bool (while-cond statement) (m-state (while-cond statement) state))
        (m-state-while statement (m-state (while-stmt statement) (m-state (while-cond statement) state)))
        (m-state (while-cond statement) state))))
(define while-cond cadr)
(define while-stmt caddr)

; denotational semantics of variable-declaration option 1
; Mstate(var <variable> (<value>), S) = {
;   if (lookup <variable>, S) = true
;     return 'error
;   if <value>
;     return add(<variable>, 'Undefined), S) // a new state
;   else
;     return add(<variable>, Mvalue(<value>, S), S)//Mstate(<value>, S))
; }
(define m-state-var
    (lambda (statement state)
      (cond
        ((state-lookup? (var-var statement) state) (error 'Mstate_var "Variable already declared"))
        ((eq? 2 (len statement)) (state-add-binding (var-var statement) varInitVal state)) ; var <variable>;
        ((and (list? (var-value statement))
                 (or (bool-op? (get-operator (var-value statement)))
                     (comp-op? (get-operator (var-value statement))))) (state-add-binding (var-var statement) (bool2sym (m-bool (var-value statement) state)) state))
        (else (state-add-binding (var-var statement) (m-value (var-value statement) state) state)))))
(define var-var cadr)
(define var-value caddr)
(define len
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (len (cdr l))))))

;========================================
; Mbool

; M_bool function
; returns whether the given condition is satisfied in the given state
(define m-bool
    (lambda (condition state)
      (if (list? condition)
        (cond
            ((eq? '==    (get-operator condition)) (eq? (m-value (get-operand1 condition) state) 
                                                        (m-value (get-operand2 condition) state)))
            ((eq? '!=    (get-operator condition)) (not (eq? (m-value (get-operand1 condition) state)
                                                             (m-value (get-operand2 condition) state))))
            ((eq? '>=    (get-operator condition)) (>= (m-value (get-operand1 condition) state)
                                                       (m-value (get-operand2 condition) state)))
            ((eq? '<=    (get-operator condition)) (<= (m-value (get-operand1 condition) state)
                                                       (m-value (get-operand2 condition) state)))
            ((eq? '>     (get-operator condition)) (> (m-value (get-operand1 condition) state)
                                                      (m-value (get-operand2 condition) state)))
            ((eq? '<     (get-operator condition)) (< (m-value (get-operand1 condition) state)
                                                      (m-value (get-operand2 condition) state)))
            ((eq? '&&    (get-operator condition)) (and (m-bool (get-operand1 condition) state)
                                                        (m-bool (get-operand2 condition) state)))
            ((eq? '|| (get-operator condition)) (or (m-bool (get-operand1 condition) state)
                                                       (m-bool (get-operand2 condition) state)))
            ((eq? '!     (get-operator condition)) (not (m-bool (get-operand1 condition) state)))
            (else (error 'Mbool "Unknown boolean or comparison operator")))
        (cond
          ((number? condition) (error 'Mbool "Type Error: Number != Boolean"))
          ((eq? 'true condition) #t)
          ((eq? 'false condition) #f)
          ((state-lookup? condition state) (m-bool (state-get-value condition state) state))
          (else (error 'Mbool "Unknown atom"))))))
        
;========================================
; Mvalue 

; Mvalue takes a syntax rule and a state, and returns a numeric value (or error condition)
(define m-value
    (lambda (statement state)
      (if (list? statement)
       (cond
            ((and (eq? 2 (len statement))
                  (eq? '- (get-operator statement))) (* -1 (m-value (get-operand1 statement) state))) ; unary operator
            ((eq? '+ (get-operator statement)) (+ (m-value (get-operand1 statement) state) 
                                                  (m-value (get-operand2 statement) state)))
            ((eq? '- (get-operator statement)) (- (m-value (get-operand1 statement) state) 
                                                  (m-value (get-operand2 statement) state)))
            ((eq? '* (get-operator statement)) (* (m-value (get-operand1 statement) state) 
                                                  (m-value (get-operand2 statement) state)))
            ((eq? '/ (get-operator statement)) (quotient (m-value (get-operand1 statement) state) 
                                                         (m-value (get-operand2 statement) state)))
            ((eq? '% (get-operator statement)) (remainder (m-value (get-operand1 statement) state) 
                                                          (m-value (get-operand2 statement) state)))
            (else (error 'Mvalue "Unknown statement")))
       (cond
            ((number? statement) statement)
            ((eq? varInitVal statement) (error 'Mvalue "Variable undefined"))
            ((or (eq? 'true statement) (eq? 'false statement)) statement) ;this technically violates the method signature...
            ((state-lookup? statement state) (m-value (state-get-value statement state)))
            (else (error 'Mvalue "Variable not declared"))))))

;===================================
; Mname

(define m-name
    (lambda (variable state)
        (if (state-lookup? variable state)
            variable
            (error 'Mname "Variable not found in the state"))))



