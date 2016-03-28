; Rebecca Frederick
; EECS 345
; Programming Project 3

(load "functionParser.scm")

;======================================
; error-handling:
(define debug 1)
(define myerror
  (lambda (arg1 arg2 state)
  (if (zero? debug)
      (error arg1 arg2); this will produce the nicely-formatted error message as intended
      (error arg1 arg2 (print state))))) ; this will produce a real error, but the error message will
                                         ; still include both arguments, -and- the state gets printed.
; default break/continue continuation
(define default-brk (lambda (s) (myerror 'break "Break or Continue outside of Loop" s)));(error 'break "Break or Continue outside of while loop")))
(define default-throw (lambda (e s) (myerror 'throw "Throw without Catch" s)));(error 'throw "Throw without Catch")))
; general error handlers with 1 and 2 params
(define err1 (lambda (s) (myerror 'Error "Error!" s)))
(define err2 (lambda (e s) (err1 s)))

;=======================================
; main interpreter function

(define interpret
  (lambda (f)
    (call/cc
     (lambda (break)
       (let ((do-interpret (lambda (return)
                             (return (m-value '(funcall main) (create-table (parser f) empty-state))))))
         (do-interpret (lambda (s) (break s))))))))

; same as interpret function from Project 2, but takes a parsetree directly instead of a file, and an initial state
(define old-interpret2
    (lambda (parsetree state)
      (call/cc
       (lambda (break)
         (let ((do-interpret (lambda (return)
                               (run-state parsetree state default-brk default-brk default-throw return))))
           (do-interpret (lambda (s) (break s))))))))

; takes a parse tree 't' and initial state 's' and returns the program state
; Formally, a parse tree is a list where each sublist corresponds to a statement.
(define run-state
    (lambda (pt state break continue throw prog-return)
      (if (null? pt)
          state
          (run-state (cdr pt)
                     (m-state (car pt) state break continue throw prog-return)
                     break continue throw prog-return))))


(define empty-state '())
(define empty-scope-state '(()()))
(define varInitVal 'Undefined)
(define progRetVal '$v0)
(define noValue 'Void)

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

; check if scope-state is equal to empty-scope-state, since (eq? '(()) '(())) => #f
(define empty-scope-state? (lambda (s) (null? (car s))))
(define empty-state? (lambda (s) (null? s)))

; return the first scope in the state ( ->( (x,y,z) (1,2,3) )  ( (q,r,s) (4,5,6) )  )
(define state-peek-scope (lambda (s) (if (empty-state? s)
                                         (error 'state-peek-scope "empty state") ; for debugging
                                         (car s))))
; return the first variable in the state
(define state-peek-var (lambda (s) (cond ((empty-state? s) (error 'state-peek-scope "empty state"))
                                         ((empty-scope-state? (state-peek-scope s)) '())
                                         (else (caar (state-peek-scope s))))))
; return the first value in the state
(define state-peek-val (lambda (s) (cond ((empty-state? s) (error 'state-peek-scope "empty state"))
                                         ((empty-scope-state? (state-peek-scope s)) '())
                                         (else (caadr (state-peek-scope s))))))

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
; should NOT pop the scope!
(define state-pop-binding 
    (lambda (state)
        (cond
            ((empty-state? state) state)
            ((empty-scope-state? (state-peek-scope state)) state)
            ;((null? (state-rest-vars state)) (state-rest-scope state))
            (else (cons (cons (state-rest-vars state)
                              (cons (state-rest-vals state) '()))
                        (state-rest-scope state))))))

; add a new (variable, value) to the first scope of the state and return the new state
(define state-push-binding 
    (lambda (variable value state)
        (cond
            ((empty-state? state) (state-push-binding variable value (state-push-scope state)))
            (else (cons (cons (cons variable (state-all-vars state))
                              (cons (cons value (state-all-vals state)) '()))
                        (state-rest-scope state))))))

; add a new scope to the state and return the new state
(define state-push-scope
  (lambda (state)
    (cons empty-scope-state state)))

; remove the top scope from the state and return the new state
; same as state-rest-scope with error handling
(define state-pop-scope
  (lambda (state)
    (if (empty-state? state)
        (error 'state-pop-scope "state is empty")
        (cdr state))))

; add a variable binding to the first scope of the state and return the new state
(define state-add-binding
    (lambda (variable value state)    
        (cond
            ((state-lookup-inscope? variable state) (state-push-binding variable value (state-rem-binding variable state)))
            (else (state-push-binding variable value state)))))

; remove a variable binding from the first scope of the state and return the new state
(define state-rem-binding
    (lambda (variable state)
        (cond
            ((empty-state? state) empty-state)
            ((eq? variable (state-peek-var state)) (state-pop-binding state))
            (else (state-add-binding (state-peek-var state) (state-peek-val state) (state-rem-binding variable (state-pop-binding state)))))))

; check whether a variable is already recorded in the first scope of the state
; return true/false
(define state-lookup-inscope?
    (lambda (variable state)
        (cond
            ((empty-state? state) #f)
            ((empty-scope-state? (state-peek-scope state)) #f)
            ((eq? variable (state-peek-var state)) #t)
            (else (state-lookup-inscope? variable (state-pop-binding state))))))

; check whether a variable is recorded in any scope of the state (starting with inner-most scope)
(define state-lookup?
  (lambda (variable state)
    (cond
      ((empty-state? state) #f)
      ((empty-scope-state? (state-peek-scope state)) (state-lookup? variable (state-pop-scope state)))
      ((eq? variable (state-peek-var state)) #t)
      (else (state-lookup? variable (state-pop-binding state))))))

; check whethere a function (name, paramlist) is recorded in any scope of the state (starting with inner-most scope)
; having a separate lookup function for functions allows functions to be overloaded with different paramenter lists.
; not implemented.
(define state-lookup-func?
  (lambda (funcname funcparams-eval state)
    (state-lookup? funcname state)))
    ;(cond
    ;  ((empty-state? state) #f)
    ;  ((empty-scope-state? (state-peek-scope state)) (state-lookup-func? funcname funcparams (state-pop-scope state)))
    ;  ((and (eq? funcname (state-peek-var state)) 
    ;        (and (list? (state-peek-val state)) (listleneq? funcparams-eval (closure-paramlist (state-peek-val state))))) #t)
    ;  (else (state-lookup-func? funcname funcparams (state-pop-binding state))))))

; find the value associated with a variable in the state (starting with inner-most scope)
; return the value of the variable, or an error condition if the variable cannot be found
(define state-get-value
    (lambda (variable state)
        (cond
            ((empty-state? state) (error 'state-get-value "Variable not in State"))
            ((empty-scope-state? (state-peek-scope state)) (state-get-value variable (state-pop-scope state)))
            ((eq? variable (state-peek-var state)) (state-peek-val state))
            (else (state-get-value variable (state-pop-binding state))))))

;=====================================
; Mstate 

; main Mstate function
; takes a statment and a state and updates the state by evaluating the statement
; takes a program-return continuation, and 'goto'-continuations
(define m-state
    (lambda (statement state break continue throw prog-return)
      (if (list? statement)
        (cond
          ((eq? 'var (get-operator statement)) (m-state-var statement state break continue throw prog-return))
          ((eq? '= (get-operator statement)) (m-state-assign statement state break continue throw prog-return))
          ((eq? 'if (get-operator statement)) (m-state-if statement state break continue throw prog-return))
          ((eq? 'while (get-operator statement)) (m-state-while statement state break continue throw prog-return))
          ((eq? 'return (get-operator statement)) (m-state-return statement state break continue throw prog-return))
          ((eq? 'begin (get-operator statement)) (m-state-begin-scope statement state break continue throw prog-return))
          ((eq? 'break (get-operator statement)) (m-state-break state break))
          ((eq? 'continue (get-operator statement)) (m-state-continue state continue))
          ((eq? 'throw (get-operator statement)) (m-state-throw statement state throw))
          ((eq? 'try (get-operator statement)) (m-state-tcf statement state break continue throw prog-return))
          ((or (bool-op2? (get-operator statement))
               (comp-op? (get-operator statement))) (m-state (get-operand2 statement) (m-state (get-operand1 statement) state break continue throw prog-return) break continue throw prog-return))
          ((bool-op1? (get-operator statement)) (m-state (get-operand1 statement) state break continue throw prog-return))
          ((eq? 'function (get-operator statement)) (m-state-funcdef statement state break continue throw prog-return))
          ((eq? 'funcall (get-operator statement)) (prog-return (m-value statement state)))
          (else (myerror 'Mstate "Unknown statement" statement)))
        (cond
          ((number? (m-value statement state)) state)
          ((bool? (m-value statement state)) state)
          ((eq? noValue statement) state)
          ((state-lookup? statement state) state)
          (else (myerror 'Mstate "Unknown atom" statement))))))

; bind a function name to its closure, where the closure consists of the formal parameter list,
; the function body, and a function that creates the function environment from the current environment
(define m-state-funcdef
  (lambda (statement state break continue throw prog-return)
    (if (state-lookup-inscope? (funcdef-name statement) state)
        (myerror 'MstateFunctionBinding "Function already declared in this scope" state)
        (state-add-binding (funcdef-name statement) (create-function-closure statement) state))))


(define m-state-break (lambda (state break) (break state)))
(define m-state-continue (lambda (state continue) (continue state)))


(define m-state-throw
  (lambda (statement state throw)
    (throw (except-stmt statement) state)))
(define except-stmt cadr)

(define m-state-tcf
  (lambda (statement state break continue throw prog-return)
    (call/cc
     (lambda (try-break)
       (letrec ((finally (lambda (s)
                    (cond
                      ((null? (finally-stmt statement)) s)
                      ((list? (car (finally-body statement))) (run-state (finally-body statement) s break continue throw prog-return))
                      (else (m-state (finally-body statement) s break continue throw prog-return)))))

                (try (lambda (s try-throw)
                       (if (list? (car (try-body statement)))
                           (finally (run-state (try-body statement) s break continue try-throw prog-return))
                           (finally (m-state (try-body statement) s break continue try-throw prog-return)))))

                (catch (lambda (e s)
                         (if (list? (car (catch-body statement)))
                             (finally (run-state (replace*-cps (catch-err statement) e (catch-body statement)) s break continue throw prog-return))
                             (finally (m-state (replace*-cps (catch-err statement) e (catch-body statement)) s break continue throw prog-return))))))
         (try state (lambda (e s) (try-break (catch e s)))) )))))

  ; (try body (catch (e) body) (finally body))
(define try-body cadr)
(define catch-body (lambda (t) (if (null? (cddr (caddr t)))  '()  (car (cddr (caddr t))))))
(define catch-err (lambda (t) (car (cadr (caddr t)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

; a new block of code. pop the scope when finished executing.
(define m-state-begin-scope
  (lambda (statement state break continue throw prog-return)
    (call/cc
     (lambda (brk)
       (let ((begin-scope (lambda (b c)
                            (state-pop-scope (run-state (cdr statement) (state-push-scope state) b c throw prog-return)))))
         (begin-scope (lambda (s) (brk (break (state-pop-scope s)))) (lambda (s) (brk (continue (state-pop-scope s))))))))))             
    

(define m-state-assign
  (lambda (statement state break continue throw prog-return)
    (cond
      ((not (state-lookup? (assign-var statement) state)) (error 'Mstate_assign "variable has not been declared"))
      (else (letrec ((original-state state)
                     (loop (lambda (statement state return)
                             (if (state-lookup-inscope? (assign-var statement) state)
                                 (return (state-add-binding (assign-var statement)
                                                            (m-value (assign-expr statement) original-state)
                                                            (state-rem-binding (assign-var statement) state)))
                                 (loop statement (state-pop-scope state) (lambda (v) (return (cons (state-peek-scope state) v))))))))
              (loop statement state (lambda (v) v)))))))
(define assign-var cadr)
(define assign-expr caddr)


(define m-state-if
    (lambda (statement state break continue throw prog-return)
        (cond 
            ((m-bool (if-cond statement) (m-state (if-cond statement) state default-brk default-brk default-brk default-brk))
             (m-state (if-then statement)
                      (m-state (if-cond statement) state default-brk default-brk default-brk default-brk)
                      break continue throw prog-return))
            ((eq? 3 (len statement)) state) ; no optional-else-statement
            (else
             (m-state (if-optelse statement)
                      (m-state (if-cond statement) state default-brk default-brk default-brk default-brk)
                      break continue throw prog-return)))))
(define if-cond cadr)
(define if-then caddr)
(define if-optelse cadddr)


(define m-state-return
    (lambda (statement state break continue throw prog-return) 
        ;(state-add-binding progRetVal (m-value (return-expr statement) state) state)))
        (prog-return (if (and (list? (return-expr statement))
                              (or (bool-op1? (get-operator (return-expr statement)))
                                  (bool-op2? (get-operator (return-expr statement)))
                                  (comp-op? (get-operator (return-expr statement)))))
                         (bool2sym (m-bool (return-expr statement) state))
                         (m-value (return-expr statement) state)))))
(define return-expr cadr)


(define m-state-while
  (lambda (statement state break continue throw prog-return)
    (call/cc
     (lambda (brk)
       (letrec ((loop (lambda (statement state)
                        (if (m-bool (while-cond statement) (m-state (while-cond statement) state default-brk default-brk default-throw default-brk))
                            (loop statement (m-state (while-stmt statement)
                                                     (m-state (while-cond statement) state default-brk default-brk default-throw default-brk)
                                                     (lambda (s) (brk s))
                                                     (lambda (s) (brk (loop statement s)))
                                                     throw prog-return))
                            (m-state (while-cond statement) (m-state (while-cond statement) state default-brk default-brk default-throw default-brk) default-brk default-brk default-throw default-brk)))))
         (loop statement state))))))                            
(define while-cond cadr)
(define while-stmt caddr)


(define m-state-var
    (lambda (statement state break continue throw prog-return)
      (cond
        ((state-lookup-inscope? (var-var statement) state) (error 'Mstate_var "Variable already declared"))
        ((eq? 2 (len statement)) (state-add-binding (var-var statement) varInitVal state)) ;(var <name>)
        ((and (list? (var-value statement))
                 (or (bool-op1? (get-operator (var-value statement)))
                     (bool-op2? (get-operator (var-value statement)))
                     (comp-op? (get-operator (var-value statement))))) (state-add-binding (var-var statement) (bool2sym (m-bool (var-value statement) state)) state))
        (else (state-add-binding (var-var statement) (m-value (var-value statement) state) state)))))
(define var-var cadr)
(define var-value caddr)

;========================================
; Mbool ;;;;ERROR: NEED FUNCALL TO BE ABLE TO MODIFY STATE- CAN'T ALWAYS CALL M-BOOL WITH OLD STATE

; M_bool function
; returns whether the given condition is satisfied in the given state
(define m-bool
    (lambda (condition state)
      (if (list? condition)
        (cond
            ((eq? '== (get-operator condition)) (eq? (m-value (get-operand1 condition) state) 
                                                        (m-value (get-operand2 condition) state)))
            ((eq? '!= (get-operator condition)) (not (eq? (m-value (get-operand1 condition) state)
                                                             (m-value (get-operand2 condition) state))))
            ((eq? '>= (get-operator condition)) (>= (m-value (get-operand1 condition) state)
                                                       (m-value (get-operand2 condition) state)))
            ((eq? '<= (get-operator condition)) (<= (m-value (get-operand1 condition) state)
                                                       (m-value (get-operand2 condition) state)))
            ((eq? '>  (get-operator condition)) (> (m-value (get-operand1 condition) state)
                                                      (m-value (get-operand2 condition) state)))
            ((eq? '<  (get-operator condition)) (< (m-value (get-operand1 condition) state)
                                                      (m-value (get-operand2 condition) state)))
            ((eq? '&& (get-operator condition)) (and (m-bool (get-operand1 condition) state)
                                                        (m-bool (get-operand2 condition) state)))
            ((eq? '|| (get-operator condition)) (or (m-bool (get-operand1 condition) state)
                                                       (m-bool (get-operand2 condition) state)))
            ((eq? '!  (get-operator condition)) (not (m-bool (get-operand1 condition) state)))
            ((eq? 'funcall (get-operator condition)) (m-bool (m-value condition state) state))
            (else (myerror 'Mbool "Unknown expression" (get-operator condition))))
        (cond
          ((number? condition) (myerror 'Mbool "Type Error: given Number, expected Boolean" condition))
          ((eq? noValue condition) (myerror 'Mbool "Type Error: given 'Void', expected Boolean" condition))
          ((boolsym? condition) (sym2bool condition))
          ((bool? condition) condition)
          ((state-lookup-inscope? condition state) (m-bool (state-get-value condition state) state))
          (else (myerror 'Mbool "Unknown atom" condition))))))

(define bool2sym (lambda (b) (if b 'true 'false)))
(define sym2bool (lambda (sym) (eq? 'true sym)))
(define boolsym? (lambda (sym) (or (eq? 'true sym) (eq? 'false sym))))
(define bool? (lambda (sym) (or (eq? #t sym) (eq? #f sym))))
(define bool-op1? (lambda (sym) (eq? '! sym)))
(define bool-op2?
  (lambda (sym)
    (cond
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
        
;========================================
; Mvalue 

; Mvalue takes an expression and a state, and returns a numeric value, boolean value, or error condition
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
            ((eq? '= (get-operator statement)) (m-value (state-get-value (assign-var statement)
                                                                         (m-state statement state default-brk default-brk default-throw default-brk))
                                                        (m-state statement state default-brk default-brk default-throw default-brk)))
            ((eq? 'funcall (get-operator statement)) (m-value-funcall statement state))
            (else (myerror 'Mvalue "Unknown statement" statement)))
       (cond
            ((number? statement) statement)
            ((eq? varInitVal statement) (myerror 'Mvalue "Variable undefined" statement))
            ((or (eq? 'true statement) (eq? 'false statement)) statement)
            ((eq? noValue statement) state) ;void function return
            ((state-lookup? statement state) (m-value (state-get-value statement state) state))
            (else (myerror 'Mvalue "Variable not declared" statement))))))

; do basic error-checking on the function call then evaluate the function
(define m-value-funcall
  (lambda (statement state)
    (if (not (state-lookup? (funcall-name statement) state))
        (myerror 'Mvalue_Funcall "Function not defined" (funcall-name statement))
        (evaluate-function-byreference (funcall-paramlist statement) (state-get-value (funcall-name statement) state) state))))

(define evaluate-function-byreference
  (lambda (paramlist closure currentstate)
    (evaluate-function (closure-fbody closure)
                       (((lambda (m) (m m))
                         (lambda (f)
                           (lambda (actuals formals fenv currentenv)
                             (cond
                               ((and (null? actuals) (null? formals)) fenv)
                               ((or (null? actuals) (null? formals)) (myerror 'EvaluateFunction "Actual and Formal Parameter lists differ in length" actuals))
                               (else ((f f) (cdr actuals) (cdr formals) (state-add-binding (car formals) (m-value (car actuals) currentenv) fenv) currentenv))))))
                        paramlist (closure-paramlist closure) ((closure-env closure) currentstate) currentstate)                                                                                                                    
                       currentstate)))

(define evaluate-function
  (lambda (fbody fenv oldstate)
    ((lambda (funcRetVal)
      (cond
         ((number? funcRetVal) funcRetVal)
         ((boolsym? funcRetVal) funcRetVal)
         ((bool? funcRetVal) (bool2sym funcRetVal))
         (else (myerror 'EvaluateFunction "Unknown return type:" funcRetVal))));noValue)))
    (old-interpret2 fbody fenv))))

(define bind-params-byname
  (lambda (actuals formals body state)
    (letrec ((loop (lambda (evaluated formals body)
                     (cond
                       ((and (null? evaluated) (null? formals)) body)
                       ((or (null? evaluated) (null? formals)) (error 'BindParams "Actual and Formal parameter lists differ in length"))
                       ((and (eq? '& (car formals))
                             (or (number? (car evaluated)) (eq? #t (car evaluated)) (eq? #f (car evaluated))))
                             (error 'BindParams "Cannot pass a literal value as a reference"))
                        ((eq? '& (car formals)) (loop (cdr evaluated) (cddr formals) (replace*-cps (cadr formals) (car evaluated) body)))
                        (else (loop (cdr evaluated) (cdr formals) (replace*-cps (car formals) (car evaluated) body)))))))
      (loop (evaluate-params-reference actuals state) formals body))))    

;===================================
; Mname

(define m-name
    (lambda (variable state)
        (if (state-lookup-inscope? variable state)
            variable
            (error 'Mname "Variable not found in the state"))))

;===================================
; New functions to handle functions

; 'outer layer' of the interpreter: create a symbol table with global variable and function definitions.
; This will be the base layer of the program's environment. the 'table' is just a single-scope state.
(define create-table
  (lambda (parsetree table)
    (cond
      ((null? parsetree) table)
      ((eq? 'var (get-operator (car parsetree))) (create-table (cdr parsetree) (do-global-var-binding (car parsetree) table)))
      ((eq? 'function (get-operator (car parsetree))) (create-table (cdr parsetree) (do-global-func-binding (car parsetree) table)))
      ((eq? '= (get-operator (car parsetree))) (create-table (cdr parsetree) (do-global-var-assign (car parsetree) table)))
      (else (error 'CreateTable "Can only have variable/function declarations and variable assignments at global level")))))

(define do-global-var-binding (lambda (statement table) (m-state-var statement table err1 err1 err2 err1)))
(define do-global-var-assign (lambda (statement table) (m-state-assign statement table err1 err1 err2 err1)))
(define do-global-func-binding (lambda (statement table) (m-state-funcdef statement table err1 err1 err2 err1)))

; create the function closure from the funtion-definition statement: (function <name> (<args>) (<body>))
(define create-function-closure
  (lambda (statement)
    (cons (funcdef-paramlist statement) (cons (funcdef-body statement) (cons (mk-create-func-env (funcdef-name statement)) '())))))

; return a function to create the function environment from the current environment
(define mk-create-func-env
  (lambda (fname)
    ((lambda (m) (m m))
     (lambda (r)
       (lambda (currentstate)
         (state-push-scope (if (state-lookup-inscope? fname currentstate)
                               currentstate
                               ((r r) (state-pop-scope currentstate)))))))))

; for a function-call statement: (funcall name [args])
(define funcall-name (lambda (statement) (cadr statement)))
(define funcall-paramlist
  (lambda (statement)
    (if (null? (cddr statement))
        '()
        (((lambda (m) (m m))
          (lambda (f)
            (lambda (l)
              (if (null? l)
                  '()
                  (cons (car l) ((f f) (cdr l)))))))
         (cddr statement)))))

; for a function-definition statement: (function name (args) (body))
(define funcdef-name (lambda (statement) (cadr statement)))
(define funcdef-paramlist
  (lambda (statement)
    (cond
      ((null? (cddr statement)) '())
      ((not (list? (caddr statement))) (cons (caddr statement) '()))
      (else (caddr statement)))))
(define funcdef-body (lambda (statement) (cadddr statement)))

; for a function-closure: ((args) (body) (mk-env-function))
(define closure-paramlist (lambda (closure) (car closure)))
(define closure-fbody (lambda (closure) (cadr closure)));(cadr closure)))
(define closure-env (lambda (closure) (caddr closure)))

;============================
; utility functions

; element-wise substitution
(define replace*-cps
  (lambda (old new l)
    (letrec ((loop-cps (lambda (old new l return)
                         (cond
                           ((null? l) (return l))
                           ((pair? (car l)) (loop-cps old new (cdr l) (lambda (v) (loop-cps old new (car l) (lambda (v2) (return (cons v2 v)))))))
                           ((eq? (car l) old) (loop-cps old new (cdr l) (lambda (v) (return (cons new v)))))
                           (else (loop-cps old new (cdr l) (lambda (v) (return (cons (car l) v)))))))))
      (loop-cps old new l (lambda (v) v)))))

; test if two (flat) lists are equal 
(define listeq?
  (lambda (l1 l2)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (l1 l2)
                        (cond
                          ((and (null? l1) (null? l2)) #t)
                          ((or (null? l1) (null? l2)) (break #f))
                          ((not (eq? (car l1) (car l2))) (break #f))
                          (else (loop (cdr l1) (cdr l2)))))))
         (loop l1 l2))))))

; test if two (flat) lists are the same length
(define listleneq?
  (lambda (l1 l2)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (l1 l2)
                        (cond
                          ((and (null? l1) (null? l2)) #t)
                          ((or (null? l1) (null? l2)) (break #f))
                          (else (loop (cdr l1) (cdr l2)))))))
         (loop l1 l2))))))

(define len (lambda (l) (apply + (map (lambda (x) 1) l))))




