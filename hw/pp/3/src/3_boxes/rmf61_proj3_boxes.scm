; Rebecca Frederick
; EECS 345
; Programming Project 3 (using boxes)

(load "functionParser.scm")

;======================================
; error-handling: if debug=0, myerror will produce the intended error message; else it will produce
; a real error, but the error message will include both arguments, plus the thing that gets printed.
(define debug 1)
(define myerror (lambda (a1 a2 printme) (if (zero? debug) (error a1 a2) (error a1 a2 (print printme)))))
; default continuations
(define default-brk (lambda (s) (myerror 'break "Break outside of loop" s)));(error 'break "Break outside of loop")))
(define default-con (lambda (s) (myerror 'continue "Continue outside of loop" s)));(error 'continue "Continue outside of loop")))
(define default-throw (lambda (e s) (myerror 'throw "Throw without Catch" s)));(error 'throw "Throw without Catch")))
(define default-ret (lambda (v s) (myerror 'return "Return statement" v)))
; general error handlers with 1 and 2 params
(define err1 (lambda (s) (myerror 'Error "Error!" s)))
(define err2 (lambda (e s) (err1 s)))

; constants
(define empty-state '())
(define empty-scope-state '(()()))
(define varInitVal 'Undefined)
(define progRetVal '$v0) ;not used.
(define noValue 'Void)

;=======================================
; main interpreter function
(define interpret
  (lambda (f)
    (call/cc
     (lambda (break)
       (let ((do-interpret (lambda (parsetree return)
                             (m-value '(funcall main) (create-table parsetree (box empty-state)) default-brk default-con default-throw return))))
         (do-interpret (parser f) (lambda (v bs) (break v))))))))

; same as interpret function from Project 2, but takes a parsetree directly instead of a file,
; an initial state, and a return continuation (lambda (v s)...) to know where to pass control
; after the function returns, and there the return value and resulting state will be handled appropriately.
(define interpret-function
  (lambda (parsetree bstate break continue throw return)
    (if (null? parsetree)
        (return noValue bstate) ;this will be executed in the case with no return statement in the function body
        (interpret-function (cdr parsetree) (m-state (car parsetree) bstate break continue throw return) break continue throw return))))

; returns the resulting program state from executing the expressions in a parsetree from a given initial state
(define run-state
    (lambda (pt bstate break continue throw prog-return)
      (if (null? pt) 
          bstate
          (run-state (cdr pt) 
                     (m-state (car pt) bstate break continue throw prog-return)
                     break continue throw prog-return))))

;===================================
; state manipulation functions

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
                                         (error 'StatePeekScope "empty state") ; for debugging
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

; return the global (outermost/last) scope of a state
(define state-global-scope
  (lambda (s)
    (if (or (empty-state? s) (empty-state? (state-pop-scope s)))
        s
        (state-global-scope (state-pop-scope s)))))      

; in the following methods, 'variables' means vars and functions
; return a list of all variables in the first scope of the state
(define state-all-vars (lambda (s) (car (state-peek-scope s))))
; return a list of all values in the first scope of the state
(define state-all-vals (lambda (s) (cadr (state-peek-scope s))))
; return a list of all variables in the last (outermost/global) scope of the state
(define state-all-global-vars (lambda (s) (state-all-vars (state-global-scope s))))
; return a lsit of all values in the last (outermost/global) scope of the state
(define state-all-global-vals (lambda (s) (state-all-vals (state-global-scope s))))
; return a list of all variables in all scopes of the state
(define state-all-vars*
  (lambda (s)
    (((lambda (m) (m m))
      (lambda (f)
        (lambda (state ret)
          (if (empty-state? state)
              (ret '())
              ((f f) (state-pop-scope state) (lambda (v) (ret (myappend (state-all-vars state) v))))))))
     s (lambda (v) v))))

; return the number of scope levels in a state
(define num-scopes?
  (lambda (s)
    (if (empty-state? s)
        0
        (+ 1 (num-scopes? (state-pop-scope s))))))

; return a list of all variables in all scopes of the second state, which are not in the new scopes of the first state
; (i.e., variables in the second state which the first state has not redefined)
(define state-all-vars-not-inscope*
  (lambda (s1 s2)
    (if (or (empty-state? s1) (empty-state? s2))
        (state-all-vars* s2)
        (((lambda (m n) (m m n))
          (lambda (f g)
            (lambda (state1 state2-vars nlevels ret)
              (if (null? state2-vars)
                  (ret '())
                  ((f f g) state1 (cdr state2-vars) nlevels (lambda (v) (ret (append v ((g g) state1 (car state2-vars) nlevels))))))))
          (lambda (g)
            (lambda (state var nlevels)
              (cond
                ((<= nlevels 0) (cons var '())) ;var not redefined in the top nlevels of the state. good to return.
                ((not (state-lookup? var state)) (cons var '())) ;don't need to return this as a variable that needs to be updated, but that goes against the contract of this general method.
                ((state-lookup-inscope? var state) '()) ;var has been redefined
                (else ((g g) (state-pop-scope state) var (- nlevels 1)))))))
         s1 (state-all-vars* s2) (- (num-scopes? s1) (num-scopes? s2)) (lambda (v) v)))))
    

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
(define state-push-scope (lambda (state) (cons empty-scope-state state)))

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
; not implemented yet-- just calls state-lookup?.
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
            ((empty-state? state) (myerror 'StateGetValue "Variable not in State" variable))
            ((empty-scope-state? (state-peek-scope state)) (state-get-value variable (state-pop-scope state)))
            ((eq? variable (state-peek-var state)) (state-peek-val state))
            (else (state-get-value variable (state-pop-binding state))))))

;=====================================
; Mstate 

; takes a statment and a state and updates the state by evaluating the statement
; takes a program-return continuation, and 'goto'-continuations
; takes a boxed state and returns a boxed state
(define m-state
    (lambda (statement bstate break continue throw prog-return)
      (if (list? statement)
        (cond
          ((eq? 'var (get-operator statement)) (m-state-var statement bstate break continue throw prog-return))
          ((eq? '= (get-operator statement)) (m-state-assign statement bstate break continue throw prog-return) state)
          ((eq? 'if (get-operator statement)) (m-state-if statement bstate break continue throw prog-return))
          ((eq? 'while (get-operator statement)) (m-state-while statement bstate break continue throw prog-return))
          ((eq? 'return (get-operator statement)) (m-state-return statement bstate break continue throw prog-return))
          ((eq? 'begin (get-operator statement)) (m-state-begin-scope statement bstate break continue throw prog-return))
          ((eq? 'break (get-operator statement)) (m-state-break bstate break))
          ((eq? 'continue (get-operator statement)) (m-state-continue bstate continue))
          ((eq? 'throw (get-operator statement)) (m-state-throw statement bstate break continue throw prog-return))
          ((eq? 'try (get-operator statement)) (m-state-tcf statement bstate break continue throw prog-return))
          ((or (bool-op2? (get-operator statement))
               (comp-op? (get-operator statement))) (m-state (get-operand2 statement) 
                                                             (m-state (get-operand1 statement) bstate break continue throw prog-return) 
                                                             break continue throw prog-return))
          ((bool-op1? (get-operator statement)) (m-state (get-operand1 statement) bstate break continue throw prog-return))
          ((eq? 'function (get-operator statement)) (m-state-funcdef statement bstate break continue throw prog-return))
          ((eq? 'funcall (get-operator statement)) (m-state-funcall statement bstate break continue throw prog-return))
          (else bstate));(else (myerror 'Mstate "Unknown statement" statement)))
        (cond
          ((or (number? (m-value statement bstate break continue throw prog-return))
               (boolsym? (m-value statement bstate break continue throw prog-return))
               (bool? (m-value statement bstate break continue throw prog-return))
               (eq? noValue statement)
               (state-lookup? statement (unbox bstate)))
            bstate)
          (else (myerror 'Mstate "Unknown atom" statement))))))
(define get-operator car)
(define get-operand1 cadr)
(define get-operand2 caddr)

; bind a function name to its closure, where the closure consists of the formal parameter list,
; the function body, and a function that creates the function environment from the current environment
(define m-state-funcdef
  (lambda (statement bstate break continue throw prog-return)
    (if (state-lookup-inscope? (funcdef-name statement) (unbox bstate))
        (myerror 'MstateFunctionBinding "Function already declared in this scope" (unbox bstate))
        (box (state-add-binding (funcdef-name statement) (create-function-closure statement) (unbox bstate))))))

; interpret the function and copy the outer-scope variable values back to the original state README
(define m-state-funcall
  (lambda (statement bstate break continue throw prog-return)
    (call/cc
     (lambda (mstatebreak)
       (let ((do-m-state-funcall (lambda (statement state)
                                   (if (not (state-lookup? (funcall-name statement) state))
                                       (myerror 'Mvalue_Funcall "Function not defined" (funcall-name statement))
                                       (evaluate-function-byreference (funcall-paramlist statement)
                                                                      (state-get-value (funcall-name statement) state)
                                                                      (box state)
                                                                      break
                                                                      continue
                                                                      (lambda (e s) (throw e (if (box? s) s (box s))));(vars-not-inscope-copy s state))))
                                                                      (lambda (v s) (mstatebreak (if (box? s) s (box s)))))))));(box (vars-not-inscope-copy s state)))))))))
         (do-m-state-funcall statement (unbox bstate)))))))

(define m-state-break (lambda (bstate break) (break bstate)))
(define m-state-continue (lambda (bstate continue) (continue bstate)))


(define m-state-throw
  (lambda (statement bstate break continue throw prog-return)
    (throw (m-value (except-stmt statement) bstate break continue throw prog-return) bstate)));README no side-effects
    ;(throw (m-value (except-stmt statement) bstate break continue throw prog-return) (m-state (except-stmt statement) bstate break continue throw prog-return)))
(define except-stmt cadr)

(define m-state-tcf
  (lambda (statement bstate break continue throw prog-return)
    (call/cc
     (lambda (try-break)
       (letrec ((finally (lambda (bs)
                    (cond
                      ((null? (finally-stmt statement)) bs)
                      ((list? (car (finally-body statement))) (run-state (finally-body statement) bs break continue throw prog-return))
                      (else (m-state (finally-body statement) bs break continue throw prog-return)))))

                (try (lambda (bs try-throw)
                       (if (list? (car (try-body statement)))
                           (finally (run-state (try-body statement) bs break continue try-throw prog-return))
                           (finally (m-state (try-body statement) bs break continue try-throw prog-return)))))

                (catch (lambda (e bs)
                         (if (list? (car (catch-body statement)))
                             (finally (run-state (replace*-cps (catch-err statement) e (catch-body statement)) bs break continue throw prog-return))
                             (finally (m-state (replace*-cps (catch-err statement) e (catch-body statement)) bs break continue throw prog-return))))))
         (try state (lambda (e bs) (try-break (catch e bs)))) )))))

  ; (try body (catch (e) body) (finally body))
(define try-body cadr)
(define catch-body (lambda (t) (if (null? (cddr (caddr t)))  '()  (car (cddr (caddr t))))))
(define catch-err (lambda (t) (car (cadr (caddr t)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

; a new block of code. pop the scope when finished executing.
(define m-state-begin-scope
  (lambda (statement bstate break continue throw prog-return)
    (call/cc
     (lambda (brk)
       (let ((begin-scope (lambda (b c)
                            (state-pop-scope (run-state (cdr statement) (box (state-push-scope (unbox bstate)))b b c throw prog-return)))))
         (begin-scope (lambda (bs) (brk (break (box (state-pop-scope (unbox s)))))) (lambda (bs) (brk (continue (box (state-pop-scope (unbox s))))))))))))             
    
; update the state to reflect a new variable binding (= varname value) README: STATE only in '19/20 works' commit
(define m-state-assign
  (lambda (statement bstate break continue throw prog-return)
    (state-assign (assign-var statement)
                  (m-value (assign-expr statement) bstate break continue throw prog-return)
                  (m-state (assign-expr statement) bstate break continue throw prog-return))));state)))
(define state-assign
  (lambda (var value bstate)
    (if (not (state-lookup? var (unbox bstate)))
        (myerror 'StateAssign "variable has not been declared" var)
        (((lambda (m) (m m))
          (lambda (f)
            (lambda (var value bs ret)
              (if (state-lookup-inscope? var (unbox bs))
                  (ret (box (state-add-binding var value (state-rem-binding var (unbox bs)))))
                  ((f f) var value (box (state-pop-scope (unbox bs))) (lambda (v) (ret (box (cons (state-peek-scope (unbox bs)) v)))))))))
         var value bstate (lambda (v) v)))))      
(define assign-var cadr)
(define assign-expr caddr)

; if-then-else statement with side-effects
(define m-state-if
    (lambda (statement bstate break continue throw prog-return)
        (cond 
            ((m-bool (if-cond statement) (m-state (if-cond statement) bstate break continue throw prog-return) break continue throw prog-return)
             (m-state (if-then statement)
                      (m-state (if-cond statement) bstate break continue throw prog-return)
                      break continue throw prog-return))
            ((eq? 3 (len statement)) bstate) ; no optional-else-statement
            (else
             (m-state (if-optelse statement)
                      (m-state (if-cond statement) bstate break continue throw prog-return)
                      break continue throw prog-return)))))
(define if-cond cadr)
(define if-then caddr)
(define if-optelse cadddr)

; returns the value of the expression along with the state by passing them
; to the return continuation: (lambda (v s) ... )
(define m-state-return
    (lambda (statement bstate break continue throw prog-return) 
        ;(state-add-binding progRetVal (m-value (return-expr statement) state) state)))
        (prog-return (if (and (list? (return-expr statement))
                              (or (bool-op1? (get-operator (return-expr statement)))
                                  (bool-op2? (get-operator (return-expr statement)))
                                  (comp-op? (get-operator (return-expr statement)))))
                         (bool2sym (m-bool (return-expr statement) bstate break continue throw prog-return))
                         (m-value (return-expr statement) bbstate break continue throw prog-return))
                     (m-state (return-expr statement) bstate break continue throw prog-return))));state)))
(define return-expr cadr)

; while-loop with side-effects
(define m-state-while
  (lambda (statement bstate break continue throw prog-return)
    (call/cc
     (lambda (brk)
       (letrec ((loop (lambda (statement bstate)
                        (if (m-bool (while-cond statement) (m-state (while-cond statement) bstate break continue throw prog-return) break continue throw prog-return)
                            (loop statement (m-state (while-stmt statement)
                                                     (m-state (while-cond statement) bstate break continue throw prog-return)
                                                     (lambda (bs) (brk bs))
                                                     (lambda (bs) (brk (loop statement bs)))
                                                     throw prog-return))
                            (m-state (while-cond statement) (m-state (while-cond statement) bstate break continue throw prog-return) break continue throw prog-return)))))
         (loop statement bstate))))))                            
(define while-cond cadr)
(define while-stmt caddr)

; variable declaration (var varname [value]) ; STATE only in README:'19/20 works' commit
(define m-state-var
    (lambda (statement bstate break continue throw prog-return)
      (cond
        ((state-lookup-inscope? (var-var statement) (unbox bstate)) (error 'Mstate_var "Variable already declared"))
        ((eq? 2 (len statement)) (box (state-add-binding (var-var statement) varInitVal (unbox bstate)))) 
        ((and (list? (var-value statement))
              (or (bool-op1? (get-operator (var-value statement)))
                  (bool-op2? (get-operator (var-value statement)))
                  (comp-op? (get-operator (var-value statement)))))
         (box (state-add-binding (var-var statement)
                                 (bool2sym (m-bool (var-value statement) bstate break continue throw prog-return))
                                 (unbox (m-state (var-value statement) bstate break continue throw prog-return)))));state))
        (else (box (state-add-binding (var-var statement)
                                      (m-value (var-value statement) bstate break continue throw prog-return)
                                      (unbox (m-state (var-value statement) bstate break continue throw prog-return))))))));state)))))
(define var-var cadr)
(define var-value caddr)

;========================================
; Mbool

; NB: to support functions that return a value but also manipulate global variables as operands to the conditional operators,
; may need to replace the second instance of 'state' in each pair by
; (m-state (get-operand2 condition) (m-state (get-operand1 condition) state default-brk default-con default-throw default-ret) default-brk default-con default-throw default-ret))

; returns a #t/#f value: whether the given condition is satisfied in the given state
(define m-bool
    (lambda (condition bstate break continue throw prog-return)
      (if (list? condition)
        (cond
            ((eq? '== (get-operator condition)) (eq? (m-value (get-operand1 condition) bstate break continue throw prog-return) 
                                                     (m-value (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '!= (get-operator condition)) (not (eq? (m-value (get-operand1 condition) bstate break continue throw prog-return)
                                                          (m-value (get-operand2 condition) bstate break continue throw prog-return))))
            ((eq? '>= (get-operator condition)) (>= (m-value (get-operand1 condition) bstate break continue throw prog-return)
                                                    (m-value (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '<= (get-operator condition)) (<= (m-value (get-operand1 condition) bstate break continue throw prog-return)
                                                    (m-value (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '>  (get-operator condition)) (> (m-value (get-operand1 condition) bstate break continue throw prog-return)
                                                   (m-value (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '<  (get-operator condition)) (< (m-value (get-operand1 condition) bstate break continue throw prog-return)
                                                   (m-value (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '&& (get-operator condition)) (and (m-bool (get-operand1 condition) bstate break continue throw prog-return)
                                                     (m-bool (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '|| (get-operator condition)) (or (m-bool (get-operand1 condition) bstate break continue throw prog-return)
                                                    (m-bool (get-operand2 condition) bstate break continue throw prog-return)))
            ((eq? '!  (get-operator condition)) (not (m-bool (get-operand1 condition) bstate break continue throw prog-return)))
            ((eq? 'funcall (get-operator condition)) (m-bool (m-value condition bstate break continue throw prog-return) state break continue throw prog-return))
            (else (myerror 'Mbool "Unknown expression" (get-operator condition))))
        (cond
          ((number? condition) (myerror 'Mbool "Type Error: given Number, expected Boolean" condition))
          ((eq? noValue condition) (myerror 'Mbool "Type Error: given 'Void', expected Boolean" condition))
          ((boolsym? condition) (sym2bool condition))
          ((bool? condition) condition)
          ((state-lookup? condition (unbox bstate)) (m-bool (state-get-value condition (unbox bstate)) bstate break continue throw prog-return))
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
    (lambda (statement bstate break continue throw prog-return)
      (if (list? statement)
       (cond
            ((and (eq? 2 (len statement))
                  (eq? '- (get-operator statement))) (* -1 (m-value (get-operand1 statement) bstate break continue throw prog-return))) ; unary operator
            ((eq? '+ (get-operator statement)) (+ (m-value (get-operand1 statement) bstate break continue throw prog-return) 
                                                  (m-value (get-operand2 statement) bstate break continue throw prog-return)))
            ((eq? '- (get-operator statement)) (- (m-value (get-operand1 statement) bstate break continue throw prog-return) 
                                                  (m-value (get-operand2 statement) bstate break continue throw prog-return)))
            ((eq? '* (get-operator statement)) (* (m-value (get-operand1 statement) bstate break continue throw prog-return) 
                                                  (m-value (get-operand2 statement) bstate break continue throw prog-return)))
            ((eq? '/ (get-operator statement)) (quotient (m-value (get-operand1 statement) bstate break continue throw prog-return) 
                                                         (m-value (get-operand2 statement) bstate break continue throw prog-return)))
            ((eq? '% (get-operator statement)) (remainder (m-value (get-operand1 statement) bstate break continue throw prog-return) 
                                                          (m-value (get-operand2 statement) bstate break continue throw prog-return)))
            ((eq? '= (get-operator statement)) (m-value (state-get-value (assign-var statement)
                                                                         (m-state statement bstate break continue throw prog-return))
                                                        (m-state statement bstate break continue throw prog-return) break continue throw prog-return))
            ((eq? 'funcall (get-operator statement)) (m-value-funcall statement bstate break continue throw prog-return))
            (else (myerror 'Mvalue "Unknown statement" statement)))
       (cond
            ((number? statement) statement)            
            ((boolsym? statement) statement)
            ((bool? statement) statement)
            ((eq? noValue statement) statement)
            ((eq? varInitVal statement) (myerror 'Mvalue "Variable undefined" statement))
            ((not (state-lookup? statement (unbox bstate))) (myerror 'Mvalue "Variable not declared" statement))
            ((state-lookup? statement (unbox bstate)) (m-value (state-get-value statement (unbox bstate)) bstate break continue throw prog-return))
            (else (myerror 'Mvalue "Unknown atom" statement))))))

; do basic error-checking on the function call then evaluate the function
; (m-value ...) does not update the state, so no need to copy the environment here.
(define m-value-funcall ;README
  (lambda (statement bstate break continue throw prog-return)
    (call/cc
     (lambda (mvaluebreak)
       (let ((do-m-value-funcall (lambda (statement bstate break continue throw prog-return)
                                   (if (not (state-lookup? (funcall-name statement) (unbox bstate)))
                                       (myerror 'Mvalue_Funcall "Function not defined" (funcall-name statement))
                                       (evaluate-function-byreference (funcall-paramlist statement)
                                                                      (state-get-value (funcall-name statement) (unbox bstate))
                                                                      bstate
                                                                      break
                                                                      continue
                                                                      (lambda (e s) (throw e (if (box? s) s (box s))));(box (vars-not-inscope-copy s (unbox bstate)))))
                                                                      (lambda (v s) (mvaluebreak (check-func-return v))))))))
         (do-m-value-funcall statement bstate break continue throw prog-return))))))

; evaluate a function-call by-reference.
; done in bind-params-byreference:
;   -evaluate the actuals parameters in the current context.
;   -bind the actual parameters to the formal parameters in the function context.
; call the general evaluate-function routine to interpret the function body in the function context.
; takes a return function (lambda (v s)...) to handle the return value and resulting state, and return the
; proper value (or state) to the caller.
(define evaluate-function-byreference
  (lambda (paramlist closure bcurrentstate break continue throw return)
    (interpret-function (closure-fbody closure)
                        (bind-params-byreference paramlist closure bcurrentstate break continue throw (lambda (v s) (error 'EvaluateFunctionByReference "Called return")))
                        break
                        continue
                        throw
                        return)))

; returns the function environment with the new parameter bindings
(define bind-params-byreference
  (lambda (paramlist closure bcurrentstate break continue throw prog-return)
    (((lambda (m) (m m))
      (lambda (f)
        (lambda (actuals formals bfenv bcurrentenv)
          (cond
            ((and (null? actuals) (null? formals)) bfenv)
            ((or (null? actuals) (null? formals)) (myerror 'BindParamsByReference "Actual and Formal Parameter lists differ in length" actuals))
            ((and (eq? '& (car formals))
                  (or (number? (car evaluated)) (bool? (car evaluated)) (boolsym? (car evaluated))))
             (error 'BindParamsByReference "Cannot pass a literal value as a reference"))
            ((eq? '& (car formals)) ((f f) (cdr actuals) (cddr formals) bfenv bcurrentenv)) ;TODO
            (else ((f f) (cdr actuals)
                         (cdr formals)
                         (box (state-add-binding (car formals)
                                                 (m-value (car actuals) bcurrentenv  break continue throw prog-return)
                                                 (unbox bfenv)))
                         bcurrentenv))))))
     (evaluate-params-byreference paramlist bcurrentstate break continue throw '())
     (closure-paramlist closure)
     (box (state-push-scope ((closure-env closure) (unbox bcurrentstate))))
     bcurrentstate)))

; evaluate the parameters in the current context.
; details: evaluation of all parameters is done using the original current state,
; so if a parameter-expression modifies a global variable that is used by a later
; parameter, that change will not be reflected in the evaluation of the later param.
(define evaluate-params-byreference
  (lambda (paramlist bcurrentstate break continue throw savelist)
    (call/cc
     (lambda (evalbreak)
       (let ((do-eval-params-byreference ((lambda (m) (m m))
                                           (lambda (f)
                                             (lambda (l bs save)
                                               (cond
                                                 ((null? l) save)
                                                 ((pair? (car l)) ((f f) (cdr l) bs (myappend save
                                                                                              (cons (m-value (car l) bs  break continue throw (lambda (v s) (evaluate-params-byreference l (if (box? s) s (box s)) (myappend save (cons (break v) '()))))) '()))))
                                                 (else ((f f) (cdr l) bs (myappend save (cons (car l) '())))))))))) ; don't evaluate symbolic variable
         (do-eval-params-byreference paramlist bcurrentstate savelist))))))

; check the value returned from the function (mostly to enforce consistency with true/false vs #t/#f)
(define check-func-return
  (lambda (funcRetVal)
    (cond
      ((number? funcRetVal) funcRetVal)
      ((boolsym? funcRetVal) funcRetVal)
      ((bool? funcRetVal) (bool2sym funcRetVal))
      ((eq? noValue funcRetVal) funcRetVal)
      ((void? funcRetVal) noValue)
      (else (myerror 'EvaluateFunction "Unknown return type" funcRetVal)))))    

;===================================
; Mname

(define m-name
    (lambda (variable bstate)
        (if (state-lookup-inscope? variable (unbox bstate))
            variable
            (error 'Mname "Variable not found in the state"))))

;===================================
; functions to handle functions

; 'outer layer' of the interpreter: create a symbol table with global variable and function definitions.
; This will be the base layer of the program's environment. the 'table' is just a single-scope state.
(define create-table
  (lambda (parsetree btable)
    (cond
      ((null? parsetree) btable)
      ((or (eq? 'var (get-operator (car parsetree))) 
           (eq? 'function (get-operator (car parsetree)))) 
       (create-table (cdr parsetree) (m-state (car parsetree) btable default-brk default-con default-throw default-ret)))
      (else (error 'CreateTable "Can only have variable/function declarations at global level")))))

; create the function closure from the funtion-definition statement: (function <name> (<args>) (<body>))
(define create-function-closure
  (lambda (statement)
    (cons (funcdef-paramlist statement) (cons (funcdef-body statement)
                                              (cons (mk-create-func-env (funcdef-name statement)) '())))))

; return a function to create the function environment from the current environment
(define mk-create-func-env
  (lambda (fname)
    ((lambda (m) (m m))
     (lambda (r)
       (lambda (currentstate)
         (if (state-lookup-inscope? fname currentstate)
                               currentstate;(state-push-scope currentstate); done by bind-params now
                               ((r r) (state-pop-scope currentstate))))))))

; copy variables that are in scope in the calling environment from the function environment
(define environment-var-copy
    ((lambda (m) (m m))
      (lambda (f)
        (lambda (fenv currentstate vars)
          (cond
            ((null? vars) currentstate)
            ((or (not (state-lookup? (car vars) currentstate)) (not (state-lookup? (car vars) fenv))) ((f f) fenv currentstate (cdr vars)))
            ((list? (state-get-value (car vars) currentstate)) ((f f) fenv currentstate (cdr vars))) ; don't copy functions
            (else ((f f) fenv (state-assign (car vars) (state-get-value (car vars) fenv) currentstate) (cdr vars))))))))
(define global-vars-copy
  (lambda (fenv currentstate)
    (environment-var-copy fenv currentstate (state-all-global-vars currentstate))))
(define all-vars-copy
  (lambda (fenv currentstate)
    (environment-var-copy fenv currentstate (state-all-vars* currentstate))))
(define vars-not-inscope-copy
  (lambda (fenv currentstate)
    (environment-var-copy fenv currentstate (state-all-vars-not-inscope* fenv currentstate))))

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

; combine two lists
(define myappend
  (lambda (l1 l2)
    (((lambda (m) (m m))
      (lambda (f)
        (lambda (l1 l2 ret)
          (if (null? l1)
              (ret l2)
              ((f f) (cdr l1) l2 (lambda (v) (ret (cons (car l1) v))))))))
   l1 l2 (lambda (v) v))))

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

; get the (flat) length of a list
(define len (lambda (l) (apply + (map (lambda (x) 1) l))))
