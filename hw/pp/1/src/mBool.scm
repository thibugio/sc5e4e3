; collection of functions used for M_bool

; M_bool function
; returns whether the given condition is satisfied in the given state
(define m-bool
    (lambda (condition state) 
        (cond
            ((eq? 'true  (get-operator condition)) #t)
            ((eq? 'false (get-operator condition)) #f)
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
            ((eq? '||    (get-operator condition)) (or (m-bool (get-operand1 condition) state)
                                                       (m-bool (get-operand2 condition) state)))
            ((eq? '!     (get-operator condition)) (not (m-bool (get-operand1 condition) state))))))
        

(define get-operator car)
(define get-operand1 cadr)
(define get-operand2 caddr)
