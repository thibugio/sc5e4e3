; collection of functions used for M_value

(define get-operator car)
(define get-operand1 cadr)
(define get-operand2 caddr)

; Mvalue takes a syntax rule and a state, and returns a numeric value (or error condition)
(define m-value
    (lambda (statement state)
       (cond
            ((number? statement) statement)
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
            ;((eq? '= (get-operator statement)) (m-value (get-operand2 statement statement) state))
            ((number? (state-get-value statement state)) (state-get-value statement state))
            (else (error 'Unknown "Mvalue: Unknown statement"))
