; A function computing the denotational semantics for the value of an expression.
;  The function uses abstraction for the operand and operator locations in the expression
;  The abstraction makes the code easier to read and easier to modify should we need to
;  change expressions between infix, prefix, or postfix notation
(define M_value
  (lambda (expression)
    (cond
      ((number? expression) expression)
      ((eq? '+ (operator expression)) (+ (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '- (operator expression)) (- (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '* (operator expression)) (* (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '/ (operator expression)) (quotient (M_value (operand1 expression)) (M_value (operand2 expression))))
      ((eq? '% (operator expression)) (remainder (M_value (operand1 expression)) (M_value (operand2 expression))))
      (else (error 'unknown "unknown expression")))))

(define operator caddr)

(define operand1
  (lambda (e)
    (car e)))

(define operand2 cadr)
      