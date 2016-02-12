; collection of functions used for Mname

(load "mStateCore.scm")

(define m-name
    (lambda (variable state)
        (if (state-lookup? variable state)
            variable
            (error 'Mname "Variable not found in the state"))))
