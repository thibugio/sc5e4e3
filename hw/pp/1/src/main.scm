; Rebecca Frederick
; EECS 345
; Programming Project 1

(load "../external/simpleParser.scm")
(load "mState.scm")

; abstractions for the parse tree
(define parsetree-pop cdr)
(define parsetree-peek car)
; extract the type of the statement (i.e., 'while', 'if',...)
(define statement-type car)

; takes a filename, calls parser with the filename, evaluates the parse tree
; returned by parser, and returns the proper value of the program state 
; (binding pairs)
(define interpret
    (lambda (f)
        (run-state (parser f) empty-state )))

; takes a parse tree 't' and initial state 's' and returns the program state
; Formally, a parse tree is a list where each sublist corresponds to a statement.
(define run-state
    (lambda (t s)
        (cond
            ((null? t) s)
            (else (m-state (parsetree-peek t) s) 

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

