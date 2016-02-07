; Rebecca Frederick
; EECS 345
; Programming Project 1

(load "../external/simpleParser.scm")

; takes a filename, calls parser with the filename, evaluates the parse tree
; returned by parser, and returns the proper value of the program state 
; (binding pairs)
(define interpret
    (lambda (f)
        (run-state (parser f))))

; takes a parse tree 't' and returns the program state
; Formally, a parse tree is a list where each sublist corresponds to a statement.
(define run-state
    (lambda (t)
        (state-acc t '() )))

; accumulate the program state given its parse tree
(define state-acc
    (lambda (t state)
        (cond
            ((null? (car t)) state )
            (else (cons (m-state (car t) state) (state-acc (cdr t state)))))))

; extract the type of the statement (i.e., 'while', 'if',...)
(define statement-type car)

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
