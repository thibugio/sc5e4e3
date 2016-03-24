(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; can we do recursion without recursive bindings?
; crash is a placeholder for factorial => end of recursion
(define crash
  (lambda (n)
    (error 'crashed)))

(define factorial1
  (lambda (n)
    (if (zero? n)
        1
        (* n ((lambda (n)
               (if (zero? n)
                   1
                   (* n ((lambda (n)
                           (if (zero? n)
                               1
                               (* n ((lambda (n)
                                       (if (zero? n)
                                           1
                                           (* n (crash (- n 1)))))
                                     (- n 1)))))
                         (- n 1)))))
              (- n 1))))))

(define factorial2
  ((lambda (fact) ; takes a function 'fact' as input and returns a function
     (lambda (n) ; the function that gets bound to factorial2 when applied to the function supplied
       (if (zero? n) ; (factorial2 0) => this 'n' gets the 0
           1
           (* n (fact (- n 1))))))
   ((lambda (fact) ; need to pass 'fact' in as a function
      (lambda (n)
        (if (zero? n) ; (factorial2 1) => this 'n' gets the 0
            1
            (* n (fact (- n 1))))))
    ((lambda (fact)
       (lambda (n)
         (if (zero? n) ; (factorial2 2) => this 'n' gets the 0
             1
             (* n (fact (- n 1))))))
     ((lambda (fact)
        (lambda (n)
          (if (zero? n) ; (factorial2 3) => this 'n' gets the 0, but this 'fact' is 'crash'
              1
              (* n (fact (- n 1))))))
      crash)))))

(define factorial3
  ((lambda (mkfact) ;(lambda (fact)...) is 'mkfact'; call it on 'crash' => 'fact' is replaced by 'crash'
     (mkfact (mkfact (mkfact (mkfact crash))))) ; pass a copy of itself into itself
   (lambda (fact) ; takes a function, which is the next layer of the recursion
     (lambda (n) ; factorial function that gets bound to factorial3
       (if (zero? n)
           1
           (* n (fact (- n 1))))))))

; namebinding not needed here at all
(((lambda (mkfact) 
     (mkfact (mkfact (mkfact (mkfact crash)))))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (- n 1)))))))
 3) ; => 6

(define factorial4
  ((lambda (mkfact) 
     (mkfact mkfact))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1)))))))) ; contract violation (* n (fact (- n 1))))): 'fact' takes a function, not a number
                                            ; (fact fact) => calling 'mkfact' again on the function
                                            ; takes itself, passes it in as 'fact', replaces itself, and returns a (lambda (n))
                                            ; still doing loop-unrolling, but can do it as needed
                                            ; able to create the next layer of code when it needs it!

; still do not need any name-binding:
(((lambda (mkfact) 
     (mkfact mkfact))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1)))))))
 6) ; => 720

; do not need non-functional bindings for recursion!
; recursion is just a function that replaces itself with another function
   
     
