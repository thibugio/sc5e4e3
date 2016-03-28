(define factorial
  (lambda (n)
    (if (zero? n) 
        1
        (* n (factorial (- n 1))))))

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
  ((lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (- n 1))))))
   ((lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (- n 1))))))
    ((lambda (fact)
       (lambda (n)
         (if (zero? n)
             1
             (* n (fact (- n 1))))))
     ((lambda (fact)
        (lambda (n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))))
      crash)))))

(define factorial3
  ((lambda (mkfact)
     (mkfact (mkfact (mkfact (mkfact crash)))))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n (fact (- n 1))))))))

(define factorial4
  ((lambda (mkfact)
     (mkfact mkfact))
   (lambda (fact)
     (lambda (n)
       (if (zero? n)
           1
           (* n ((fact fact) (- n 1))))))))