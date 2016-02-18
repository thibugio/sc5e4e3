; First, a normal recursion example
(define factorial
  (lambda (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; Re-write factorial with tail recursion using accumulator passing style.
;  We call the function with (factorial-acc 6 1).
(define factorial-acc
  (lambda (n acc)
    (if (zero? n)
        acc
        (factorial-acc (- n 1) (* n acc)))))

; Another tail recursive version of factorial, this time using continuation passing style.
;  We call the function wiht (factorial-cps 6 (lambda (v) v))
(define factorial-cps
  (lambda (n return)
    (if (zero? n)
        (return 1)
        (factorial-cps (- n 1) (lambda (v) (return (* n v)))))))    ; v stores the value returned from the recursive call to factorial with n - 1.

; sumnumbers using accumulator passing style.  Call with (sumnumbers-acc list 0)
(define sumnumbers-acc
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((number? (car l)) (sumnumbers-acc (cdr l) (+ (car l) acc)))
      (else (sumnumbers-acc (cdr l) acc)))))