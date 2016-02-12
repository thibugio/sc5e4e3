Welcome to DrRacket, version 6.3 [3m].
Language: Pretty Big; memory limit: 128 MB.
> (pull-out-numbers2 '(a 1 2 b 3) (lambda (v1 v2) (cons v1 (cons v2 '()))))
((1 2 3) (a b))
> (pull-out-numbers2 '(a 1 2 b 3) (lambda (v1 v2) (append v1 v2)))
(1 2 3 a b)
> 