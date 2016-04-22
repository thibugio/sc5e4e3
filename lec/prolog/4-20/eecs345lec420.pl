% myappend: append two lists

% (define myapppend
%    (lambda (l1 l2)
%        (if (null? l1) 
%            l2
%            (cons (car l1) (myappend (cdr l1) l2)))))

% myappend (list1, list2, resultlist) is true if and only if resultlist is the result of appending list1 and list2
% read ':-' as 'results from', or equivalently, 'if myappend(T,L,R) then myappend([H|T],L,[H|R])'
%myappend([], L, L).
%myappend([H|T],L,[H|R]) :- myappend(T, L, R).



% contains (X, L) => true if X is in L

%(define contains
%    (lambda (x l)
%        (cond
%            ((null? l) #f)
%            ((eq? x (car l)) #t)
%            (else (contains x (cdr l))))))

contains(X, [X|_]). %T is unneeded (doesn't need to be resolved). Use wildcard 'dont care' symbol _.
contains(X, [_|T]) :- contains(X, T).
%contains(X,[]). false does not exist in prolog; get the null case by not including the rule.



% insertbefore (a, x, [w, x, y, z]) => [w, a, x, y, z]
insertbefore(_, _, [], []).
insertbefore(A, X, [X|T], [A,X|T]).
insertbefore(A, X, [H|T1], [H|T2]) :- insertbefore(A, X, T1, T2).



% insertbeforeall (a, x, [w, x, y, x, z, x]) => [w, a, x, y, a, x, z, a, x]
insertbeforeall(_, _, [], []).
insertbeforeall(A, X, [X|T1], [A,X|T2]) :- insertbeforeall(A, X, T1, T2).
insertbeforeall(A, X, [H|T1], [H|T2]) :- insertbeforeall(A, X, T1, T2).



% flatten



% factorial
% "is" is used for numeric compuation, and the right side MUST be resolved at the time you use "is" by the time you get to the left.
factorial(0,1).
factorial(N,X) :- M is N - 1, factorial(M,Y), X is Y * N.
