% append two lists
% myappend (list1, list2, resultlist) is true if and only if resultlist
%   is the result of appending list1 onto list2

myappend([],L,L).
myappend([H|T],L,[H|R]) :- myappend(T, L, R).

%(define myappend
%  (lambda (l1 l2)
%    (if (null? l1)
%         l2
%        (cons (car l1) (myappend (cdr l1) l2)))))

% contains: is x in a list?
% contains (x, L) => true if x is in L
% _ means "don't care"

contains(X, [X|_]).
contains(X, [_|T]) :- contains(X,T).

%(define contains
%   (lambda (x l)
%     (cond
%       ((null? l) #f)
%       ((eq? x (car l)) #t)
%       (else (contains x (cdr l))))))

% insertbefore a x [w,x,y,z]  =>  [w,a,x,y,z]
insertbefore(_,_,[],[]).
insertbefore(A,X,[X|T],[A,X|T]).
insertbefore(A,X,[H|T1],[H|T2]) :- insertbefore(A,X,T1,T2).

% insertbeforeall a x [w,x,y,x,z,x]  =>  [w,a,x,y,a,x,z,a,x]
insertbeforeall(_,_,[],[]).
insertbeforeall(A,X,[X|T1],[A,X|T2]) :- insertbeforeall(A,X,T1,T2).
insertbeforeall(A,X,[H|T1],[H|T2]) :- insertbeforeall(A,X,T1,T2).

% try this one: flatten

% "is" is used for numeric computation, and the right side must be resolved
% at the time you use "is"
factorial(0,1).
factorial(N,X) :- M is N - 1, factorial(M,Y), X is Y * N.
