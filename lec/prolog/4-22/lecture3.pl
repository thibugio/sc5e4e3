
% A version of factorial that lets us place a variable in either position
%  of the predicate function.  It is less efficient than the one from
%  last lecture.
factorial2(0, 1).
factorial2(N, X) :-  factorial2(M, Y), N is M + 1, X is Y * N.

% A "cut" is the proposition !.  ! is always true, but prolog will not
% backtrack over the cut.  Any variable that has a value when we cross
% the cut will keep that value.
% We can use cut in last lecture's factorial so that it does not keep
% looking for a solution after it finds one.
% Because of how the recursion works, we can't find a good spot for cut
% in the above version of factorial.
factorial(0, 1).
factorial(N, X) :- M is N - 1, factorial(M, Y), !, X is N * Y.
