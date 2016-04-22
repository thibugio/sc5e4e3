% math in prolog: 'is'

factorial(0, 1).
factorial(N, X) :- M is N - 1, factorial(M, Y), !, X is N * Y.

% allow a variable in either position of the predicate function
factorial2(0, 1).
factorial2(N, X) :- factorial2(M, Y), N is M + 1, X is Y * N.
% calculate factorial2(X,120):
% 1. 120 is not 1.
% 2. try M=0, Y=1. is 1 == 120? no.
% 3. try M=1, Y=1. is 1 == 120? no.
% 3. try M=2, Y=2. is 2 == 120? no.
% 3. try M=3, Y=6. is 6 == 120? no.
% ... need a CUT

% a cut is the proposition !. ! is always true, but prolog will not backtrack
% over the cut. Any variable that has a value when we cross the cut will keep
% that value. Can change variables on the left of the cut that have not yet
% been assigned a value, and variable on the right of the cut.

% does not work.
factorial3(0, 1).
factorial3(N, X) :- factorial3(M, Y), N is M + 1, !, X is Y * N.

% factorial3(5,X)
    % factorial3(M,Y) => try factorial3(0,1), which is a correct fact. assign M=0, Y=1.
    % N is M + 1 => N = 1, 1 != 5, no good!
    % !
    % X = 0, Y = 1. now backtrack

    % factorial3(1,1)
    % N is M + 1 => 5 is 1 + 1. try to backtrack-- we've already crossed the cut though so we can't!
