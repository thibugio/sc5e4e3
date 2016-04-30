% look up a value in a state:    lookup name state
% remove a binding from a state:  remove_binding name state
% add a binding to a state:       add_binding name value state
% a state is of the form [[x, 5], [y, 10], [z, 3]]

lookup(N, [[N,V]|_], V) :- !.
lookup(N, [_|T], V) :- lookup(N, T, V). 

add_binding(N, V, S, [[N,V]|S]).

remove_binding(N, [], []).
remove_binding(N, [[N,_]|S], S) :- !.
remove_binding(N, [H|T], [H|S2]) :- remove_binding(N, T, S2).

% M_value takes [x, +, 5]
M_value([E1, +, E2], S, V) :- M_value(E1, S, V1), M_value(E2, S, V2), V is V1 + V2.
M_value([E1, *, E2], S, V) :- M_value(E1, S, V1), M_value(E2, S, V2), V is V1 * V2.
M_value(E, _, E) :- number(E).
M_value(E, S, V) :- lookup(E, S, V).

% M_boolean takes [x, <, 10]
% M_boolean takes [a, &&, b]
M_boolean([E1, <, E2], S) :- M_value(E1, S, V1), M_value(E2, S, V2), V1 < V2.

% statement list
M_state([], S, S).
M_state([H|T], S, S2) :- M_state(H, S, S1), M_state(T, S1, S2).

% assignment
M_state([N, =, V], S, S2) :- M_value(V, S, V1), remove_binding(N, S, S1), add_binding(N, V1, S1, S2).

% while needs 2 cases: the condition is true and the condition is not true (use !)
