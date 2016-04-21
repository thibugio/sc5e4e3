% Facts about the Weasley family
% Facts end in . and are all lowercase.
% Facts should be grouped together.

male(ron).
male(percy).
male(george).
male(fred).
male(charlie).
male(bill).
male(arthur).
male(harry).
male(jamesjr).
male(albusjr).
male(hugo).

female(ginny).
female(molly).
female(hermione).
female(fleur).
female(lilyjr).
female(rose).
female(veronique).


parentof(arthur, ron).
parentof(molly, ron).
parentof(arthur, george).
parentof(molly, george).
parentof(arthur, bill).
parentof(molly, bill).
parentof(arthur, charlie).
parentof(molly, charlie).
parentof(arthur, fred).
parentof(molly, fred).
parentof(arthur, ginny).
parentof(molly, ginny).
parentof(ginny, lilyjr).
parentof(ginny, albusjr).
parentof(ginny, jamesjr).
parentof(harry, lilyjr).
parentof(harry, albusjr).
parentof(harry, jamesjr).
parentof(ron, hugo).
parentof(ron, rose).
parentof(hermione, hugo).
parentof(hermione, rose).
parentof(bill, veronique).
parentof(fleur, veronique).

married(harry, ginny).
married(fleur, bill).
married(hermione, ron).
married(arthur, molly).
married(ginny, harry).
married(bill, fleur).
married(ron, hermione).
married(molly, arthur).

% Rules are of the form A :- B,C,D.
% meaning A <- B and C and D

grandparentof(X,Y) :- parentof(X,Z),parentof(Z,Y).
sibling(X,Y) :- parentof(Z,X),parentof(Z,Y),X\=Y.
uncleof(X,Y) :- parentof(Z,Y),sibling(X,Z),male(X).
uncleof(X,Y) :- parentof(Z,Y),sibling(W,Z),male(X),married(W,X).
%auntof(X,Y) :- 

% Lists.  [] is the empty list
%         [a,b,c] is a list of 3 elements
%         [H|T] is a list with at least one element, car H and cdr T
% myappend(list1, list2, result).
myappend([],L,L).
myappend([H | T], L, [H | L2]) :- myappend(T, L, L2).

