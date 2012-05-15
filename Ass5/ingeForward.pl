:-dynamic predictedValue/2.

input(a,3).
input(b,2).
input(c,2).
input(d,3).
input(e,3).

output(f,10).
output(g,12).

%wat wij uitrekenen (forward propagation)
%predictedValue(x,6)

%wat de user heeft gemeten
%probedValue(x,4)

%component(Serial, Element, Input1, Input2,output)
component(m1, mult, a, c, x).
component(m2, mult, b, d, y).
component(m3, mult, c, e, z).
component(a1, add, x, y, f).
component(a2, add, y, z, g).

diagnose:-
       forwardPropagation,
       findall(X, (output(X,Y), predictedValue(X,Y2), not(Y == Y2)) , WrongOutputs),
       write('Wrong outputs:'), write(WrongOutputs).


forwardPropagation:-
	findall(X , component(X, _, _, _, _), Components),
	forward(Components),!.

forward([]):-!.

forward(ComponentsToCheck):-
	findall(X , (input(X,_);predictedValue(X,_)), Knownodes),
	findall(X,(component(X, _, X1, X2,X3), member(X1,Knownodes), member(X2,Knownodes), \+member(X3,Knownodes)),NewComponents),
        member(X, NewComponents),
	predictComponentOutcome(X),
	exclude(ComponentsToCheck,[X], ComponentsToCheck2),
	forward(ComponentsToCheck2).

%for the multiply Component
predictComponentOutcome(X):-
	component(X, mult, A, B, C),
	(input(A,A2);predictedValue(A,A2)),
        (input(B,B2);predictedValue(B,B2)),
	Value is A2*B2,
	assert(predictedValue(C,Value)).

%for the added Component
predictComponentOutcome(X):-
	component(X, add, A, B, C),
	(input(A,A2);predictedValue(A,A2)),
        (input(B,B2);predictedValue(B,B2)),
	Value is A2+B2,
	assert(predictedValue(C,Value)).

% returns all elements from the first list
% that are not in the second list
exclude([], _, []).

exclude([H|L], L2, [H|T]):-
    \+member(H, L2),!,
    exclude(L, L2, T).

exclude([H|L], L2, T):-
    member(H, L2),!,
    exclude(L, L2, T).





