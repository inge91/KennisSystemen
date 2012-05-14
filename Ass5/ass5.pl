
% the model system for ex. 1

system1(A, B, C, D, E):-
m1(A, C, X),
assert(predict(x/X),
m2(B, D, Y),
assert(predict(y/Y),
m3(C, E, Z),
assert(predict(y/Y),
a1(X, Y, F),
assert(predict(f/F),
a2(Y, Z, G),
assert(predict(g/G).


m1(A, C, X):-
    A * C = X.

m2(B, D, Y):-
    B * D = Y.

m3(C, E, Z):-
    C * E = Z.

a1(X, Y, F):-
    X+Y = F.

a2(Y, Z, G):-
    X+Y = F.


[[1,[A, C], multiply, X, m1], [1, [B,D], multiply, Y, m2], [1, [C,E], multiply, Z, m3],
 [2, [X,Y], add, F, a1], [2, [Y,Z],add, G,a2]]



