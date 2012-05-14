input(a,3).
input(b,2).
input(c,2).
input(d,3).
input(e,2).

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



% backpropagation
backprop:-
    findall(X/Y, output(X,Y), Output),
    backprop(Output).

backprop([], []).

% in case the predicted output is the same as the
% measured output, go forth with measured output
backprop([X/Y|T], ErrorSet):-
    predictedValue(X, Y),
    backprop(T, ErrorSet).

backprop([X/Y|T], ErrorSet):-
    % output X is not as predicted
    \+predictedValue(X, Y),
    % everything before X is put in the errorset
    create_errorset(X, ErrorSet),
    findall(F, component(_, _, F, F, X) , PreList).

% combine values with a slash 
combine_values([], [], []).

combine_values([H|L1],)[H2|L2], [H1/H2|L3]):-
    combine_values(L1, L2, L3).

% create errorset creates an errorset
% for the given element
create_errorset(F, [L|ErrorSet] ):-
    findall_values(F, L)
    findall(X, component())
