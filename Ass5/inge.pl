:-dynamic predictedValue/2.
:-dynamic probedValue/2.
:-dynamic test/2.

input(a,3).
input(b,2).
input(c,2).
input(d,3).
input(e,2).

output(f,10).
output(g,12).
backprop([X/Y|T], [Errors|Error]):-
    predictedValue(X, Z),
    writef("Value of output %t seems incorrect.\nEstimated output: %t \nMeasured

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

% backpropagation
backprop(Err):-
    findall(X/Y, output(X,Y), Output),
    backprop(Output, Errors),
    flatten(Errors, E),
    elimenate_errors(E, Err).

% measured output, go forth with measured output
% both basecases
backprop([], []).

backprop([X/Y|T], Error):-
    predictedValue(X, Y),
    write("Value seems ok\n"),
    backprop(T, Error).

backprop([X/Y|T], [Errors|Error]):-
    predictedValue(X, Z),
    writef("Value of output %t seems incorrect.\nEstimated output: %t \nMeasured
        output: %t\n",
    [X, Y, Z]),
        output: %t\n Extracting conflict set...",
    % front is last function, end is first function
    create_errorset(X, Errors),
    backprop(T, Error).
    
% print all elements
% combine values with a slash 

% print all elements
print_elements([]).

print_elements([H|T]):-
    write(H),
    print_elements(T).

combine_values([], [], []).

combine_values([H1|L1], [H2|L2], [H1/H2|L3]):-
    combine_values(L1, L2, L3).

% create errorset creates an errorset
% for the given element
create_errorset(Value, PreviousValues ):-
    findall_values(Value, PreviousValues).

% basic case: we reached only input values
findall_values(H, Prev):-
    findall(X, component(_,_,X,_,H), List),
    findall(X, component(_,_,_,X,H), List2),
    append(List, List2, List3),
    all_input_values(List3),
    component(Prev, _,_,_, H),
    writef("Added %t to errorlist\n", [H]).

% findall pevious values until we reach input
findall_values(H, [Prev|PreviousValues]):-
    component(Prev, _,_,_,H),
    writef("Added %t to errorlist\n", [H]),
    findall(X, component(_,_,X,_,H), List),
    findall(X, component(_,_,_,X,H), List2),
    append(List, List2, List3),
    findall_values_list(List3, PreviousValues).

% works the same way as findall values, but
% uses it with a list
findall_values_list([], []).    

findall_values_list([H|T], [X|PreviousValues]):-
    findall_values(H, X),
    findall_values_list(T, PreviousValues).
    
% get name of component when having their input arguments
get_components([], []).

get_components([H|T], [X| Values]):-
    (component(X,_,H,_, _);
    component(X,_,_,H, _)),
    get_components(T, Values).

% all values in the list are input values
all_input_values([]).

all_input_values([H|T]):-
    input(H,_),
    all_input_values(T).

% Elimenate components that are not
% faulty by probing for values 
% First arguments components
% NEEEEEEDS ENTROPHY!!! (choose the most middle component or something?)

elimenate_errors([], []).

% if there is already a probed value
% don't start asking dumb questions!
%elimenate_errors([H|T], Tail):-
%    component(H,_,_,_,Output),
elimenate_errors([H|T], Tail):-
    component(H,_,_,_,Output),
    (probedValue(Output, _);
     output(Output,_ )),   
    elimenate_errors(T, Tail).
% as probe then component that
% calculated it is not at fault, just like all before it.
elimenate_errors([H|T], [AllBad|Tail]):-
    component(H,_,A,B,Output),
    predictedValue(Output, Val),
    writef("What do you measure at point %t", [Output]),
    read(NewVal),
    assert(probedValue(Output, NewVal)),
    ((
    probedValue(Output, Val),
    write('Probe has same value as predicted. \n All components that came before
    will be added to the good set'),
    predecessors(H, Preds),
    append([H], Preds, Preds2),
    remove_good(Preds2, T, NewT),
    AllBad = [],
    % remove everything before,
    % this component as well?,
    elimenate_errors(NewT, Tail));
    (
    % if the probed value is not equal to the predicted value
    \+probedValue(Output, Val),
    % both are input nodes
    input(A,_),
    input(B,_),
    writef("%t is malfunctioning", [H]),
    % we certainly know now the model is not functioning and is set above
    % we do a check forward to see if more elements are not functioning(Bad)
    findall(X, component(X, _, Output, _, _), L),
    findall(X, component(X, _, _, Output, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good, Bad),
    flatten(Bad, Bad2),
    append([H], Bad2, AllBad),
    % the Good ones are elimenated from the T set
    remove_good(Good, T,  T2),
    elimenate_errors(T2, Tail));
    (
    % if the probed value is not equal to the predicted value
    \+probedValue(Output, Val),
    % we do a check forward to see if more elements are not functioning(Bad)
    check_forward(H, Good, Bad),
    append(H, Bad, AllBad),
    % the Good ones are elimenated from the T set
    remove_goods(Good,T,  T2),
    elimenate_errors(T2, Tail))).
    


check_forward([],[],[]).

% basecase: H is here a component that
% leads to an output element
% if it is calculated well it gets added to Good
check_forward(H, H, []):-
    component(H, Type,X,Y,Out),
    output(Out, R),
    %all different possibilities... Yes, it is ugly
    ((test(X, XVal),
     test(Y, YVal));
    (test(X, XVal),
     probedValue(Y, YVal));
    (test(Y, YVal),
     probedValue(X, XVal));
    (test(X, XVal),
     predictedValue(Y, YVal));
    (test(Y, YVal),
     predictedValue(X, XVal));
    (probedValue(X, XVal),
    probedValue(Y, YVal));
    (probedValue(X, XVal),
    predictedValue(Y, YVal));
    (probedValue(Y, YVal),
    predictedValue(X, XVal));
    (predictedValue(X, XVal),
    predictedValue(Y, YVal))),!,
    calculate(XVal, YVal, Type, R).

    
% if both values are probedvalues (very important!)
% and the value wont give the right output the component is bad
check_forward(H, [], [H]):-
    component(H, Type,X,Y,Out),
    output(Out, R),
    (probedValue(X, XVal),
    probedValue(Y, YVal)),
    calculate(XVal, YVal, Type, Some),
    Some =\= R.

% nothing to do
check_forward(H,[], []):-
    component(H, _,_,_,Out),
    output(Out, _).

% not an output component
% but output is probed and
% is not the same as tested
check_forward(H, _, H):-
    component(H, Type,X,Y,Out),
    probedValue(Out, R),
    (probedValue(X, XVal),
    probedValue(Y, YVal)),
    calculate(XVal, YVal, Type, Some),
    Some =\= R.

check_forward(H, [H|Good], Bad):-
    component(H, Type,X,Y,Out),
    probedValue(Out, R),
    ((test(X, XVal),
     test(Y, YVal));
    (test(X, XVal),
     probedValue(Y, YVal));
    (test(Y, YVal),
     probedValue(X, XVal));
    (test(X, XVal),
     predictedValue(Y, YVal));
    (test(Y, YVal),
     predictedValue(X, XVal));
    (probedValue(X, XVal),
    probedValue(Y, YVal));
    (probedValue(X, XVal),
    predictedValue(Y, YVal));
    (probedValue(Y, YVal),
    predictedValue(X, XVal));
    (predictedValue(X, XVal),
    predictedValue(Y, YVal))),!,
    calculate(XVal, YVal, Type, R),
    findall(X, component(X, _, Out, _, _), L),
    findall(X, component(X, _, _, Out, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good, Bad).


% not an output component
% and not able to say if good or bad
% (just working through it)
check_forward(H, Good, Bad):-
    component(H, Type,X,Y,Out),
    ((test(X, XVal),
     test(Y, YVal));
    (test(X, XVal),
     probedValue(Y, YVal));
    (test(Y, YVal),
     probedValue(X, XVal));
    (test(X, XVal),
     predictedValue(Y, YVal));
    (test(Y, YVal),
     predictedValue(X, XVal));
    (probedValue(X, XVal);
    probedValue(Y, YVal)),
    (probedValue(X, XVal),
    predictedValue(Y, YVal));
    (probedValue(Y, YVal),
    predictedValue(X, XVal));
    (predictedValue(X, XVal),
    predictedValue(Y, YVal))),!,
    calculate(XVal, YVal, Type, Val),
    assert(test(Out, Val)),
    findall(X, component(X, _, Out, _, _), L),
    findall(X, component(X, _, _, Out, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good, Bad).


check_forward_list([H|T], [NewGood|Good], [NewBad|Bad]):-
    check_forward(H,  NewGood, NewBad),
    check_forward(T, Good, Bad).




calculate(X, Y, mult, Out):-
    Out is X * Y.
    
calculate(X, Y, add , Out):-
    Out is X + Y.



% gives all components that come before H
% (and are linked with H)
predecessors(H, []):-
    component(H,_, X, Y, _),
    (input(X,_),!,
    input(Y, _)).

predecessors(H, [P|Q]):-
    component(H,_, X, Y, _),
    (component(P, _, _, _, X),
    input(Y, _));
    (component(P, _, _, _, Y),
    input(X, _)),
    predecessors(P, Q).
    
predecessors(H, L):-
    component(H,_, X, Y, _),
    component(P, _, _, _, X),
    component(Q, _, _, _, Y),!,
    predecessors_list([P|Q], L).
    
predecessorts_list([],[]):-!.
predecessors_list([H|T], [L2|L]):-
    predecessors(H, L2),!,
    conc(L2, H, L2),
    predesserts_list(T, L).

% remove all goods from bad set

remove_good(_, [], []):-!.

remove_good(_, [], []).
remove_good(Goods, [H|Bads], NewBad):-
    member(H, Goods),!,
    remove_good(Goods, Bads, NewBad).
remove_good(Goods, [H|Bads], [H|NewBad]):-
    \+member(H, Goods),!,
    remove_good(Goods, Bads, NewBad).

