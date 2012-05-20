%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Ass 5                   %%
%%                          %%
%%  Richard Pronk           %%
%%                          %%
%%  Inge Becht              %%
%%                          %%
%%                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% First things first:
% Some awesome ascii art(ok, not that awesome) to represent the component configuration of ex2
%
% Ex2:
%
%3 a---
%       m1 --- 6 k ---
%2 b---
%                      a1 ---12  p
%2 c---                           \
%       m2 --- 6 l ---             \
%3 d---                             \
%                                     m6 --- 120 s  GIVEN OUTPUT = 100
%2 e---                             / 
%       m3 --- 6 m ---             /
%3 f---                           /
%                      a2 --- 10 q   
%2 g---                           \
%       m4 --- 4 n ---             \           
%2 h---                            /  m7 --- 100 t  GIVEN OUTPUT = 120
%                      a3 --- 10 r
%3 i---
%       m5 --- 4 o ---
%2 j---
%   
% the first column is input, the last one output
% m1 = multiply and a1 = add
% The value in front of each number is the predicted value given the input
%
%

% now for how the code works.
% To start the code just use the ex1/0 and ex2/0 predicates.
%
% When the user starts one of these predicates we first do the forward
% propagation. This calculates the values as they are predicted given the input
% facts you can see below (input/2). When these predicted values (the
% predictedValue/2 facts) are asserted, we look at which output doesnt match the
% estimated output. After we established this we take all the components that
% come before these wrong outputs and include them ti the conflict set.
% We do not deal in minimum conflict sets in an explicit way, our program works
% this out for himself quiet nicely.
% To ensure the output of most informative components is probed first we 
% sort the components on most occuring. This works because when gathering all
% possible malfunctioning components we sometimes get duplicates (look at
% comments by findall_values/2 to understand why this happens) and these
% duplicates are most informative. This seems to work well.
%
% now that we have a list of components that are malfunctioning we go into elimenate_errors/2
% with each of these components
% . If its output is not an output element we ask the user to measure what
% his value is at that point (we probe the value). Now there are 3
% possibilities:
% 1. The probed value is the same as the measured value. When this happens
%    we choose to take all component coming before and including this one
%    into the good set. These are considered good(although there is a possiblity
%    that two malfunctioning components can give a good answer in the end, but
%    we use this naive technique so we do not have to probe all components).
% 2. The probed value is not the same as the measured value, and the component
%    takes only input elements. When this happens the component is permanently
%    added to the bad set.
% 3. The probed value is not the same as the measured valuem and the component
%    does not take only input components. In this case we can say nothing about
%    the correctnet of the component .
%
% All these cases are followed by the same step: forward_check/2
% The forward check walks from the just probed component output to the output
% element and checks if these are good/bad/not detemined. We only consider an
% component bad if both input elements are probed and its output element does
% not match the calculated value. The good set does NOT need only probed values
% in its input to be consired good. This is why in the case of for example x2
% when the faulty output s = 100 ans t = 120 and we say the measured value at q
% = 10 and the measured value at p = 10 and k = 4 this is sufficient to see that
% l is working fine, although its value is never probed( measured l =6 and added
% to probed value 4 gets us 10 as we would expect).
%
% To understand more about the inner working read the comments at the predicates


:-dynamic predictedValue/2.
:-dynamic probedValue/2.
:-dynamic test/2.
:-dynamic input/2.
:-dynamic output/2.
:-dynamic component/5.

reset:-
    retractall(predictedValue(_,_)),
    retractall(probedValue(_,_)),
    retractall(test(_,_)). 

setCircuit1:-
    retractall(input(_,_)),
    retractall(output(_,_)),
    retractall(component(_, _, _, _, _)),
    assert(input(a,3)),
    assert(input(b,2)),
    assert(input(c,2)),
    assert(input(d,3)),
    assert(input(e,3)),
    assert(output(f,10)),
    assert(output(g,12)),
    assert(component(m1, mult, a, c, x)),
    assert(component(m2, mult, b, d, y)),
    assert(component(m3, mult, c, e, z)),
    assert(component(a1, add, x, y, f)),
    assert(component(a2, add, y, z, g)).
    
setCircuit2:-
    retractall(input(_,_)),
    retractall(output(_,_)),
    retractall(component(_, _, _, _, _)),
    assert(input(a,3)),
    assert(input(b,2)),
    assert(input(c,2)),
    assert(input(d,3)),
    assert(input(e,2)),
    assert(input(f,3)),
    assert(input(g,2)),
    assert(input(h,2)),
    assert(input(i,3)),
    assert(input(j,2)),
    assert(output(s,100)),
    assert(output(t,120)),
    assert(component(m1, mult, a, b, k)),
    assert(component(m2, mult, c, d, l)),
    assert(component(m3, mult, e, f, m)),
    assert(component(m4, mult, g, h, n)),
    assert(component(m5, mult, i, j, o)),
    assert(component(m6, mult, p, q, s)),
    assert(component(m7, mult, q, r, t)),
    assert(component(a1, add, k, l, p)),
    assert(component(a2, add, m, n, q)),
    assert(component(a3, add, n, o, r)).
       
ex1:-
    setCircuit1,
    forwardPropagation,
    backprop,!,
    reset.

ex2:-
    setCircuit2,
    forwardPropagation,
    backprop,!,
    reset.

/* DE-COMMENT IF PROGRAM DOENS"T WORK ANYMORE!
backprop([X/Y|T], [Errors|Error]):-
    predictedValue(X, Z),
    writef("Value of output %t seems incorrect.\nEstimated output: %t \nMeasured
*/

%The calculated values (forward propagation)
%predictedValue(x,6)
%
%The measured values
%probedValue(x,4)
%

diagnose:-
       forwardPropagation,
       findall(X, (output(X,Y), predictedValue(X,Y2), not(Y == Y2)) , WrongOutputs),
       write('Wrong outputs:'), write(WrongOutputs).

%makes sure all components are used
forwardPropagation:-
	findall(X , component(X, _, _, _, _), Components),
	forward(Components),!.

%continue calculation new nodes till all components are used
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
backprop:-
    findall(X/Y, output(X,Y), Output),
    backprop(Output, Errors),
    flatten(Errors, E),
    doubles(E, D),
    unique(E, D, U),
    append(D, U, Sorted),
    elimenate_errors(Sorted, Err),
    flatten(Err, FinalErrors),
    write('\nThe following components are malfunctioning:\n'),
    unique2(FinalErrors, FinalErrors2),
    printlist(FinalErrors2).

printlist([]).
printlist([H]):-
    write(H),!.

printlist([H|T]):-
    write(H),!,
    write(', '),
    printlist(T).


% addes element to the output lists that are double
doubles([], []).

doubles([H|T], [H|U]):-
    member(H, T),
    doubles(T, U).

doubles([_|T], U):-
    doubles(T, U).

% only add unique values
unique([], _, []).
unique([H|E], D, [H|U]):-
    \+member(H, D),
    unique(E, D, U).

unique([_|E], D, U):-
    unique(E, D, U).

unique2([], []).

unique2([H|T], [H|X]):-
    \+member(H, T),!,
    unique2(T, X).

unique2([_|T], X):-
    unique2(T,X),!.


% measured output, go forth with measured output
% both basecases
backprop([], []).

backprop([X/Y|T], Error):-
    predictedValue(X, Y),
    backprop(T, Error).

backprop([X/Y|T], [Errors|Error]):-
    predictedValue(X, Z),
    writef("Value of output %t seems incorrect.\nEstimated output: %t \nMeasured output: %t\n", [X, Z, Y]),
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

% create errorset 
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
    writef("Added %t to errorlist\n", [Prev]).

% findall pevious values until we reach input
findall_values(H, [Prev|PreviousValues]):-
    component(Prev, _,_,_,H),
    writef("Added %t to errorlist\n", [Prev]),
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
% or most often in the list

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
    \+output(Output, _),
    % do some probing
    writef("\n\nWhat do you measure at point %t\n >", [Output]),
    read(NewVal),
    assert(probedValue(Output, NewVal)),
    ((
    probedValue(Output, Val),
    write('\nProbe has same value as predicted.\n
    All components that came before will be added to the good set:\n'),
    % we want all the predecessors because they work good
    predecessors(H, Pred),
    printlist(Pred),
    flatten(Pred, Preds),
    append([H], Preds, Preds2),
    %all elements we need to check forwrad
    findall(X, component(X, _, Output, _, _), L),
    findall(X, component(X, _, _, Output, _), L2),
    append(L, L2, L3),
    write('\nChecking for other dependencies...'),
    %check what is good or what is bad
    check_forward_list(L3, Good2, Bad),
    flatten(Bad, AllBad),
    flatten(Good2, Good3),
    % we want all the predecessors from elements that
    % seem to be good from check_forward_list
    predecessors_list(Good3, Good4),
    flatten(Good4, Good5),
    predecessors_list(AllBad, BadPred),
    flatten(BadPred, BadPred2),
    %elements we know are bad should STAY bad
    exclude_second(BadPred, Good5, Good), 
    append(Preds2, Good, Preds3),
    write('\nThe following components are now in the good set:\n'),
    printlist(Preds3),
    write('\nThe following components are now in the definite bad set:\n'),
    printlist(AllBad),
    %exclude all elements we know are good from probing
    remove_good(Preds3, T, NewT),
    % remove everything before,
    % this component as well?,
    retractall(test(_,_)),
    elimenate_errors(NewT, Tail));
    (
    % if the probed value is not equal to the predicted value
    \+probedValue(Output, Val),
    % both are input nodes
    input(A,_),
    input(B,_),
    writef("\n%t is malfunctioning and will be added to the bad list\n", [H]),
    % we certainly know now the model is not functioning and is set above
    % we do a check forward to see if more elements are not functioning(Bad)
    findall(X, component(X, _, Output, _, _), L),
    findall(X, component(X, _, _, Output, _), L2),
    append(L, L2, L3),
    write('\nChecking for other dependencies...'),
    check_forward_list(L3, Good2, Bad),
    flatten(Bad, Bad2),
    flatten(Good2, Good3),
    predecessors_list(Good3, Good4),
    flatten(Good4, Good5),
    append([H], Bad2, AllBad),
    predecessors_list(AllBad, BadPred),
    flatten(BadPred, BadPred2),
    exclude_second(BadPred, Good5, Good), 
    % the Good ones are elimenated from the T set
    write('\nThe following components are now in the good set:\n'),
    printlist(Good),
    write('\nThe following components are now in the definite bad set:\n'),
    printlist(AllBad),
    remove_good(Good, T,  T2),
    retractall(test(_,_)),
    elimenate_errors(T2, Tail));
    (
    % if the probed value is not equal to the predicted value
    \+probedValue(Output, Val),
    % we do a check forward to see if more elements are not functioning(Bad)
    findall(X, component(X, _, Output, _, _), L),
    findall(X, component(X, _, _, Output, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good2, Bad),
    flatten(Bad, Bad2),
    flatten(Good2, Good3),
    write('\nChecking for other dependencies...'),
    predecessors_list(Good3, Good4),
    flatten(Good4, Good5),
    AllBad = Bad2,
    predecessors_list(AllBad, BadPred),
    predecessors_list([H], Un),
    flatten(Un, Unknown),
    flatten(BadPred, BadPred2),
    exclude_second(Unknown, Good5, Good6),
    exclude_second(BadPred, Good6, Good), 
    write('\nThe following components are now in the good set:\n'),
    printlist(Good),
    write('\nThe following components are now in the definite bad set:\n'),
    printlist(AllBad),
    % the Good ones are elimenated from the T set
    remove_good(Good,T,  T2),
    retractall(test(_,_)),
    elimenate_errors(T2, Tail))).

% exclude elements of the first list that are in the second list
exclude_second(_, [], []):-!.

exclude_second(Bad, [H|Good], [H|StillGood]):-
    \+member(H, Bad),!,
    exclude_second(Bad, Good, StillGood).
           
exclude_second(Bad, [_|Good], StillGood):-
    exclude_second(Bad, Good, StillGood),!.
           
% the forward check to see
% if more good and bad components can be deducted
check_forward([],[],[]).

% basecase: H is here a component that
% leads to an output element
% if it is calculated well it gets added to Good
check_forward(H, H, []):-
    component(H, Type,X,Y,Out),
    output(Out, R),
    %all different possibilities... Yes, it is ugly
    once(((test(X, XVal),
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
    predictedValue(Y, YVal)))),
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
    output(Out, _),!.

% not an output component
% but output is probed and
% is not the same as tested
check_forward(H, [], [H]):-
    component(H, Type,X,Y,Out),
    probedValue(Out, R),
    (probedValue(X, XVal),
    probedValue(Y, YVal)),
    calculate(XVal, YVal, Type, Some),
    Some =\= R.

check_forward(H, [H|Good], Bad):-
    component(H, Type,X,Y,Out),
    probedValue(Out, R),
    once(((test(X, XVal),
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
    predictedValue(Y, YVal)))),
    calculate(XVal, YVal, Type, R),
    findall(Q, component(Q, _, Out, _, _), L),
    findall(Q, component(Q, _, _, Out, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good, Bad).


% not an output component
% and not able to say if good or bad
% (just working through it)
check_forward(H, Good, Bad):-
    component(H, Type,X,Y,Out),
    once(((test(X, XVal),
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
    predictedValue(Y, YVal)))),!,
    calculate(XVal, YVal, Type, Val),
    assert(test(Out, Val)),
    findall(Q, component(Q, _, Out, _, _), L),
    findall(Q, component(Q, _, _, Out, _), L2),
    append(L, L2, L3),
    check_forward_list(L3, Good, Bad).


check_forward_list([], [], []):-!.

check_forward_list([H|T], [NewGood|Good], [NewBad|Bad]):-
    check_forward(H,  NewGood, NewBad),!,
    check_forward_list(T, Good, Bad).

% check backwards doest the same thing as check forward,
% but works to the input arguments, instead of the output
% arguments


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
    (((component(P, _, _, _, X),
    input(Y, _)));
    (component(P, _, _, _, Y),
    input(X, _))),
    predecessors(P, Q).
    
predecessors(H, L):-
    component(H,_, X, Y, _),
    component(P, _, _, _, X),
    component(Q, _, _, _, Y),!,
    append([P], [Q], Pred),
    predecessors_list(Pred, L)
    .
    
predecessors_list([],[]):-!.
predecessors_list([H|T], [L3|L]):-
    predecessors(H, L2),!,
    append(L2, [H], L3),
    predecessors_list(T, L).

% remove all goods from bad set

remove_good(_, [], []):-!.

remove_good(_, [], []).
remove_good(Goods, [H|Bads], NewBad):-
    member(H, Goods),!,
    remove_good(Goods, Bads, NewBad).
remove_good(Goods, [H|Bads], [H|NewBad]):-
    \+member(H, Goods),!,
    remove_good(Goods, Bads, NewBad).

