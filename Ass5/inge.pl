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
    backprop(Output, _).


% both basecases
backprop([], []):-
    write("everything seems to be fine").

backprop([], [H|T]):-
    write('The following components seem to be malfunctioning:'),
    print_elements([H|T]).

% print all elements
print_elements([]).

print_elements([H|T]):-
    write(H),
    print_elements(T).

% in case the predicted output is the same as the
% measured output, go forth with measured output
backprop([X/Y|T], ErrorSet):-
    predictedValue(X, Y),
    backprop(T, ErrorSet).

backprop([X/Y|T], [Errors|Error]):-
    % output X is not as predicted
    \+predictedValue(X, Y),
    predictedValue(X, Z),
    writef("Value of output % seems incorrect.\nEstimated output: % \nMeasured output: %\n Extracting conflict set...",
    X, Y, Z),
    % every function before X is put in the errorset
    % front is last function, end is first function
    create_errorset(X, ErrorSet),
    elimenate_errors(ErrorSet,Errors),
    backprop(T, Error).


% combine values with a slash 
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
    writef("Added % to errorlist", H).

% findall pevious values until we reach input
findall_values(H, [Prev|PreviousValues]):-
    component(Prev, _,_,_,H),
    writef("Added % to errorlist", H),
    findall(X, component(_,_,X,_,H), List),
    findall(X, component(_,_,_,X,H), List2),
    append(List, List2, List3),
    %get_components(List3, Pre2),
    %append(Pre, Pre2, Prev),
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
% NEEEEEEDS ENTROPHY

elimenate_errors([], []).

% if predicted value has same value
% as probe then component that
% calculated it is not at fault.
elimenate_errors([H|T], Tail):-
    component(_,_,_,_,Output),
    predictedValue(Output, Val),
    writef("What do you measure at point %", Output),
    read(NewVal),
    assert(probedValue(Output, NewVal),
    probedValue(Output, Val),
    printf("retracted % from list, because element works fine", Output),
    % remove everything before 
    % this component as well?
    elimenate_errors(T, Tail).
    
% if predicted value has not 
% same value as probe the component that
% calulcated it is at fault
elimenate_errors([H|T] , [H|Tail]):-
    component(_,_,_,_, Output),
    predictedValue(Output, Val),
    writef("What do you measure at point %", Output),
    read(NewVal),
    assert(probedValue(Output, NewVal),
    \+probedValue(Ouput, Val),
    elimenate_errors(T, Tail).


swap([], []).
swap([H|T], X):-
    push_top([H],X, Y),
    swap(T, Y).

push_top(X, T,  [X|T]).


push_bot(H, [], H).

push_bot(H, [L|Y], [L|P]):-
    push_bot(H, Y, P).




