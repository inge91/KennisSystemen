:-dynamic na/2.
:-dynamic possible_gelijktijdig/2.
:-dynamic possible_na/2.

% a voor b en b voor c dan weet je niet of a voor b is of b voor c is 

% punt predicate parses incoming sentence
% and derives other facts
punt:-
    write('Geef me input in lijst vorm'),
    read(Time_constraint),
    parse(Time_constraint).

% parse the input to make it assertable
parse([A, voor, B]):-
    check(B, A, na),
    assert(na(B,A)),
    write('asserted na('),
    write(B),
    write(','),
    write(A),
    writeln(')'),
    impliciet_check(na).

parse([A, na, B]):-
    %check(A, B, na),
    assert(na(A,B)),
    write('asserted na('),
    write(A),
    write(','),
    write(B),
    writeln(')'),
    impliciet_check(na).

parse([A, gelijktijdig, B]):-
    %check(A, B, gelijktijdig),
    assert(gelijktijdig(A, B)),
    assert(gelijktijdig(B, A)),
    write('asserted gelijktijdig('),
    write(A),
    write(','),
    write(B),
    writeln(')'),
    impliciet_check(gelijktijdig).
    

% findall the arguments of na_rules()
impliciet_check(na):-
    findall(X/Y, na(X,Y), List),
    check_all_na(List).

impliciet_check(gelijktijdig):-
    findall(X/Y, gelijktijdig(X,Y), List),
    check_all_gelijktijdig(List).

% for all argument pairs of na rules
% check if ther are implicit rules
check_all_na([]).

check_all_na([X/Y|List]):-
    na_check_one(X,Y),
    na_check_one_timeline(X, Y),
    check_all_na(List).

% the implicit rule detector
na_check_one(A, B):-
    na(B, Z),!,
    \+na(A,Z),
    A\=Z,
    assert(na(A,Z)),
    write('asserted na('),
    write(A),
    write(','),
    write(Z),
    writeln(')').

na_check_one(B, C):-
    na(Z, B),!,
    \+na(Z,C),
    Z\=C,
    assert(na(Z,C)),
    write('asserted na('),
    write(Z),
    write(','),
    write(C),
    write(')').

na_check_one(_,_).

% Checks if there are more possible timelines
% for instance if we have na(b,a) and na(b, c)
% then derivee (b,a
na_check_one_timeline(B, C):-
    na(B, A),
    C \= A,
    assert(possible_na(A, C)),
    assert(possible_na(C, A)),
    assert(possible_gelijktijdig(C, A)),
    write(A),
    write(','),
    write(C),
    write(')'),
    write('asserted na('),
    write(C),
    write(','),
    write(A),
    write(')').

na_check_one_timeline(_,_).

%check for inconsistencies
check(X,Y, na):-
    \+na(Y,X).

check(X, Y, gelijktijdig):-
    \+na(X,Y),
    \+na(Y,X).

check_all_gelijktijdig([X/Y|T]):-
    gelijktijdig_check_one(X, Y),
    check_all_gelijktijdig(T).

gelijktijdig_check_one(A,B):-
    gelijktijdig(B, Z),
    \+gelijktijdig(A,Z),
    A \= Z,
    assert(gelijktijdig(A,Z)),
    write('asserted gelijktijdig('),
    write(A),
    write(','),
    write(Z),
    writeln(')').

gelijktijdig_check_one(B, C):-
    gelijktijdig(Z, B),
    \+gelijktijdig(Z,C),
    C \= Z,
    assert(gelijktijdig(Z,C)),
    write('asserted gelijktijdig('),
    write(Z),
    write(','),
    write(C),
    write(')').


% makes the timeline
print_timelines:-
    %find all elements that are first
    findall(X/Y, na(X,Y), List),
    member(P/Q, List),
    % OneTimeLine is a list of visited
    % points
    % Look for
    solve_this([Q], Timeline),
    print_timeline(Timeline).


solve_this(X, X):-
    the_goal(X).

solve_this(X, Z):-
    lastelement(X, Last),
    member(A, Last),
    move(A, Y),
    append(X, [Y], NewX),
    solve_this(NewX, Z).


solve_this(X, Z):-
    lastelement(X, Last),
    move(Last, Y),
    append(X, [Y], NewX),
    solve_this(NewX, Z).

%if there is no possible element after last element
the_goal(X):-
    %pick the last element
    lastelement(X, Last),
    member(Element, Last),
    \+na(_, Element),
    \+possible_na(_, Element).


the_goal(X):-
    %pick the last element
    lastelement(X, Last),
    \+na(_, Last),
    \+possible_na(_, Last).

% we only know possible moves so we only use those
move(X, Y):-
    findall(Z, na(Z, X), [] ),
    findall(Z, possible_na(Z, X), PossibleList),
    member(NewNa, PossibleList),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move(X, P):-
    findall(Z, na(Z,X), L),
    member(P, L).


all_sametime(Element, SameTime):-
    findall(X, gelijktijdig(X, Element), L1),
    findall(P, gelijktijdig(Element, P), L2),
    conc(L1, L2, SameTime).
        

























% print timelines
print_timeline([]).

print_timeline([H]):-
    write(H).
    print_timeline([]).

print_timeline([H|TimeLine]):-
    write(H),
    write(------),
    print_timeline(TimeLine).



% returns all elements from the first list
% that are not in the second list
exclude([], _, []).

exclude([H|L], L2, [H|T]):-
    \+member(H, L2),!,
    exclude(L, L2, T).

exclude([H|L], L2, T):-
    member(H, L2),!,
    exclude(L, L2, T).

% filters all doubles out of the predicate
unique_points(X, Z):-
    unique_points(X, [], Z).

unique_points([], Y, Y).

unique_points([H|T], X, Z):-
   \+ member(H, X),
   unique_points(T, [H|X], Z).

unique_points([H|T], X, Z):-
   member(H, X),
   unique_points(T, X, Z).


% print lists
print_list([]).

print_list([H|T]):-
    write(H),
    print_list(T).

% Standard Breadthfirst algorithm , by Bratko
solve(Start, Solution):-
    breadthfirst([[Start]], Solution).

breadthfirst([[Node|Path]|_], [Node|Path]):-
    goal(Node).

breadthfirst([Path|Paths], Solution):-
    extend(Path, NewPaths),
    conc(Paths, NewPaths, Paths1),
    breadthfirst(Paths1, Solution).

extend([Node|Path], NewPaths):-
    bagof([NewNode, Node|Path], 
        (s(Node, NewNode), \+ member(NewNode, [Node|Path])), 
            NewPaths),!.

extend(Path,[]).


goal(H):-
    lastelement(H, Last),
    findall(X, na(X,Last), []).

s(Node, NewNode):-
    lastelement(Node, Last),
    findall(X, na(X, Last), L),
    member(P, L),
    push_bot(P, Node, NewNode).



%get last element

lastelement([X], X).
lastelement([H|T], U):-
    lastelement(T, U),!.

push_bot(X, [], [X]).				%als Y leeg is dan is L [X]

push_bot(X, [H|T], L):-
	push_bot(X, T, L2), 			%herhaal tot de tail leeg i
	push_top(H, L2, L),!.			%voeg H (head) bij L2 en dat geeft antwoord 


push_top(X, [], [X]).				%als de Y leeg is dan is L gelijk aan [X]		

push_top(X, Y, [X|Y]).



conc([],Result,Result).
conc([Head|Tail_First],List,[Head|Tail_Result]) :-
              conc(Tail_First,List,Tail_Result).
