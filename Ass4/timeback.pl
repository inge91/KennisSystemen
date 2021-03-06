:-dynamic na/2.
:-dynamic gelijktijdig/2.
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
    \+possible_na(A,C),
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
    gelijktijd_check_one_timeline(X,Y),
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

gelijktijdig_check_one(_,_).

gelijktijd_check_one_timeline(B, C):-
    na(K, B),
    K \= C,
    \+na(K,C),
    assert(na(K, C)).

gelijktijd_check_one_timeline(B, C):-
    na(B, K),
    K \= C,
    \+na(C,K),
    assert(na(C, K)).

gelijktijd_check_one_timeline(B, C):-
    na(K, C),
    K \= B,
    \+na(K,B),
    assert(na(K, B)).

gelijktijd_check_one_timeline(B, C):-
    na(C, K),
    K \= B,
    \+na(B,K),
    assert(na(B, K)).


gelijktijd_check_one_timeline(B, C):-
    possible_na(K, B),
    K \= C,
    \+possible_na(K,C),
    assert(possbile_na(K, C)).

gelijktijd_check_one_timeline(B, C):-
    possible_na(B, K),
    K \= C,
    \+possible_na(C,K),
    assert(possible_na(C, K)).

gelijktijd_check_one_timeline(B, C):-
    possible_na(K, C),
    K \= B,
    \+possible_na(K,B),
    assert(possible_na(K, B)).

gelijktijd_check_one_timeline(B, C):-
    possible_na(C, K),
    K \= B,
    \+possible_na(B,K),
    assert(possible_na(B, K)).

gelijktijd_check_one_timeline(_,_).

% makes the timeline
print_timelines:-
    %find all elements that are first
    findall(X, na(_,X), List),
    unique_points(List, List2),
    member(L, List2),
    all_sametime(L,Q),
    % OneTimeLine is a list of visited
    % points
    % Look for
    solve_this([Q], [H|Timeline]),
    solve_this2([H|Timeline], Timeline2),            %solve_this2([H], Timeline2),
    print_timeline(Timeline2).

solve_this2(X,X):-
    the_goal2(X).

solve_this2([H|X], Z):-
    member(Element, H),!,
    move2([Element|X], Y),
    solve_this2([Y|[H|X]], Z).

solve_this2([H|X], Z):-
    move2([H|X], Y),
    solve_this2([Y|[H|X]], Z).

move2([A|T], Y):-
    member(X, A),
    findall(Z, possible_na(X, Z), []),
    findall(Z, na(X, Z), PossibleList),
    member(NewNa, PossibleList),
    flatten([A|T], History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move2([A|T], P):-
    member(X, A),
    findall(Z, possible_na(X,Z), L),
    member(P, L),
    flatten([A|T], History),
    not_members([P], History).

move2([X|T], Y):-
    findall(Z, possible_na(X, Z), []),
    findall(Z, na(X, Z), PossibleList),
    member(NewNa, PossibleList),
    flatten([X|T], History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move2([X|T], P):-
    findall(Z, possible_na(X,Z), L),
    member(P, L),
    flatten([X|T], History),
    not_members([P], History).


the_goal2([H|_]):-
    %pick the last element
    member(Element, H),
    \+na(Element,_ ),
    \+possible_na(Element,_),!.

the_goal2([H|_]):-
    %pick the last element
    atom(H),
    \+na(H,_),
    \+possible_na(H, _),!.

the_goal2([H|P]):-
    member(Element, H),
    %pick the last element
    findall(X , na(Element,X), L1),
    findall(X, possible_na(Element, X), L2),
    append(L1, L2, List),!,
    flatten([H|P], History),
    \+not_members(List, History ).

the_goal2([H|P]):-
    atom(H),
    %pick the last element
    findall(X , na(H,X), L1),
    findall(X, possible_na(H, X), L2),
    append(L1, L2, List),!,
    flatten([H|P], History),
    \+not_members(List, History).

not_members([], _).

not_members([H|List], History):-
    atom(H),
    \+member(H, History),!,
    not_members(List, History).

not_members([H|List], History):-
    not_members(H, History),!,
    not_members(List, History).

solve_this(X, X):-
    the_goal(X).

solve_this(X, Z):-
    move(X, Y),!,
    append(X, [Y], NewX),
    solve_this(NewX, Z).


%if there is no possible element after last element
the_goal(X):-
    %pick the last element
    lastelement(X, Last),
    member(Element, Last),!,
    \+na(_, Element),
    \+possible_na(_, Element).


the_goal(X):-
    %pick the last element
    lastelement(X, Last),
    atom(Last),
    \+na(_, Last),
    \+possible_na(_, Last).

% we only know possible moves so we only use those
move(X, Y):-
    lastelement(X, Last),
    member(A, Last),
    findall(Z, possible_na(Z, A), [] ),
    findall(Z, na(Z, A), PossibleList),
    member(NewNa, PossibleList),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move(X, P):-
    lastelement(X, Last),
    member(A, Last),
    findall(Z, possible_na(Z,A), L),
    member(NewNa, L),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, P).

move(X, Y):-
    lastelement(X, Last),
    member(A, Last),
    findall(Z, na(Z, A), PossibleList),
    member(NewNa, PossibleList),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).

move(X, Y):-
    lastelement(X, Last),
    findall(Z, possible_na(Z, Last), []),
    findall(Z, na(Z, Last), PossibleList ),
    member(NewNa, PossibleList),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move(X, P):-
    lastelement(X, Last),
    findall(Z, possible_na(Z,Last), L),
    member(NewNa, L),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, P).

move(X, Y):-
    lastelement(X, Last),
    findall(Z, na(Z, Last), PossibleList ),
    member(NewNa, PossibleList),
    flatten(X, History),
    not_members([NewNa], History),
    all_sametime(NewNa, Y).


all_sametime(Element, SameTimes):-
    findall(X, gelijktijdig(X, Element), L1),
    findall(P, gelijktijdig(Element, P), L2),
    conc(L1, L2, Same),
    conc(Same, [Element], SameTime),
    unique_points(SameTime, SameTimes).


%
%flatten(List, Flattened):-
%  flatten(List, [], Flattened).

%flatten([], Flattened, Flattened).
%flatten([Item|Tail], L, Flattened):-
%  flatten(Item, L1, Flattened),!,
%  flatten(Tail, L, L1),!.
%flatten(Item, Flattened, [Item|Flattened]):-
%  \+ is_list(Item),!.






% print timelines
print_timeline([]).

print_timeline([H]):-
    atom(H),
    write(H),!,
    print_timeline([]).

print_timeline([H|TimeLine]):-
    atom(H),
    write(H),
    write(------),!,
    print_timeline(TimeLine).

print_timeline([H]):-
    print_all_elements(H),!,
    print_timeline([]).

print_timeline([H|TimeLine]):-
    print_all_elements(H),
    write(------),!,
    print_timeline(TimeLine).

print_all_elements([]).

print_all_elements([H]):-
    write(H),
    print_all_elements([]).

print_all_elements([H|T]):-
    write(H),
    write('/'),
    print_all_elements(T).



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
	push_bot(X, T, L2),			%herhaal tot de tail leeg i
	push_top(H, L2, L),!.			%voeg H (head) bij L2 en dat geeft antwoord


push_top(X, [], [X]).				%als de Y leeg is dan is L gelijk aan [X]

push_top(X, Y, [X|Y]).



conc([],Result,Result).
conc([Head|Tail_First],List,[Head|Tail_Result]) :-
              conc(Tail_First,List,Tail_Result).


/*
possible_gelijktijdig(b,c).
possible_gelijktijdig(c,b).
%possible_na(b,c).

%gelijktijdig(b,c).
%gelijktijdig(c,b).
na(start,a).
na(start,b).
na(start,c).
na(start,g).
na(a,b).
na(a,c).
na(a,g).
na(b,g).
na(c,g).
%b na of gelijk c
*/
/*  test 1  SUCCES!
gelijktijdig(c,d).
gelijktijdig(d,c).
na(start,a).
na(start,b).
na(a,b).
na(start,c).
na(b,c).
na(a,c).
na(start,d).
na(b,d).
na(a,d).
na(b,c).
na(a,c).
na(start,g).
na(b,g).
na(a,g).
*/
/*  test2 SUCCES!
gelijktijdig(c,d).
gelijktijdig(d,c).
na(start,a).
na(start,b).
na(a,b).
na(start,c).
na(b,c).
na(a,c).
na(start,g).
na(b,g).
na(a,g).
na(start,i).
na(i,b).
na(i,c).
na(i,g).
na(start,l).
na(b,l).
na(a,l).
na(i,l).
na(start,d).
na(b,d).
na(a,d).
na(b,c).
na(a,c).
na(l,d).
na(i,d).
*/
/* fail
na(start,a).
na(start,b).
na(a,b).
na(start,c).
na(c,b).
na(start,d).
na(b,d).
na(a,d).
na(c,d).
possible_gelijktijdig(b,c).
possible_gelijktijdig(c,b).
*/
na(start,a).
na(start,b).
na(a,b).
na(start,c).
na(b,c).
na(a,c).
na(start,c).
na(b,c).
na(a,c).
na(start,d).
na(b,d).
na(a,d).
possible_gelijktijdig(b,c).
possible_gelijktijdig(c,b).



print_timelines2:-
    findall(X,na(start,X),List),
    unique_points(List,List2),
    timelines(List2,[]).

timelines([],[H|T]):-
     %tel elemeten
     findall(X,na(start,X),List),
     unique_points(List,List2),
     flatten(T,T_flatten),
     length(List2,L),
     length(T_flatten,L),
     print_timeline(T),nl.

timelines(X,[]):-
    timelines(X,[start]).

timelines(PossiblePoints, Solution):-
    findall(A, (member(A,PossiblePoints), findall(Test,((na(Test,A)), flatten(Solution,Test2), \+member(Test,Test2)), [])),Uitkomst),
    member(A, Uitkomst),
    findall(X, gelijktijdig(X,A), GelijktijdigLijst),
    findall(X, possible_gelijktijdig(X,A), GelijktijdigLijst2),
    append([A],GelijktijdigLijst,A2),
    (append(A2,GelijktijdigLijst2,A3);append(A2,[],A3)),
    append(Solution,[A3],Solution2),
    exclude(PossiblePoints, A3, PossiblePoints2),
    timelines(PossiblePoints2,Solution2).





