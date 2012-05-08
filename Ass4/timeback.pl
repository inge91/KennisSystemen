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
    assert(na(K, C)).

gelijktijd_check_one_timeline(B, C):-
    na(B, K),
    K \= C,
    assert(na(C, K)).

gelijktijd_check_one_timeline(B, C):-
    na(K, C),
    K \= B,
    assert(na(K, B)).

gelijktijd_check_one_timeline(B, C):-
    na(C, K),
    K \= B,
    assert(na(B, K)).

gelijktijdig_check_one_timelin(_,_).

% makes the timeline
print_timelines:-
    %find all elements that are first
    findall(X/Y, na(X,Y), List),
    member(P/L, List),
    all_sametime(L,Q),
    % OneTimeLine is a list of visited
    % points
    % Look for
    solve_this([Q], [H|Timeline]),
    solve_this2([H], Timeline2),
    append(Timeline2, Timeline, TimeLinee),
    print_timeline(TimeLinee).

solve_this2(X,X):-
    the_goal2(X).
    
solve_this2([H|X], Z):-
    member(Element, H),
    move2([Element|X], Y),
    solve_this2([Y|[H|X]], Z).

solve_this2([H|X], Z):-
    move2([H|X], Y),
    solve_this2([Y|[H|X]], Z).

move2([A|T], Y):-
    member(X, A),
    findall(Z, na(X, Z), []),
    findall(Z, possible_na(X, Z), PossibleList),
    member(NewNa, PossibleList),
    not_members([NewNa], [X|T]),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move2([A|T], P):-
    member(X, A),
    findall(Z, na(X,Z), L),
    member(P, L),
    not_members(P, [X|T]).

move2([X|T], Y):-
    findall(Z, na(X, Z), []),
    findall(Z, possible_na(X, Z), PossibleList),
    member(NewNa, PossibleList),
    not_members([NewNa], [X|T]),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move2([X|T], P):-
    findall(Z, na(X,Z), L),
    member(P, L),
    not_members(P, [X|T]).


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
    %pick the last element
    findall(X , na(H,X), L1),
    findall(X, possible_na(H, X), L2),
    append(L1, L2, List),!,
    \+not_members(List, [H|P] ).

not_members([], _).

not_members([H|List], History):-
    \+member(H, History),
    not_members(List, History).

solve_this(X, X):-
    the_goal(X).

solve_this(X, Z):-
    move(X, Y),
    append(X, [Y], NewX),
    solve_this(NewX, Z).


solve_this(X, Z):-
    move(X, Y),
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
    atom(Last),
    \+na(_, Last),
    \+possible_na(_, Last).

% we only know possible moves so we only use those
move(X, Y):-
    lastelement(X, Last),
    member(A, Last),
    findall(Z, na(Z, A), [] ),
    findall(Z, possible_na(Z, A), PossibleList),
    member(NewNa, PossibleList),
    not_members([NewNa], X),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move(X, P):-
    lastelement(X, Last),
    member(A, Last),
    findall(Z, na(Z,A), L),
    member(NewNa, L),
    not_members([NewNa], X),
    all_sametime(NewNa, P).

move(X, Y):-
    lastelement(X, Last),
    findall(Z, na(Z, Last), [] ),
    findall(Z, possible_na(Z, Last), PossibleList),
    member(NewNa, PossibleList),
    not_members([NewNa], X),
    all_sametime(NewNa, Y).

% we have a certain na so we use that
move(X, P):-
    lastelement(X, Last),
    findall(Z, na(Z,Last), L),
    member(NewNa, L),
    not_members([NewNa], X),
    all_sametime(NewNa, P).

all_sametime(Element, SameTimes):-
    findall(X, gelijktijdig(X, Element), L1),
    findall(P, gelijktijdig(Element, P), L2),
    conc(L1, L2, Same),
    conc(Same, [Element], SameTime),
    unique_points(SameTime, SameTimes)
    .
        




















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
	push_bot(X, T, L2), 			%herhaal tot de tail leeg i
	push_top(H, L2, L),!.			%voeg H (head) bij L2 en dat geeft antwoord 


push_top(X, [], [X]).				%als de Y leeg is dan is L gelijk aan [X]		

push_top(X, Y, [X|Y]).



conc([],Result,Result).
conc([Head|Tail_First],List,[Head|Tail_Result]) :-
              conc(Tail_First,List,Tail_Result).



na(c, a).

%Je start dus bij het 'start symbool'
% dan zoek je alle elementen die na het start symbool komen en nooit het eerste argument zijn
% in een na() feit, alleen het tweede argument. 
% Je haalt er hier een van uit en gebruikt deze om alle possible_na's toe te
% passen. Voeg deze alleen toe als symbolen hiervan nieuw zijn en niet al
% voorkomen in je huidige code.
% als er geen possible na's zijn dan probeer je hetzelfde met de na's
% als allebei een lege lijst oplevert dan ben je klaar!
% denk erom member te gebruiken


%start -> a
%
%    findall(X, possible_na(X, a), [])
%    findall(X, na(X, a), [] ).
%    not_members(b, [a])
%
%    possible_na(b, a)

%[a, [b,c]]  


