%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Ass 4                   %%
%%                          %%
%%  Richard Pronk           %%
%%                          %%
%%  Inge Becht              %%
%%                          %%
%%                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic na/2.
:-dynamic gelijktijdig/2.
:-dynamic possible_gelijktijdig/2.

% Our Program works as follows:
%
% The user starts the punt/0 predicate which starts the promptloop
% The user can enter the following things:
% [a, voor, b].
% [b, na, a].
% [b, gelijktijdig, c].
% [b, gelijktijdig, of, na, c].
% [b, na, of, gelijktijdig, c].
% [b, gelijktijdig, of, voor, c].
% [b, voor, of, gelijktijdig, c].
%
% when using stop. the input loop will stop. Use solve/0 to get the timelines.
%
%INPUT ELEMENTS SHOULD ONLY CONSISTS OF ATOMS!!!!
%
% Everytime the user inputs a relation between two elements we assert each
% element to a starting point start(for retrieving the timeline) and we assert
% the direct relation to the fact base(if it has an 'or' we assert both).
% If we have a voor() case we write it like a na/2 fact. (na(a, b) means that b
% comes after na)
%
% Eacht time we assert a rule we make explicit some of the underlying information
% that carries with it. We do this everytime so we don't miss any possible underlying
% rules, and it seems to work.
% Some of these possible underlying rules are:
% - if b after a and c after b then c after a
% - if a sametime c and d after c then d after a
% etc.
%
% basically everything we do, implicit or explicit, gets asserted to a na() or
% gelijktijdig() rule. If a new rule is inconsistent with the data we give our
% user a warning and no data is asserted.
% One of these inconsistend rules is  that if b after a then b CANNOT be before
% a.
%
% We wanted to get everything so explicity so we could make a timeline searcher
% that could find all elements just by adding the next na-rule and use the
% history to keep track of which elements we already added.
%
% punt/0 input loop that asks the user for all time events he wants to assert.
% we coulnt try everything and some things may not work. What does work is the following input:
%
% the simple case
% [a, voor, b].
% [c, voor, b].
%
% a case like
% [a, voor, b].
% [c, na, b].
% [d, gelijktijdig,c].
% [g, na, b].
% [i, voor, b].
% [l, voor, b].
%
% or :
% [c , gelijktijdig, or na, b].
% [d, na, b].
% [a, voor, b].
punt:-
    write(' Give a possible time relation in list form (example [a, voor, b].\n,
        type stop to calculate timelines.\n'),
    read(Input),
    A = Input,
   ((Input == stop,!,
    write('stopped'));
    parse(A),
    punt).

% parsing of all the different possible
% kind of inputs

parse([A, voor, B]):-
    \+not_legal(A, na, B),
    add_start(A),
    add_start(B),
    add_pair(A, na, B),
    make_explicit4(A, na, B).

% if the input is illegal,
% do nothing
parse([A, voor, B]):-
    not_legal(A, na, B),
    write('The input is illegal because of the occurence of na('),
    write(B),
    write(','),
    write(A),
    writeln(')'),
    writeln('Nothing is asserted').

parse([A, na, B]):-
    \+not_legal(B, na, A),
    add_start(A),
    add_start(B),
    add_pair(B, na, A),
    make_explicit4(B,na,A).

% illegal input; do nothing
parse([A, na, B]):-
    not_legal(B, na, A),
    write('The input is illegal because of the occurence of na('),
    write(B),
    write(','),
    write(A),
    writeln(')'),
    writeln('Nothing is asserted').

parse([A, gelijktijdig, B]):-
    add_start(A),
    add_start(B),
    add_pair(B, gelijktijdig, A),
    add_pair(A, gelijktijdig, B),
    make_explicit4(A,gelijktijdig, B).


% cases voor, gelijktijdig and na, gelijktijdig
parse([A, V, of,G, B]):-
    ((V == voor,
    G == gelijktijdig);
    (G == voor,
     V == gelijktijdig)),
     parse([A, voor, B]),
     assert(possible_gelijktijdig(A,B)),
     assert(possible_gelijktijdig(A,B)).

parse([A, N, of,G, B]):-
    ((N == na,
    G == gelijktijdig);
    (G == na,
     N == gelijktijdig)),
     parse([A, na, B]),
     assert(possible_gelijktijdig(A,B)),
	 assert(possible_gelijktijdig(B, A)).


% TODO
% parse([A, voor, of, gelijktijdig, B]):-
% parse([A, na, of, gelijktijdig, C]).

% here we state what is illegal input
% A na B is illegal if there
% is an instance of B na A in the code
not_legal(A, na, B):-
    na(B, A).


% add_start ensures that
% all element are after start
add_start(Element):-
    \+na(start, Element),!,
    assert(na(start, Element)),
    print_assert(start, na, Element).

% if already exists, do nothing
add_start(Element):-
    na(start, Element),!.

% adds a timepair, nothing
% special
add_pair(A, na, B):-
    \+na(A, B),
    \+not_legal(A, na, B),
    A \= B,
    assert(na(A, B)),
    print_assert(A, na, B).

add_pair(A, na, B):-
    na(A, B);
    A == B;
    not_legal(A, na, B).

add_pair(A, gelijktijdig, B):-
    \+gelijktijdig(A, B),
    A \= B,
    assert(gelijktijdig(A, B)),
    print_assert(A, gelijktijdig, B).

add_pair(A, gelijktijdig, B):-
    gelijktijdig(A, B);
    A == B.


% prints what is asserted
print_assert(A, na, C):-
    write('asserted: na('),
    write(A),
    write(','),
    write(C),
    writeln(')').


print_assert(A, gelijktijdig, C):-
    write('asserted: gelijktijdig('),
    write(A),
    write(','),
    write(C),
    writeln(')').


make_explicit(na):-
    findall(X/Y, (na(X, Y), X\= start), List),
    remove_start(List, NewList),
    make_explicit2(NewList, na).

make_explicit(gelijktijdig):-
    findall(X/Y, gelijktijdig(X, Y), List),
    make_explicit2(List, gelijktijdig).

make_explicit2([], na).
make_explicit2([], gelijktijdig).

make_explicit2([X/Y|List], na):-
    make_explicit3(X, na, Y),
    make_explicit2(List, na).

make_explicit2([X/Y|List], gelijktijdig):-
    make_explicit3(X, gelijktijdig, Y),
    make_explicit2(List, gelijktijdig).

% Makes all rules explicit needed for a na relation
make_explicit3(A, na, B):-
    % all elements after B ar alements after A
   ((findall(X, na(B, X), NA_elements),
        findall(X, gelijktijdig(B, X), Gelijk),
        append(NA_elements, Gelijk, All),
    add_pairs(A, na, All));
    ( findall(X, na(B, X), []))),

    % all elements before A are alements before B
    ((findall(X, na(X, A), NA_elements3),
        findall(X, gelijktijdig(X, A), Gelijk2),
        append(NA_elements3, Gelijk2, All2),
    add_pairs2(All2, na, B));
    findall(X, na(X, A), [])),

    % all elements before B and not yet in
    % a relation with A are getting a symmetric
    % relation with A
    ((findall(X, na(X, B), NA_elements2),
        remove_start(NA_elements2, NA),
    no_relations(A, NA, NewNA),
    add_pairs3(A, na, NewNA),
    add_pairs4(NewNA, na, A));
    (findall(X, na(X, B), []))),

    % all elements after A not yet in a relationship
    % with B are getting a symmetric relation with a
    ((findall(X, na(A, X), NA_elements4),
        remove_start(NA_elements4, NA2),
    no_relations(B, NA2, NewNA2),
    add_pairs3(B, na, NewNA2),
    add_pairs4(NewNA2, na,B));
    (findall(X, na(A, X),[]))).

        % ((findall(X, na(X, B), NA_elements2),
        % add_pairs(A, na, NA_elements2),
        % add_pairs2(NA_elements2, na, A));
        % (findall(X, na(X, B), []))),

        % ((findall(X, na(A, X), NA_elements4),
        % add_pairs(B, na, NA_elements4),
        % add_pairs2(NA_elements4, na,B));
        % (findall(X, na(A, X),[]))).



% makes explicit all things that have to do with gelijktijdig relations
make_explicit3(B, gelijktijdig, A):-
    (findall(X, gelijktijdig(B, X), Gelijktijdige_elements),
        add_pairs5(A, gelijktijdig,Gelijktijdige_elements),
        add_pairs6(Gelijktijdige_elements, gelijktijdig, A)
    ),
    (findall(X, gelijktijdig(X, A), Gelijktijdige_elements2),
        add_pairs5(B, gelijktijdig, Gelijktijdige_elements2),
        add_pairs6(Gelijktijdige_elements2, gelijktijdig, B)),
    (findall(X, na(X, A), NA_elements),
		remove_start(NA_elements, NA_el),
        add_pairs6(NA_el, na, B )),
    (findall(X, na(A, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)),
    (findall(X, na(X, B), NA_elements3),
	remove_start(NA_elements3, NA_el3),
        add_pairs6(NA_el3, na, A )),
    (findall(X, na(B, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)).
make_explicit3(A, na, B):-
    % all elements after B ar alements after A
   ((findall(X, na(B, X), NA_elements),
        findall(X, gelijktijdig(B, X), Gelijk),
        append(NA_elements, Gelijk, All),
    add_pairs(A, na, All));
    ( findall(X, na(B, X), []))),

    % all elements before A are alements before B
    ((findall(X, na(X, A), NA_elements3),
        findall(X, gelijktijdig(X, A), Gelijk2),
        append(NA_elements3, Gelijk2, All2),
    add_pairs2(All2, na, B));
    findall(X, na(X, A), [])),

    % all elements before B and not yet in
    % a relation with A are getting a symmetric
    % relation with A
    ((findall(X, na(X, B), NA_elements2),
        remove_start(NA_elements2, NA),
    no_relations(A, NA, NewNA),
    add_pairs3(A, na, NewNA),
    add_pairs4(NewNA, na, A));
    (findall(X, na(X, B), []))),

    % all elements after A not yet in a relationship
    % with B are getting a symmetric relation with a
    ((findall(X, na(A, X), NA_elements4),
        remove_start(NA_elements4, NA2),
    no_relations(B, NA2, NewNA2),
    add_pairs3(B, na, NewNA2),
    add_pairs4(NewNA2, na,B));
    (findall(X, na(A, X),[]))).

        % ((findall(X, na(X, B), NA_elements2),
        % add_pairs(A, na, NA_elements2),
        % add_pairs2(NA_elements2, na, A));
        % (findall(X, na(X, B), []))),

        % ((findall(X, na(A, X), NA_elements4),
        % add_pairs(B, na, NA_elements4),
        % add_pairs2(NA_elements4, na,B));
        % (findall(X, na(A, X),[]))).


% After a lot of trial and error....
% make_explicit4 uses a na relation to
% define everything that comes before and
% after both elements (Except in cases of ambiguity
make_explicit4(A, na, B):-
	% all elements after B ar alements after A
	((findall(X, na(B, X), NA_elements),
     findall(X, gelijktijdig(B, X), Gelijk),
        append(NA_elements, Gelijk, All),
    add_pairs(A, na, All));
    ( findall(X, na(B, X), []))),

    % all elements before A are alements before B
    ((findall(X, na(X, A), NA_elements3),
        findall(X, gelijktijdig(X, A), Gelijk2),
        append(NA_elements3, Gelijk2, All2),
    add_pairs2(All2, na, B));
    findall(X, na(X, A), [])).


make_explicit4(B, gelijktijdig, A):-
    (findall(X, gelijktijdig(B, X), Gelijktijdige_elements),
        add_pairs5(A, gelijktijdig,Gelijktijdige_elements),
        add_pairs6(Gelijktijdige_elements, gelijktijdig, A)
    ),
    (findall(X, gelijktijdig(X, A), Gelijktijdige_elements2),
        add_pairs5(B, gelijktijdig, Gelijktijdige_elements2),
        add_pairs6(Gelijktijdige_elements2, gelijktijdig, B)),
    (findall(X, na(X, A), NA_elements),
		remove_start(NA_elements, NA_el),
        add_pairs6(NA_el, na, B )),
    (findall(X, na(A, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)),
    (findall(X, na(X, B), NA_elements3),
	remove_start(NA_elements3, NA_el3),
        add_pairs6(NA_el3, na, A )),
    (findall(X, na(B, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)).





% makes explicit all things that have to do with gelijktijdig relations
make_explicit(B, gelijktijdig, A):-
    (findall(X, gelijktijdig(B, X), Gelijktijdige_elements),
        add_pairs5(A, gelijktijdig,Gelijktijdige_elements),
        add_pairs6(Gelijktijdige_elements, gelijktijdig, A)
    ),
    (findall(X, gelijktijdig(X, A), Gelijktijdige_elements2),
        add_pairs5(B, gelijktijdig, Gelijktijdige_elements2),
        add_pairs6(Gelijktijdige_elements2, gelijktijdig, B)),
    (findall(X, na(X, A), NA_elements),
		remove_start(NA_elements, NA_el),
        add_pairs6(NA_el, na, B )),
    (findall(X, na(A, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)),
    (findall(X, na(X, B), NA_elements3),
	remove_start(NA_elements3, NA_el3),
        add_pairs6(NA_el3, na, A )),
    (findall(X, na(B, X), NA_elements2),
	remove_start(NA_elements2, NA_el2),
        add_pairs5(B, na, NA_el2)).

% we check if one element
% and a list have no prior
% relatiosn, if it has, the element is deleted out of the list
no_relations(_, [], []).

no_relations(X, [H|T], [H|R]):-
    \+na(X, H),
    \+na(H, X),
    \+gelijktijdig(X, H),
    X \= H,
    no_relations(X, T, R).

no_relations(X, [H|T], R):-
    (na(X,H);
    na(H, X);
    gelijktijdig(X, H);
    X == H),
    no_relations(X, T, R).



% adds all instances of the third
% argument in relation to the first argument
add_pairs(_, na, []).
add_pairs(_, gelijktijdig, []).

add_pairs(X , na, [H|T]):-
    add_pair(X, na , H),
    add_pairs(X, na, T).

add_pairs(X, gelijktijdig, [H|T]):-
    add_pair(X, gelijktijdig, H),
    add_pairs(X, gelijktijdig, T).

add_pairs2([], na, _).
add_pairs2([], gelijktijdig, _).

add_pairs2([H|T], na, X):-
    add_pair(H, na, X),
    add_pairs2(T, na, X).

add_pairs2([H|T], gelijktijdig, X):-
    add_pair(H, gelijktijdig, X),
    add_pairs2(T, gelijktijdig, X).


% add_pairs3 doesnt check in history when adding relations
add_pairs3(_, na, []).

add_pairs3(X, na, [H|T]):-
    add_pair2(X, na, H),
    add_pairs3(X, na, T).

add_pairs3(_, gelijktijdig, [] ).

add_pairs3(X, gelijktijdig, [H|T]):-
    add_pair2(X, gelijktijdig, H),
    add_pairs3(X, gelijktijdig, T).

% add_pairs4 doesnt check for history either but also uses a list in
% the first argument
add_pairs4([], na, _).
add_pairs4([], gelijktijdig, _).

add_pairs4([H|T], na, X):-
    add_pair2(H, na, X),
    add_pairs4(T, na, X).

add_pairs4([H|T], gelijktijdig, X):-
    add_pair2(H, gelijktijdig, X),
    add_pairs4(T, gelijktijdig, X).

add_pair2(H, na, X):-
    \+na(H,X),
    H \= X,
    assert(na(H, X)),
    print_assert(H, na, X).

add_pair2(H, na, X):-
    na(H, X);
    H == X.

add_pair2(H, gelijktijdig, X):-
    \+gelijktijdig(H,X),
    H \= X,
    assert(gelijktijdig(H, X)),
    print_assert(H, gelijktijdig, X).

add_pair2(H, gelijktijdig, X):-
    gelijktijdig(H, X);
    H == X.

% these are all help predicates and not very interesting to describe

add_pair3(H, na, X):-
    H \= X,
    assert(na(H, X)),
    print_assert(H, na, X).

add_pair3(H, na, X):-
    H == X.

add_pair3(H, gelijktijdig, X):-
    H \= X,
    assert(gelijktijdig(H, X)),
    print_assert(H, gelijktijdig, X).

add_pair3(H, gelijktijdig, X):-
    H == X.

% add_pairs3 doesnt check in history when adding relations
add_pairs5(_, na, []).

add_pairs5(X, na, [H|T]):-
    add_pair3(X, na, H),
    add_pairs5(X, na, T).

add_pairs5(_, gelijktijdig, [] ).

add_pairs5(X, gelijktijdig, [H|T]):-
    add_pair3(X, gelijktijdig, H),
    add_pairs5(X, gelijktijdig, T).

% add_pairs4 doesnt check for history either but also uses a list in
% the first argument
add_pairs6([], na, _).
add_pairs6([], gelijktijdig, _).

add_pairs6([H|T], na, X):-
    add_pair3(H, na, X),
    add_pairs6(T, na, X).

add_pairs6([H|T], gelijktijdig, X):-
    add_pair3(H, gelijktijdig, X),
    add_pairs6(T, gelijktijdig, X).


remove_start([], []):-!.

remove_start([start|T], P):-
    remove_start(T, P),!.

remove_start([X|T], [X|P]):-
    remove_start(T, P),!.


solve:-
    findall(X,na(start,X),List),
    unique_points(List,List2),
    timelines(List2,[]).

timelines([],[_|T]):-
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
    write('='),
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
