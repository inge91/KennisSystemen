:-dynamic na/2.

% a voor b en b voor c dan weet je niet of a voor b is of b voor c is 

% punt predicate parses incoming sentence
% and derives other facts
punt:-
    write('Geef me input in lijst vorm'),
    read(Time_constraint),
    parse(Time_constraint).

% parse the input to make it assertable
parse([A, voor, B]):-
    assert(na(B,A)),
    write('asserted na('),
    write(B),
    write(','),
    write(A),
    writeln(')'),
    na_check.

parse([A, na, B]):-
    assert(na(A,B)),
    write('asserted na('),
    write(A),
    write(','),
    write(B),
    writeln(')'),
    na_check.

% findall the arguments of na_rules()
na_check:-
    findall(X/Y, na(X,Y), List),
    check_all_na(List).


% for all argument pairs of na rules
% check if ther are implicit rules
check_all_na([]).

check_all_na([X/Y|List]):-
    na_check_one(X,Y),
    check_all_na(List).

% the implicit rule detector
na_check_one(A, B):-
    na(B, Z),!,
    assert(na(A,Z)),
    write('asserted na('),
    write(A),
    write(','),
    write(Z),
    writeln(')').

na_check_one(B, C):-
    na(Z, B),!,
    assert(na(Z,C)),
    write('asserted na('),
    write(Z),
    write(','),
    write(C),
    write(')').


    
