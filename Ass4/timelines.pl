:-dynamic voor/2.

voor(A,C):-
    voor(A,B),
    voor(B,C).

na(C, A):-
    na(C, B),
    na(B, C).


% punt predicate parses incoming sentence
% and derives other facts
punt:-
    write('Geef me input in lijst vorm'),
    read(Time_constraint),
    parse(Time_constraint).

parse([A, voor, B]):-
    assert(voor(A,B)).
    


