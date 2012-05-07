% Kennis Systemen Opdracht 1.2 Dataset
% 6093906 Inge Becht
% 6286658 Maarten de Jonge
% alle attributen en relaties van dieren worden hier beschreven.
% in geval van de attributen is er gekozen om links te maken tussen de waardes
% die een attribuut kan hebben door middel van de value restrictions.
% in het geval van bijvoorbeeld een schildpad is een reptiel een schildpad in het geval
% dat deze tussen de 50-200 eieren legt.
% In geval van attributen met waarheiswaarde betekent 1/1 altijd waar
% en 0/0 altijd false (daar lijkt het KL-ONE representatie zich niet goed
% voor te lenen...

:- dynamic isa/2.
:- dynamic attribute/3.

% isa weergeeft de hierarchische structuur waarbij het linker argument
% onder het rechter argument staat in de structuur
isa(animal, thing).
isa(mammal, animal).
isa(amfibian, animal).
isa(reptile, animal).
isa(fish, animal).
isa(cow, mammal).
isa(platypus, mammal).
isa(dolphin, mammal).
isa(african_green_tree_frog, amfibian).
isa(turtle, reptile).
isa(boomslang, reptile).
isa(stingray, fish).

% attribute geeft het eigenschap(tweede argument
% van het eerste argument aan, waarbij het derde argument
% voor waarheidswaarde of aantal kan staan
% mogelijk staan niet alle vebrindingen er in,
% maar het principe moge duidelijk zijn
attribute(mammal, legs, 0/4).
attribute(amphibian, legs, 0/4).
attribute(reptile, legs, 0/4).
attribute(african_green_tree_frog, legs, 4/4).
attribute(platypus, legs, 4/4).
attribute(dolphins, legs, 0/0).
attribute(boomslang, legs, 0/0).
attribute(cow, legs, 4/4).
attribute(animal, living, 1/1).
attribute(mammal, warmblooded, 1/1).
attribute(amphibian, warmblooded, 0/0).
attribute(reptile, warmblooded, 0/0).
attribute(fish, warmblooded, 0/0).
attribute(reptile, nipples, 0/0).
attribute(fish, nipples, 0/0).
attribute(amphibian, nipples, 0/0).
attribute(mammals, nipples, 0/4).
attribute(cow, nipples, 4/4).
attribute(platypus, nipples, 0/0).
attribute(dolphins, nipples, 2/2).
attribute(mammal, eggs, 0/0).
attribute(amphibian, eggs, 1/200).
attribute(reptile, eggs, 1/200).
attribute(fish, eggs, 1/200).
attribute(boomslang, eggs, 1/1).
attribute(stringray, eggs, 1/1).
attribute(turtle, eggs, 50/200).
attribute(african_green_tree_frog, eggs, 100/200).

% opdracht 1.3
% de prettyprint, verder niks over toe te lichten

show:-
	isaprint,
	write('\n\n'),
	attributeprint.
	
isaprint:-
	findall(X, isa(X, _), List1),
	findall(Y, isa(_, Y), List2),
	printingisa(List1, List2).

printingisa([],[]).
	
printingisa([Sub|Tail], [Top|Tail2]):-
	write(Sub),
	write(' is a direct subclass of '),
	write(Top),
	write('\n'),
	printingisa(Tail, Tail2).

attributeprint:-
	findall(X, attribute(X, _,_), List1),
	findall(Y, attribute(_, Y, _), List2),
	findall(Z, attribute(_, _, Z/_), List3),
	findall(W, attribute(_, _, _/W), List4),
	printingattribute(List1, List2, List3, List4).

printingattribute([], [], [], []).

printingattribute([Hanimal|Tail], [Hattr| Tail2], [Hnum|Tail3], [Hnum2| Tail4]):-
    (valuetrue(Hanimal, Hnum, Hnum2, Hattr);
    (valuezero(Hanimal, Hnum, Hnum2, Hattr);
    (valuesame(Hanimal, Hnum, Hnum2, Hattr);
    valueelse(Hanimal, Hnum, Hnum2, Hattr)))),
	write('\n'),
	printingattribute(Tail, Tail2, Tail3, Tail4).

valuetrue(Hanimal, Hnum, Hnum2, Hattr):-
	Hnum == 1,
	Hnum2 == 1,
	write(Hanimal),
	write(' has attribute '),
	write(Hattr).

valuezero(Hanimal, Hnum, Hnum2, Hattr):-
	Hnum == 0,
	Hnum2 == 0,
	write(Hanimal),
	write(' has not attribute '),
	write(Hattr).

valuesame(Hanimal, Hnum, Hnum2, Hattr):-
	Hnum == Hnum2,
	write(Hanimal),
	write(' has attribute '),
	write(Hattr),
	write(' with value '),
	write(Hnum).

valueelse(Hanimal, Hnum, Hnum2, Hattr):-
	write(Hanimal),
	write(' has attribute '),
	write(Hattr),
	write(' with value '),
	write(Hnum),
	write(' to '),
	write(Hnum2).
	
printer([]).
printer([H|T]):-
	writeln(H),
	printer(T).

% opdracht 4, de functie die nieuwe dingen toevoegt. go chortcuts zijn onderaan
% te vinden. concepten wordt aangeroepen om de loop te verzorgen

% eerste element is de naam van het nieuwe concept, 
% tweede argument de attributen in vlgorde
% dere argument lijstbijbehorende waardes in volgorde van de attrivuten.
concepten(Concept, Attrs, Values):-
    % kijk eerst of het concept al in de boom staat
    (isinisa(Concept);
    List = [],
    List2 = [],
    %%% addisa en addnotisa werken niet,
    % maar het idee was dat de ene de concepten
    % teruggeeft die identieke attributen heeft aan die van het toe te 
    % voegen concept
    % en  addnotisa die niet overeenkomen met die van het nieuwe concept.
    % zo wordt het nieuwe concept in supergroeps alleen toegevoegd
    % aan die concepten die geheel overeenkomen met het nieuwe concept.. maar
    % de implementatie werkt helaas niet.
    addisa(Concept, Attrs, Values, List),
    addnotisa(Concept, Attrs,Values, List2),
    Element = [],
    supergroups(Element,List, List2),
    % als het nieuwe concept geen attributen gemeen heeft
    % met al bestaande concepten dan voeg je die toe aan thing
    % en anders aan dat concept
    (empty(Element, Concept, Attrs, Values);
    addelement(Element, Concept))),
    % voeg alleen oude elementen toe
    addattrunchecked(Concept, Attrs, Values).


supergroups([],_,_). 

% voeg isa toe waar directe verband is tussen het
% concept en de gegeven Elements(die al in de chart staan
addelement([],_).
addelement([H|Elements], Concept):-
     assert(isa(Concept, H)),
     addelement(Elements, Concept).

%adds concept directly to Thing if it doesnt share attributes with other
%concepts.
empty(Elem, Concept, Attrs, Values):-
    Elem == [],
    assert(isa(Concept, thing)),
    write('added new concept'),
    write( Concept),
    write(' isa thing \n'),
    addattr(Concept, Attrs, Values).

% voeg alle attributen toe die hier staan in combinatie met het toe te voegen 
% concept en de values
addattr(_, [], []).
addattr(Concept, [H|Attrs], [H2|Values]):-
    assert(attribute(Concept,H, H2 )),
    write('added attribute ' ),
    write(H),
    write(' with value '),
    write(H2),
    write(' to '),
    write(Concept),
    write('\n'),
    addattr(Concept, Attrs, Values).


% kijk of er een kwestie is van inheritance
hierarchyvalue(thing, _, _).

hierarchyvalue(Concept, Att, Value):-
    \+ attribute(Concept, Att, Value),
    isa(Concept, Upper),
    hierarchyvalue(Upper, Att, Value).


% addattrunchecked gebruikt eigen filter om te bepalen of de waarde al overefd
% wordt
addattrunchecked(_, [] ,[] ).
addattrunchecked(Concept, [H|Attr], [H2|Value]):-
    ((hierarchyvalue(Concept, H, H2),
    assert(attribute(Concept, H, H2)),
    write('added '),
    write(H),
    write(' as attribute of '),
    write(Concept),
    write(' with value of '),
    write(H2))
    ;
    write(' not added because of inheritance')),
    addattrunchecked(Concept, Attr, Value).

addnotisa(_, [], [], []).
addnotisa(_, [], [], _).
addnotisa(Concept, [Hattr|Attrs], [H2|Values], Rest):-
    findall(Concepts, attribute(Concepts, Hattr, H2), List),
    addnotisa(Concept, Attrs, Values, [List| Rest] ).


% het concept staat al ergens in de boom
isinisa(Concept):-
    (isa(Concept,_);
    isa(_, Concept)).

% addisa\4 makes a list of lists of concepts that have the same value attributes
% as new concept
addisa(_, [], [], []).

addisa(_, [], [], _).

addisa(Concept, [Hattr|Attrs], [H2|Values], Rest):-
    findall(Concepts, attribute(Concepts,Hattr, H2), List),
    addisa(Concept, Attrs,Values, [List| Rest] ).

% auto adden werkt goed; wordt onder Thing geplaatst
go1:-
    concepten(car, [],[]).

%werkt goedl wordt onder thing geplaats en als die er al staat wordt alleen het
%attribuut toegevoegd( en als die er al staat wordt die niet opnieuw
%toegevoegd).
go2:-
    concepten(car, [metal], [1/1]).

%warmblooded makes steve_irwin a mammel
go3:-
    concepten(steve_irwin, [warmblooded], [1/1]).
