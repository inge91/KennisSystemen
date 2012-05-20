% Kennis Systemen Opdracht 1.2 Dataset
% 6093906 Inge Becht

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
attribute(amfibian, legs, 0/4).
attribute(reptile, legs, 0/4).
attribute(african_green_tree_frog, legs, 4/4).
attribute(platypus, legs, 4/4).
attribute(dolphins, legs, 0/0).
attribute(boomslang, legs, 0/0).
attribute(cow, legs, 4/4).
attribute(animal, living, 1/1).
attribute(mammal, warmblooded, 1/1).
attribute(amfibian, warmblooded, 0/0).
attribute(reptile, warmblooded, 0/0).
attribute(fish, warmblooded, 0/0).
attribute(reptile, nipples, 0/0).
attribute(fish, nipples, 0/0).
attribute(amfibian, nipples, 0/0).
attribute(mammal, nipples, 0/4).
attribute(cow, nipples, 4/4).
attribute(platypus, nipples, 0/0).
attribute(dolphins, nipples, 2/2).
attribute(mammal, eggs, 0/0).
attribute(amfibian, eggs, 1/200).
attribute(reptile, eggs, 1/200).
attribute(fish, eggs, 1/200).
attribute(boomslang, eggs, 1/1).
attribute(stringray, eggs, 1/1).
attribute(turtle, eggs, 50/200).
attribute(african_green_tree_frog, eggs, 100/200).

% opdracht 1.3
% de prettyprint, verder niks over toe te lichten

show:-
	isaprint.
	
isaprint:-
	findall(X, isa(X, _), List1),
	findall(Y, isa(_, Y), List2),
    only_animals(List1, List2, Animals),
	print_animals(Animals).

% we want things only in the first
% list and not in the second one
only_animals([], _, []).

only_animals([H|T], List, [H|Rest]):-
    \+member(H, List),
    only_animals(T, list, Rest).

only_animals([_|T], List, Rest):-
    only_animals(T, List, Rest).

print_animals([]).
print_animals([H|T]):-
    writef("%t is a ", [H]),
    all_predecessors(H),
    write('\nAttributes:\n'),
    all_attributes(H, []),
    write('\n'),
    print_animals(T).

all_predecessors(R):-
   isa(R, thing),
   write('thing'). 

all_predecessors(H):-
    isa(H, R),
    write(R),
    write(', is a '),
    all_predecessors(R).

all_attributes(H, History):-
    isa(H, thing),
    findall(X, attribute(H, X, _), List1),
	findall(Z, attribute(H, _, Z/_), List2),
	findall(W, attribute(H, _, _/W), List3),
    print_values(List1, List2, List3, History) .

all_attributes(H, History):-
    findall(X, attribute(H, X, _), List1),
	findall(Z, attribute(H, _, Z/_), List2),
	findall(W, attribute(H, _, _/W), List3),
    print_values(List1, List2, List3, History),
    isa(H, R),
    append(List1, History, NewHistory),
    all_attributes(R, NewHistory).


print_values([], [], [], _).

print_values([H|L], [H1|L1], [H2|L2],History):-
    print_value(H, H1, H2, History),
    print_values(L, L1, L2, History).

print_value(Attr, 1, 1, History):-
    \+member(Attr, History),
    write('   -'),
    write(Attr),
    write('\n').

print_value(Attr, 0, 0, History):-
    \+member(Attr, History),
    write('   -'),
    write('not '),
    write(Attr),
    write('\n').

print_value(Attr, V, V, History):-
    \+member(Attr, History),
    write('   -'),
	write(Attr),
	write(' with value '),
	writeln(V).

print_value(Attr,  V, V2, History):-
    \+member(Attr, History),
    write('   -'),
	write(Attr),
	write(' with value '),
	write(V),
    write(' to '),
    writeln(V2).

print_value(_,_,_,_).


% opdracht 4, de functie die nieuwe dingen toevoegt. go chortcuts zijn onderaan
% te vinden. concepten wordt aangeroepen om de loop te verzorgen

%adding new concept works as follows:
% first there is a check if the concept is already found in the tree.
% if so, the user gets a warning that the concept is not addded because it already exists
% In case the concept exists we look at its attributes. All attributes that are already hiher up in the tree
% will be ignored(as well as attributes already contained by the concept himself in the tree.
% after this is done the process terminates
% in the case there is a new concept and no parent is specified,
% we look at all the attributes that are given with the new concept. If sufficient attributes are
% specified for a given parent than we choose that parent to hang the new concept under (for instance,
% if we want something to be classified as a mammal we need at least the attributs warmblooded, eggs, nipples and legs described for our new concept). 
% if no such parent can be found we add the new concept to thing.
% if there is a parent given already we just hang it under that parent and add the attributes which are not described by parental nodes.
	
go1:-
	write('Adding concept car to the tree, with value metal 1/1\n'),
    add_new_concept(car, [[metal, 1, 1]],[]).
	
go2:-
	write('Adding concept horse with attribute nipples(2/2), legs(4/4), warmblooded, no eggs, 1 tail\n'),
    add_new_concept(horse, [[nipples, 2, 2],[legs, 4,4 ],[warmblooded, 1,1],[eggs, 0,0],[tail, 1,1]], []).
	
go3:-
	write('Adding new concept lizzard under amfibian with attribute tail(1/1)\n'),
	add_new_concept(lizzard, [[tail, 1, 1]], [amfibian]).

go4:-
	write('Adding new attribute horns to cow\n'),
	add_new_concept(cow, [[horns, 2,2]], []).

% the first predicate that checks if the concept already is in the tree
add_new_concept(Concept, Attrs, Isa):-
	% first check if the Concept is already in the database
	((concept_known(Concept),
	write('Concept already in tree!\nChecking attributes...\n'),
	% if that is the case you add the Attributes
	 add_attrs(Concept, Attrs));
	% else you start the Conceptadder
	unknown_concept_adder(Concept, Attrs, Isa)
	 ).

% adding a concept of which no isa relation has been given 
unknown_concept_adder(Concept, Attrs, []):-
	%if there are no concepts given or no ancestors can be found 
	((length(Attrs,X),
	  X =:= 0,
	  write('There is insufficient information on where to place the concept, 
	so we place it under thing\n')) ;
	(finding_possible_ancestors(Attrs, AttEmpty),
	AttEmpty ==[[]],
	write('The given attributes do not match with a possible parent, so we place it under thing\n'));
	(finding_possible_ancestors(Attrs, Ancestors),
	flatten(Ancestors, An),
	right_ancestor(An, An2),
	An2 == [],
	write('The given attributes do not match with a possible parent, so we place it under thing\n'))),
	assert(isa(Concept, thing)),
	% we still add the attributes
	add_attrs(Concept, Attrs).
	
% in case a parent can be found.
unknown_concept_adder(Concept, Attrs, []):-
	finding_possible_ancestors(Attrs, AncestorsPerAttr),
	flatten(AncestorsPerAttr, AncestorsPerAttr2),
	right_ancestor(AncestorsPerAttr2, An),
	writef("Adding %t under %t\n", [Concept, An]),
	assert(isa(Concept, An)),
	add_attrs(Concept, Attrs).

% adding an unknown concept in the case of having a known parent
unknown_concept_adder(Concept, Attrs, Isa):-
	length(Isa, Br),
	Br =:= 1,
	member(X, Isa),!,
	% check if the parent is known in the tree
	((isinisa(X),
	 writef("the parent %t is already in the tree. We place Concept under it\n", [Isa, Concept]),
	 assert(isa(Concept, X)),
	 add_attrs(Concept, Attrs)
	);
	(% if not, add the parent and Concept to the tree under thing
	writef("The parent of %t was not recognised and will be inserted at thing level", [Concept]),
	assert(isa(X, thing)),
	assert(isa(Concept, X)),
	write('Adding attributes...\n'),
	add_attrs(Concept, Attrs)
	)). 

% case of assing more than 1 direct parent
unknown_concept_adder(Concept, _, _):-
	writef("You are trying to add more than one direct parent to %t. \n This is not possible!\n",[Concept]).

% right ancetor checks if there can be a ancestor found
% with all the given new attributes of the concept
% this succeeds only if all possible attributes of a parent
% is matched with the attributes given by the concept
right_ancestor([], []):-!.
	
right_ancestor([X|AncestorsPerAttr], X):-
	all_occurences(X, AncestorsPerAttr, List),
	findall(Y, attribute(X, Y, _) ,List2),
	length(List, Bla), 
	length(List2, Bla2),
	(Bla + 1) =:= Bla2,!.
	
right_ancestor([X|AncestorsPerAttr], R):-
	all_occurences(X, AncestorsPerAttr, List),
	findall(Y, attribute(X, Y, _) ,List2),
	length(List, Bla), 
	length(List2, Bla2),
	\+ (Bla +1)=:= Bla2,
	exclude(X, AncestorsPerAttr, AncestorsNew),
	right_ancestor(AncestorsNew, R),!.	

% exclude X from the output list
exclude(X, [H|T], [H|R]):-
	exclude(X, T, R).
	
exclude(X, [X|T], R):-
	exclude(X, T, R).

% output gets all the duplicates of first argument in second list
all_occurences(_, [], []).

all_occurences(X, [X|Ancestors], [X|List]):-
	all_occurences(X, Ancestors, List).
	
all_occurences(X, [_|Ancestors], List):-
	all_occurences(X, Ancestors, List).	

finding_possible_ancestors([], []).	

finding_possible_ancestors([[Attr, Val1, Val2]|Attrs], [List|Ancestors]):-
	findall(X, (attribute(X, Attr, Y/Z), Y=<Val1, Z >= Val2), List),
	finding_possible_ancestors(Attrs, Ancestors).
	
	

% The concept is known if it can be found in the tree	
concept_known(Concept):-
	(isa(Concept, _);
	isa(_, Concept)).

% adds attributes to a concept	
add_attrs(_, []).	

% Attrs form: lists of [ Attr, Val1, Val2]
add_attrs(Concept, [[Attr, Val1, Val2] |Attrs]):-
	%first check if there are ancestors that specify the same attribute
	
	ancestors(Concept, Ancestors),
	% if the attribute is not exactly like one of the ancestors
	% then add the new attribute. Else just go to the following attribute
	((check_attr(Ancestors, Attr, Val1, Val2),	
	\+attribute(Concept, Attr, _/_),
	writef("added new attribute %t to %t with value %t - %t\n", [Attr, Concept, Val1, Val2]),
	assert(attribute(Concept, Attr, Val1/Val2)),
	add_attrs(Concept, Attrs))
	;
	writef("Attribute %t was already in another part of the tree and will not be added\n", [Attr]),
	(add_attrs(Concept, Attrs))).

%adds all new isa values
add_isa(Concept, [H|Isa]):-
	% concruct all already known ancestors
	ancestors(Concept, Ancestors),
	% we only want to add new attributes so we filter those
	((\+member(H, Ancestors),
	 writef("Added %t as new ancestor of %t\n", [H, Concept]),
	 assert(isa(Concept, H),
	 add_isa(Concept, Isa)))
	 ;
	 (writef("Concept %t alrady has ancestor %t\n", [Concept, H]),
	 add_isa(Concept, Isa))).

% gives all ancestors of a given Concept MULTIPLE ANCESTORE LINES SHOULD ALSO BE AN OPTION??
ancestors(Concept, []):-
	\+isa(Concept, _).
	
ancestors(Concept, [An|Cestors]):-
	isa(Concept, An),
	ancestors(An, Cestors).
	

	
% chck if any of the ancestors havet his attribute.
check_attr([], _, _, _).

check_attr([H|Ancestors], Attr, Val1, Val2):-
	\+attribute(H, Attr, Val1/Val2),
	check_attr(Ancestors, Attr, Val1, Val2).
	
	
	



	
