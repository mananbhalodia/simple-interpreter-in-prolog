member(X,[X]).
member(X,[X|_]).
member(X,[_|R]):-member(X,R).

union([X|Y],Z,W) :- member(X,Z), !, union(Y,Z,W).
union([X|Y],Z,[X|W]) :- \+ member(X,Z), !, union(Y,Z,W).
union([],Z,Z).

intersection([X|Y],Z,[X|W]) :- member(X,Z), !, intersection(Y,Z,W).
intersection([X|Y],Z,W) :- \+ member(X,Z), !, intersection(Y,Z,W).
intersection([],_,[]).

difference([X|Y],Z,W) :- member(X,Z), !, difference(Y,Z,W).
difference([X|Y],Z,[X|W]) :- \+ member(X,Z), !, difference(Y,Z,W).
difference([],_,[]).

subset([X|Y],Z) :- member(X,Z), !, subset(Y,Z).
subset([],_).

equal(X,Y) :- subset(X,Y), !, subset(Y,X).

make_pairs(X,[Y|Z],[(X,Y)|W]) :- make_pairs(X,Z,W).
make_pairs(_,[],[]).

append([],X,X).
append([X|Y],Z,[X|W]) :- append(Y,Z,W).

cartesian_product([X|Y],Z,W) :-
	make_pairs(X,Z,A), append(A,B,W), !, cartesian_product(Y,Z,B).
cartesian_product([],_,[]).
