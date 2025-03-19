np([Name],X) :- proper_noun(Name,X).
np([Art|Rest],X) :- article(Art), np2(Rest,X).
%np([Verb|Rest], X) :- verb(Verb), np2(Rest, X). 

np2([Adj|Rest],X) :- adjective(Adj,X), np2(Rest,X). 
np2([Noun|Rest],X) :- common_noun(Noun,X), mods(Rest,X). 

mods([],_). 
mods(Words,X) :- 
   append(Start,End,Words),   % Break the words into two pieces.
   pp(Start,X),               % The first part is a PP.
   mods(End,X).               % The last part is a Mods again.

pp([Prep|Rest],X) :- preposition(Prep,X,Y), np(Rest,Y).

%handles put NP on NP and achieve NP on NP commands
command(Words) :-
	append([put|NP1], [on|NP2], Words),
	np(NP1, Block1),
	np(NP2, Block2),
	put_on(Block1, Block2). 
command(Words) :-
	append([achieve|NP1], [on|NP2], Words),
	np(NP1, Block1),
	np(NP2, Block2),
	achieve_on(Block1, Block2). 
