%lexicon.pl
%By: Sam Schmitz

article(a).  article(the). article(any). 

common_noun(block, X) :- block(X). 
common_noun(pyramid, X) :- block(X), shape(X, pyramid). 
common_noun(cube, X) :- block(X), shape(X, cube). 
common_noun(wedge, X) :- block(X), shape(X, wedge). 

adjective(large, X) :- size(X, large). 
adjective(small, X) :- size(X, small). 
adjective(red, X) :- color(X, red). 
adjective(blue, X) :- color(X, blue). 
adjective(green, X) :- color(X, green). 
adjective(yellow, X) :- color(X, yellow). 

preposition(above, X, Y) :- above(X, Y). 
preposition(below, X, Y) :- above(Y, X). 
preposition(beside, X, Y) :- beside(X, Y). 
%preposition(on, X, Y) :- on(X, Y). 

proper_noun(X,X) :- \+ article(X), \+ adjective(X,_), 
                    \+ common_noun(X,_), \+ preposition(X,_,_).