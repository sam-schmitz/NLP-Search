%world_model.pl
%By: Sam Schmitz

block(block_a). block(block_b). block(block_c). block(block_d).
block(block_e). block(block_f). block(block_g).

area(area_1). area(area_2). area(area_3).
area(area_4). area(area_5).

%shape options: pyramid, cube, wedge
shape(block_a, pyramid). shape(block_g, pyramid). 
shape(block_b, cube). shape(block_c, cube). shape(block_d, cube). 
shape(block_e, cube). shape(block_f, cube). 

%colors used: blue, red, green, yellow
color(block_a, blue). color(block_d, blue). color(block_f, blue). 
color(block_b, red). color(block_e, red). 
color(block_c, green).
color(block_g, yellow). 

size(block_a, small). size(block_b, large). size(block_c, large). size(block_d, large). 
size(block_e, large). size(block_f, large). size(block_g, large). 

%location(Block, Area, Height)
location(block_a, area_1, 2). location(block_b, area_1, 1). 
location(block_c, area_2, 3). location(block_d, area_2, 2). location(block_e, area_2, 1). 
location(block_f, area_3, 1). location(block_g, area_4, 1). 

%which areas are next to each other
next_to(area_1, area_2). next_to(area_2, area_3).
next_to(area_3, area_4). next_to(area_4, area_5). 

%beside(Block1, Block2) tells if blocks are at the same height and in neighboring areas
beside(X, Y) :- 
	location(X, A, Z), 
	location(Y, B, Z), 
	next_to(A, B). 
beside(X, Y) :- 
	location(X, A, Z), 
	location(Y, B, Z), 
	next_to(B, A). 

%above(Block1, Block2) tells if Block1 is above Block2
above(X, Y) :-
	location(X, Z, A), 
	location(Y, Z, B), 
	A > B. 

%on(Block1, Block2) tells if Block1 is directly on top of Block2
on(X, Y) :-
	location(X, Z, A),
	location(Y, Z, B), 
	A is B + 1. 

%Puts Block1 onto Dest
%Dest can be a block or area
put_on(Block1, Dest) :-
	block(Block1),
	block(Dest),
	\+ any_above(Block1), 
	\+ any_above(Dest),
	shape(Dest, cube), 
	location(Dest, Area, Height1),
	retract(location(Block1, _, _)),
	Height2 is Height1 + 1,
	assert(location(Block1, Area, Height2)). 
put_on(Block1, Dest) :-
	block(Block1),
	area(Dest),
	\+ any_above(Block1), 
	\+ location(_,Dest,_),
	retract(location(Block1, _, _)),
	Height2 is 1,
	assert(location(Block1, Dest, Height2)).  

%any_above(Block) tells if there are any blocks above Block
any_above(X) :- above(_, X). 

:- dynamic location/3. 