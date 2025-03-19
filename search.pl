%search.pl
%By: Sam Schmitz

%State will be a list of the locations of all blocks 
%used as a search state
initial_state(State) :- 
	findall(location(Block, Area, Height), location(Block, Area, Height), State). 

%checks the BeforeState to check if the put_on() command is valid
%if true AfterState will be the updated state
legal_move(BeforeState, put_on(Block1, Dest), AfterState) :-		
	block(Block1),
	(block(Dest); area(Dest)),
	\+ any_above(Block1, BeforeState),
	(block(Dest) -> (\+ any_above(Dest, BeforeState), shape(Dest, cube)); true),
	Block1 \= Dest,
	member(location(Block1, _, H1), BeforeState),	
	(
		block(Dest),
		member(location(Dest, Area, Height1), BeforeState),
		%H1 =\= Height1,
		Height2 is Height1 + 1
	;
		area(Dest),
		\+ member(location(_, Dest, _), BeforeState),
		Area = Dest,
		Height2 is 1
	),
	select(location(Block1, _, _), BeforeState, TempState),
	AfterState = [location(Block1, Area, Height2) | TempState]. 

%Checks to see if there are any blocks above Block in the State
any_above(Block, State) :-
	member(location(OtherBlock, Area, HeightAbove), State),
	member(location(Block, Area, HeightBelow), State),
	HeightAbove is HeightBelow + 1. 

%Max searching depth
max_depth(5).

%checks if State is a GoalState
%checks if Block1 is on Block2 within the state
holds(State, on(Block1, Block2)) :-
	member(location(Block1, Area, H1), State),
	member(location(Block2, Area, H2), State),
	H1 is H2 + 1. 

:- use_module(sets).

%Search algorithm for the model. 
%Plan = the plan created by the algorithm, Goal is the goal state supplied by the user (on(Block1, Block2))
idcp_plan(Plan, Goal) :-
    initial_state(Initial), idcp_plan(Initial, Plan, Goal).  

%Helper function for the idcp_plan predicate
%recursively checks a growing list for a solution
idcp_plan(I, L, Goal):-
    list_to_set([I], Visited),  % generate set of states already seen
    generate_list(L),
    reachable(I, Visited, L, FinalState),
	holds(FinalState, Goal). 


% reachable(S0, Visited, Actions, S)
%Avoids states that have already been visited (Cycle Prevention)
reachable(S, _, [], S). 
reachable(S0, Visited0, [M|L], S) :- 
    legal_move(S0, M, S1),
    ( \+ member(S1, Visited0) ->  
        set_add_new(Visited0, S1, Visited1),
        reachable(S1, Visited1, L, S)
    ;  
        fail  % Prevent unnecessary re-expansion
    ).
    %set_add_new(Visited0, S1, Visited1),  
    %reachable(S1, Visited1, L, S).

generate_list(L) :-
    max_depth(Max),
    between(0, Max, Len),
    length(L, Len).

%executes a plan created by idcp_plan()
execute_plan([]) :-
	format("Plan execution complete.~n"). 
execute_plan([put_on(Block1, Block2)|Rest]) :-
	format("Putting ~w on ~w~n", [Block1, Block2]), 
	call(put_on(Block1, Block2)) -> execute_plan(Rest). 

%Creates and executes a plan to move Block to Dest
achieve_on(Block, Dest) :-
	idcp_plan(Plan, on(Block, Dest)), 
	execute_plan(Plan). 