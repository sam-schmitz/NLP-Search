% id_plan
%  General planning algorithm using iterative-deepening search
% by: John Zelle (based on Chapter 9 of Thinking as Computation).

% A search problem is defined by these predicates:
%     - legal_move(BeforeState, Action, AfterState)
%     - initial_state(State)
%     - goal_state(State)


% Optimal planning via iterative deepening

% max_depth(depth): maximum depth to search to
max_depth(15).

% plan(L): L is a list of moves from the initial state to a goal state.
id_plan(L) :- initial_state(I), id_plan(I, L).

% This version takes initial state as first argument
id_plan(I, L) :-
    generate_list(L),   % produces lists of increasing length
    reachable(I, L, G),
    goal_state(G).

% reachable(S1, L, S2): S2 is reachable from S1 using moves in L.
reachable(S, [], S).           
reachable(S1, [M|L], S3) :-
    legal_move(S1, M, S2),
    reachable(S2, L, S3).

% generate lists of increasing length up to max_depth.
generate_list(L) :-
    max_depth(Max),
    between(0, Max, Len),
    length(L, Len).


% idcp_plan iterative deepening graph search (cycle prevention).

:- use_module(sets).

idcp_plan(L) :-
    initial_state(Initial), idcp_plan(Initial, L).
   
idcp_plan(I, L):-
    list_to_set([I], Visited),  % generate set of states already seen
    goal_state(G),
    generate_list(L),
    reachable(I, Visited, L, G).

% reachable(S0, Visited, Actions, S) -- like reachable/3 but avoiding states
%     that have already been visited on path (Visited set) to avoid cycles.
reachable(S, _, [], S).
reachable(S0, Visited0, [M|L], S):-
    legal_move(S0, M, S1),
    set_add_new(Visited0, S1, Visited1),  % fails if S1 already in Visited0
    reachable(S1, Visited1, L, S).

