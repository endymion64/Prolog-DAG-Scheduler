:- module(find_optimal, [find_optimal/1]).
:- use_module(execution_time,[execution_time/2]).
:- use_module(solution,[solution/1]).
:- dynamic best/2.

%% find_optimal(-S)
%% Generates the optimal execution schedule minimizing execution time
find_optimal(_) :- 
    current_prolog_flag(max_tagged_integer,Init),
    assert(best(nil,Init)), 
    solution(X), 
    execution_time(solution(X),ValueX), 
    update_best(solution(X),ValueX),
    fail.
find_optimal(X) :- 
    best(X,_),
    retract(best(_,_)).

update_best(X,ValueX) :- 
    best(_,ValueBest), 
    ValueX < ValueBest, 
    !, 
    retract(best(_,_)),
    assert(best(X,ValueX)).
update_best(_,_).