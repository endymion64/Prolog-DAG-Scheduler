:- module(speedup, [speedup/2]).
:- use_module(execution_time,[execution_time/2]).

speedup(Solution,Speedup) :-
    execution_time(Solution,ETS),
    find_best_sequential_solution(SeqSol),
    execution_time(SeqSol,ET1),
    Speedup is ET1/ETS.

:- dynamic best_sequential/2.

find_best_sequential_solution(_) :-
    current_prolog_flag(max_tagged_integer,Init),
    assert(best_sequential(nil,Init)), 
    sequential_solution(Solution),
    execution_time(Solution,Time),
    update_best_sequential(Solution,Time),
    fail.
find_best_sequential_solution(X) :-
    best_sequential(X,_),
    retract(best_sequential(_,_)).

update_best_sequential(X,ValueX) :- 
    best_sequential(_,ValueBest), 
    ValueX < ValueBest, 
    !, 
    retract(best_sequential(_,_)),
    assert(best_sequential(X,ValueX)).
update_best_sequential(_,_).

%% sequential_solution(-Schedules)
%% Generates an execution schedule where all tasks are scheduled on one core in numerical order
sequential_solution(solution(Solution)) :-
    findall(X,core(X),Cores),
    findall(X,task(X),Tasks),
    sort_tasks(Tasks,SortedTasks),
    sequential_solution(Solution,Cores,SortedTasks).

%% sort_tasks(+Tasks,-SortedTasks)
%% Sorts a list of task atoms numerically
%% Internally, it uses sort/2, which means that duplicates are removed!
sort_tasks(Tasks,SortedTasks) :-
    maplist(task_to_int,Tasks,Ints),
    sort(Ints,SortedInts),
    maplist(int_to_task,SortedInts,SortedTasks).

%% task_to_int(+Task,-Value) 
%% Transforms the atom of a task/1 functor into an integer
task_to_int(Task,Value) :-
    task(Task),
    atom_codes(Task,X),
    [_|Tail] = X, 
    number_chars(Value,Tail).

%% int_to_task(+Int,-Task)
%% Transforms an integer to the atom of a task/1 functor
int_to_task(Value,Task) :-
    atomic_concat(t,Value,Task),
    task(Task).

%% sequential_solution(-Schedules,+Cores,+Tasks)
%% Generates an execution schedule where all elements of Tasks are scheduled in numerical order on one core of Cores
sequential_solution([],[],[]) :- !.
sequential_solution([schedule(Core,ScheduledTasks)|Schedules],[Core|Cores],ScheduledTasks) :-
    ScheduledTasks \= [],
    core(Core),
    sequential_solution(Schedules,Cores,[]) .
sequential_solution([schedule(Core,[])|Schedules],[Core|Cores],Tasks) :- 
    core(Core),
    sequential_solution(Schedules,Cores,Tasks).