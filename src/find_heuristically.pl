:- module(find_heuristically, [find_heuristically/1]).
:- use_module(execution_time, [task_term_time/3,
                               memo_task_term_time/2
                              ]).

%% find_heuristically(-solution(Schedules))
%% Heuristic algorithm to create an execution schedule
%% The implemented algorithm is Heterogeneous Earliest Finish Time (HEFT)
find_heuristically(solution(Schedules)) :-
    % Phase 1: Rank all tasks, starting from "the back" of the DAG (to benefit from memoize)
    findall(X,task(X),Tasks),
    reverse(Tasks, ReversedTasks),
    maplist(to_rank,ReversedTasks,Ranks),
    % Sort by decreasing rank
    quicksort_ranks(Ranks,>,SortedRanks),
    % Phase 2: Construct a solution by assigning the tasks to a core in the order of SortedRanks
    maplist(to_task,SortedRanks,SortedTasks),
    empty_schedule(Init),
    construct_solution(SortedTasks,Init,Schedules),
    retractall(memo_task_term_time(_,_)),
    !.

%% empty_schedule(-X)
%% Generates an empty execution schedule for the initialisation of the second phase of HEFT
empty_schedule(X) :-
    findall(Core,core(Core),Cores),
    empty_schedule(Cores,X).

empty_schedule([],[]) :- !.
empty_schedule([Core|Cores],[schedule(Core,[])|Schedules]) :-
    empty_schedule(Cores,Schedules).

%% construct_solution(+UnscheduledTasks,+OldSolution,-NewSolution)
construct_solution([],X,X) :- !.
construct_solution([UnscheduledTask|UnscheduledTasks],OldSolution,Solution) :-
    find_best_assignment(UnscheduledTask,OldSolution,NewSolution),
    construct_solution(UnscheduledTasks,NewSolution,Solution).

%% find_best_assignment(+Task,+OldSchedules,-NewSchedules)
%% Inserts Task into OldSchedules resulting into NewSchedules
%% The task is assigned to the core with the earliest finish time of the task
find_best_assignment(Task,OldSchedules,[schedule(Core,NewQueue)|Schedules]) :-
    find_best_core(Task,OldSchedules,Core),
    member(schedule(Core,Tasks),OldSchedules),
    delete_schedule(Core,OldSchedules,Schedules),
    append(Tasks,[Task],NewQueue).

%% delete_schedule(+Core, +Solution1, -Solution2)
%% Solution2 is Solution1 with the first occurance of schedule(Core,_) removed 
%% Fails if schedule(Core,_) does not occur in Solution1.
delete_schedule(Core,[schedule(Core,_)|T],T) :- !.
delete_schedule(Core,[H|T1],[H|T2]) :- 
    delete_schedule(Core,T1,T2).

:- dynamic best_core/2.

%% find_best_core(+Task,+OldSolution,-Core)
%% Determines to which core a task has to be assigned
find_best_core(Task,OldSolution,_) :-
    current_prolog_flag(max_tagged_integer,Init),
    assert(best_core(nil,Init)),
    core(Core),
    member(schedule(Core,Tasks),OldSolution),
    delete_schedule(Core,OldSolution,Schedules),
    append(Tasks,[Task],NewQueue),
    task_term_time(Task,[schedule(Core,NewQueue)|Schedules],FinishTime),
    retractall(memo_task_term_time(Task,_)),
    update_best_core(Core,FinishTime),
    fail.
find_best_core(_,_,Core) :-
    best_core(Core,_),
    retract(best_core(_,_)).

update_best_core(X,ValueX) :- 
    best_core(_,ValueBest), 
    ValueX < ValueBest, 
    !, 
    retract(best_core(_,_)),
    assert(best_core(X,ValueX)).
update_best_core(_,_).    

to_rank(Task,rank(Task,V)) :-
    task(Task),
    rank(Task,V).
to_task(rank(Task,_),Task) :-
    task(Task).

%% rank(+Task,-Value)
%% Ranks the task
%% The rank of a task 'i' is determined by the following formula:
%% rank(i) = average_process_cost(i) + max(rank(j)+average_communication_cost(i,j))
%% where task 'j' is a successor of 'i' in the DAG
%% In order to make this predicate feasible for large DAGs, it uses memoize (caching)
:- dynamic memo_rank/2.

rank(Task,Value) :- memo_rank(Task,Value),!.
rank(Task,Value) :-
    task(Task),
    avg_process_cost(Task,AvgWeight),
    successors_dag(Task,Successors),
    Successors \= [],
    !,
    maplist(rank,Successors,Ranks),
    avg_comm_cost_list(Task,Successors,AvgCommCosts),
    maplist(my_plus,Ranks,AvgCommCosts,SuccessorValues),
    max_list(SuccessorValues,SuccessorValue),
    Value is AvgWeight+SuccessorValue,
    assert(memo_rank(Task,Value)).
rank(Task,Value) :-
    task(Task),
    avg_process_cost(Task,Value),
    assert(memo_rank(Task,Value)).

%% avg_process_cost(+Task, -Value)
%% Calculates the average process cost of a task
avg_process_cost(Task,Value) :-
    findall(Cost,process_cost(Task,_,Cost),Costs),
    length(Costs,L),
    sum_list(Costs,Sum),
    Value is Sum/L.

successors_dag(Task,Successors) :-
    findall(X,depends_on(X,Task,_),Successors).

%% avg_comm_cost_list(+Task,+Successors,-AvgCommCosts) 
%% Iteratively calls avg_comm_cost/3 for all elements of the list Successors
avg_comm_cost_list(_,[],[]) :- !.
avg_comm_cost_list(Task,[Successor|Successors],[AvgCommCost|AvgCommCosts]) :- 
    avg_comm_cost(Task,Successor,AvgCommCost),
    avg_comm_cost_list(Task,Successors,AvgCommCosts).

%% avg_comm_cost(+Task,+Successor,-AvgCommCost)
%% Calculates the average communication cost between a task and one its successors
avg_comm_cost(Task,Successor,AvgCommCost) :-
    depends_on(Successor,Task,Data),
    avg_bandwith(B),
    avg_latency(L),
    AvgCommCost is L + (Data/B),
    !.

:- dynamic bandwith/1.

avg_bandwith(B) :-
    retractall(bandwith(_)),
    %% Since the channel clauses contain cuts, we cannot simply use findall/3
    gather_bandwiths,
    findall(X,bandwith(X),Bandwiths),
    length(Bandwiths,L),
    sum_list(Bandwiths,Sum),
    B is Sum/L.

gather_bandwiths :-
    core(C1),
    core(C2),
    C1 \= C2,
    channel(C1,C2,_,Bandwith),
    assert(bandwith(Bandwith)),
    fail.
gather_bandwiths.

:- dynamic latency/1.

avg_latency(L) :-
    retractall(latency(_)),
    %% Since the channel clauses contain cuts, we cannot simply use findall/3
    gather_latencies,
    findall(X,latency(X), Latencies),
    length(Latencies,N),
    sum_list(Latencies,Sum),
    L is Sum/N.

gather_latencies :-
    core(C1),
    core(C2),
    C1 \= C2,
    channel(C1,C2,Latency,_),
    assert(latency(Latency)),
    fail.
gather_latencies.

%% my_plus(+A,+B,-C)
%% Trivial predicate, similar as plus/3, but is able to add all kinds of numbers instead of solely intergers
%% Used for list manipulation
my_plus(A,B,C) :- C is A+B.

%% rank_compare(+Delta,+Rank1,+Rank2)
%% Predicate to compare rank/2 functors by their value
%% Delta should be either > or <
rank_compare(Delta,rank(_,V1),rank(_,V2)) :-
    compare(Delta,V1,V2).

%% rank_reverse_compare_or_equal(+Delta,+Rank1,+Rank2)
%% Predicate to compare rank/2 functors by their value by the opposite operator of Delta
%% Delta should be either > or <
rank_reverse_compare_or_equal(Delta,rank(_,V1),rank(_,V2)) :-
    compare(Delta,V2,V1);
    compare(=,V2,V1).

%% quicksort_ranks(+Xs,+Delta,-Ys)
%% An adaption of the quicksort algorithm using difference lists provided in the course slides
%% This algorithm is supposed to sort rank/2 functors
%% Delta is an operator used to determine the order in which to sort the ranks
%% Delta should be either < or >
quicksort_ranks(Xs,Delta,Ys) :- qsort(Xs,Ys-[],Delta),!.
qsort([],Ys-Ys,_).
qsort([X0|Xs],Ys-Zs,Delta) :-
    my_partition(Xs,X0,Ls,Bs,Delta),
    qsort(Bs,Ys2-Zs,Delta),
    qsort(Ls,Ys-[X0|Ys2],Delta).

my_partition([],_,[],[],_).
my_partition([Head|Tail],N,[Head|Littles],Bigs, Delta) :-
    rank_compare(Delta,Head,N),
    my_partition(Tail,N,Littles,Bigs, Delta).
my_partition([Head|Tail],N,Littles,[Head|Bigs],Delta) :-
    rank_reverse_compare_or_equal(Delta,Head,N),
    my_partition(Tail,N,Littles,Bigs,Delta).