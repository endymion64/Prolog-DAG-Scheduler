:- module(solution, [isSolution/1,
                     solution/1]).

%% isSolution(+solution(Schedules))
%% Tests whether a list of schedules forms a valid solution.
%% A correct solution has for each core a schedule/2 with a subset of tasks
%% Each task is assigned to only one core
%% Tasks are topologically ordered per schedule based on the dependencies 
%% This predicate cannot generate solutions
isSolution(solution(Schedules)) :-
    findall(CoreID,core(CoreID),CoreList),
    findall(TaskID,task(TaskID),TaskList),
    isSolution(Schedules,CoreList,TaskList).

%% isSolution(+Schedules,+Cores,+Tasks)
%% Internally used by isSolution/1
isSolution([],[],[]) :- !.
isSolution([schedule(Core,CoreTasks)|Schedules],Cores,Tasks) :-
    core(Core),
    is_set(CoreTasks), % no duplicate tasks
    subset(CoreTasks,Tasks),
    valid_task_order(CoreTasks), % topologically ordered
    delete(Cores,Core,CoresLeft),
    subtract(Tasks,CoreTasks,TasksLeft),
    isSolution(Schedules,CoresLeft,TasksLeft).

%% valid_task_order(+Tasks)
%% Tests whether a sequence of tasks is topologically ordered
%% The sequence is topologically ordered if no successive task is an ancestor of a previous task in the DAG
valid_task_order([]).
valid_task_order([TaskH|Tasks]) :-
    task(TaskH),
    valid_task_order(Tasks),
    not(depending_on(TaskH,Tasks)).

%% depending_on(+Task, +Tasks)
%% Tests whether Task has an ancestor in the list Tasks.
depending_on(_,[]) :- fail.
depending_on(Task1, [Task2|_]) :-
    task(Task1),
    task(Task2),
    search_df_loop([Task1],[],Task2).
depending_on(Task1,[_|Tail]) :-
    depending_on(Task1,Tail).

%% Depth First Search with loop detection through the DAG to search for ancestors
%% Adopted from the course slides
search_df_loop([Goal|_],_,Goal) :- !.
search_df_loop([Current|Rest],Visited,Goal) :-
    children(Current,Children),
    add_df(Children,Rest,Visited,NewAgenda),
    search_df_loop(NewAgenda,[Current|Visited],Goal).

add_df([],Agenda,_,Agenda).
add_df([Child|Rest],OldAgenda,Visited,[Child|NewAgenda]) :-
    not(member(Child,OldAgenda)),
    not(member(Child,Visited)),
    add_df(Rest,OldAgenda,Visited,NewAgenda).
add_df([Child|Rest],OldAgenda,Visited,NewAgenda) :-
    member(Child,OldAgenda),
    add_df(Rest,OldAgenda,Visited,NewAgenda).
add_df([Child|Rest],OldAgenda,Visited,NewAgenda) :-
    member(Child,Visited),
    add_df(Rest,OldAgenda,Visited,NewAgenda).

children(Node,Children) :-
    findall(C,depends_on(Node,C,_),Children).

%% solution(-X)
%% Generates a valid candidate solution
%% This is a list of schedule/2 functors
%% This predicate is used to find the optimal solution
solution(X) :-
    findall(CoreID,core(CoreID),CoreList),
    findall(TaskID,task(TaskID),TaskList),
    solution(X,CoreList,TaskList).

solution([],[],[]) :- !.
solution([schedule(Core,ScheduledTasks)|Schedules],[Core|Cores],Unscheduled) :-
    core(Core),
    my_subset(Unscheduled,ScheduledTasks), 
    valid_task_order(ScheduledTasks),
    subtract(Unscheduled,ScheduledTasks,TasksLeft),
    solution(Schedules,Cores,TasksLeft).

%% my_subset(+List,-Subset)
%% Generates a subset of List (including [] and List itself)
%% This predicate doesn't check ordering and thereby also reproduces permutations of subsets
%% E.g. if [1,2,3] is produced, [2,3,1] can also be produced
%% Adopted from exercise sessions
my_subset(_,[]).
my_subset(Set,[H|T]) :-
    member(H,Set),
    my_subtract(Set,H,NewSet),
    my_subset(NewSet,T).

my_subtract([],_,[]).
my_subtract([Element|Rest],Element,Result) :-
    !, 
    my_subtract(Rest,Element,Result).
my_subtract([H|Rest],Element,[H|Result]) :-
    my_subtract(Rest,Element,Result).