:- module(execution_time, [execution_time/2,
                           task_term_time/3,
                           memo_task_term_time/2]).

%% execution_time(+S,-ET)
%% Calculates the execution time of a solution
%% The execution time of a solution is the highest termination time of a task
:- dynamic last_term_time/1.

execution_time(solution(Schedules), _) :-
    assert(last_term_time(-1)),
    % Take the last task from a core
    member(schedule(_,Tasks),Schedules),
    last(Tasks,Task),
    % Get its termination time in the current solution
    task_term_time(Task,Schedules,Stop),
    % Update new found termination time if it's later
    update_last_term_time(Stop),
    % Check the other cores
    fail.
execution_time(_,Result) :-
    % When all cores have been visited, report the result
    last_term_time(Result),
    retract(last_term_time(_)),
    % Clear the cache (see task_term_time/3)
    retractall(memo_task_term_time(_,_)).

update_last_term_time(TimeValue) :- 
    last_term_time(LastTime),
    TimeValue > LastTime,
    !,
    retract(last_term_time(_)),
    assert(last_term_time(TimeValue)).
update_last_term_time(_).

%% task_term_time(+Task, +Schedules, -Time)
%% Calculates the termination time of a task in a candidate solution
%% In order to be feasable for large instances, this predicate uses memoize (caching)
:- dynamic memo_task_term_time/2.

task_term_time([],_,0) :- !.
task_term_time(Task,_,TerminationTime) :- memo_task_term_time(Task,TerminationTime),!.
task_term_time(Task,Schedules,TerminationTime) :-
    % Tasks have either dependencies or not
    dependencies(Task,Dependencies),
    Dependencies \= [],
    !,
    % Calculate termination time of all dependencies
    member(schedule(Core,Tasks),Schedules),
    member(Task,Tasks),
    task_term_time_list(Dependencies,Schedules,DependencyTermTimes),
    % Get communication costs
    communication_costs(Task,Core,Dependencies,Schedules,DependencyCommunicationCosts),
    % Sum together
    maplist(plus,DependencyTermTimes,DependencyCommunicationCosts,DependenciesDone),
    % Take max
    max_list(DependenciesDone,EarliestStartTime),
    % Calculate the possible start time according to core's queue = query the termination time of the task before me
    predecessor(Task,Tasks,PreviousTask),
    task_term_time(PreviousTask,Schedules,CoreReady),
    % Choose the highest of the two
    StartTime is max(EarliestStartTime,CoreReady),
    % Fetch the proccess time
    process_cost(Task,Core,ProcessTime),
    TerminationTime is StartTime+ProcessTime,
    % Cache the result
    assert(memo_task_term_time(Task,TerminationTime)).
task_term_time(Task,Schedules,TerminationTime) :-
    % When a task has no dependencies 
    member(schedule(Core,Tasks),Schedules),
    member(Task,Tasks),
    % Calculate the possible start time according to core's queue = query the termination time of the task before me
    predecessor(Task,Tasks,PreviousTask),
    task_term_time(PreviousTask,Schedules,StartTime),
    % Fetch the process time
    process_cost(Task,Core,ProcessTime),
    TerminationTime is StartTime+ProcessTime,
    % Cache the result
    assert(memo_task_term_time(Task,TerminationTime)),
    !. % A task has only exact one termination time in an execution schedule

communication_costs(_,_,[],_,[]) :- !.
communication_costs(Task,Core,[Dependency|Dependencies],Schedules,[Cost|Costs]) :-
    communication_cost(Task,Core,Dependency,Schedules,Cost),
    communication_costs(Task,Core,Dependencies,Schedules,Costs).

communication_cost(Task,Core1,Dependency,Schedules,Result) :-
    member(schedule(Core2,Tasks2),Schedules),
    member(Dependency,Tasks2),
    Core1 \= Core2,
    !,
    depends_on(Task,Dependency,Data),
    channel(Core1,Core2, Lat, BandWith),
    Result is Lat + (Data/BandWith).
communication_cost(_,_,_,_,0) :- !.

task_term_time_list([],_,[]) :- !.
task_term_time_list([Task|Tasks], Schedules, [Time|Times]) :-
    task_term_time(Task, Schedules, Time),
    task_term_time_list(Tasks,Schedules,Times).

dependencies(Task,Result) :-
    findall(X,depends_on(Task,X,_),Result).

predecessor(Element,List,Result) :-
    predecessor_acc(Element,List,[],Result),
    !.

predecessor_acc(Element,[Element|_],[],[]) :- !.
predecessor_acc(Element,[Element|_],[H|_],H) :- !.
predecessor_acc(Element,[H|T],PredsSoFar,Result) :-
    predecessor_acc(Element,T,[H|PredsSoFar],Result).