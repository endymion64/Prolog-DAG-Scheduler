%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Declarative Programming Project:  %
%       Task-Parallel Scheduler      %  
%                                    %
%          Youri Coppens             %
%            nr 100868               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Following the solutions of the last exercise session

% isSolution(+solution(S))
isSolution(solution(S)) :-
    findall(CoreID,core(CoreID),CoreList),
    findall(TaskID,task(TaskID),TaskList),
    isSolution(S,CoreList,TaskList).

% isSolution(+Schedules,+Cores,+Tasks)
% Tests whether a list of schedules is correct.
% A correct solution has for each core a subset of tasks
% Each task is assigned to only one core
% This predicate cannot generate solutions
isSolution([],[],[]).
isSolution([Schedule|Schedules],Cores,Tasks) :-
    isSchedule(Schedule),
    scheduleInfo(Schedule,Core,CoreTasks),
    subset(CoreTasks,Tasks), 
    delete(Cores,Core,CoresLeft),
    subtract(Tasks,CoreTasks,TasksLeft),
    isSolution(Schedules,CoresLeft,TasksLeft).

isSchedule(schedule(Core,[])) :-
    core(Core).
isSchedule(schedule(Core, [Task|Tasks])) :-
    task(Task),
    is_set([Task|Tasks]),
    isSchedule(schedule(Core,Tasks)).

scheduleInfo(schedule(Core,Tasks),Core,Tasks).


/*
solution(S) :-
    findall(CoreID,core(CoreID), CoreList),
    findall(TaskID,task(TaskID), TaskList),
    solution(S,CoreList,TaskList).
*/
/*
assignTask(schedule(Core,Tasks),Task,schedule(Core,[Task|Tasks]) :-
    isSchedule(schedule(Core,Tasks)),
    task(Task).    

isSolution(solution([schedule(c1,[t2,t5,t6]),schedule(c2,[t3,t1,t4]),schedule(c3,[t7]),schedule(c4,[])])).
find_optimal(S):-!.
find_heuristically(S):-!.
pretty_print(S):-!.
*/