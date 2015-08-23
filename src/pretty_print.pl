:- module(pretty_print, [pretty_print/1]).
:- use_module(execution_time,[execution_time/2]).
:- use_module(speedup,[speedup/2]).

pretty_print(solution(X)) :-
    writeln('Parallel Task Schedule'),
    length(X,L),
    execution_time(solution(X),ExecTime),
    speedup(solution(X),Speedup),
    format('~w ~d~n',['#cores:',L]),
    format('~w ~d~n',['Execution time:',ExecTime]),
    format('~w ~f~n~n',['Speedup:',Speedup]),
    print_schedules(X).

print_schedules([]) :- nl,!.
print_schedules([schedule(Core,Tasks)|Schedules]) :-
    format('~w ~w ~n~w',['Core:',Core, '[']),
    print_tasks(Tasks,1),
    writeln(']'),
    nl,
    print_schedules(Schedules).

print_tasks([],_) :- !.
print_tasks([Task],_) :-
    write(Task),
    !.
print_tasks([Task|Tasks], 10) :-
    !,
    write(Task),
    nl,
    print_tasks(Tasks,1).
print_tasks([Task|Tasks], ColumnNumber) :-
    format('~a~w ',[Task,',']),
    NextColumnNumber is ColumnNumber+1,
    print_tasks(Tasks,NextColumnNumber).
