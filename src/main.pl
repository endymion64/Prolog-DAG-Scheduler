%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    %
%  Declarative Programming Project:  %
%       Task-Parallel Scheduler      %
%                                    %
%           Youri Coppens            %
%                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% HOW TO RUN %%%

/*****************

1) Open terminal/command line in the project source folder 'src'
2) Run the following command: swipl ../data/<instance-file> main.pl
3) Enjoy querying!

*****************/

:- module(main,[]).
:- reexport([ speedup,
              find_optimal,
              find_heuristically,
              pretty_print
            ]).
:- reexport(solution, except([solution/1])).
:- reexport(execution_time, except([ task_term_time/3, 
                                     memo_task_term_time/2
                                   ])).