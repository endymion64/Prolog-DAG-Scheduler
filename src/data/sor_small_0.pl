%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              sor_small.pl              %
%                                        %
%        Scheduling of a parallel        %
% successive over relaxation computation %
%      on a heterogeneous system         %
%   (non-uniform communication costs)    %
%                                        %
%       Declarative Programming          %
%              2014-2015                 %
%                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%%%   Cores  %%%
%%%%%%%%%%%%%%%%
% The cores of the machine
% core(Id): a core with unique identifier 'Id'.
core(c1).
core(c2).
core(c3).
core(c4).

%%%%%%%%%%%%%%%%%%
%%%   Tasks    %%%
%%%%%%%%%%%%%%%%%%
% The tasks the application is made up of, which are to be scheduled.
% task(Id): a task with unique identifier 'Id'.
task(t1).
task(t2).
task(t3).
task(t4).
task(t5).
task(t6).

%%%%%%%%%%%%%%%%%%
%% Dependencies %%
%%%%%%%%%%%%%%%%%%
% The execution order dependencies between tasks
% depends_on(Ta,Tb,Data): before task 'Ta' can be executed, 
% task 'Tb' must have been executed and thereafter 'Data' megabytes of data (result of/produced by 'Tb') must have been moved from the processor that executed 'Tb' to the processor that will execute 'Ta'.

% %In this benchmark tasks are interdependent, and data (matrix-chunk) is communicated between tasks.
depends_on(t2,t1,25).
depends_on(t3,t1,25).
depends_on(t4,t1,25).
depends_on(t5,t1,25).
depends_on(t6,t2,25).
depends_on(t6,t3,25).
depends_on(t6,t4,25).
depends_on(t6,t5,25).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Processing Costs   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies how long the processing of each task takes on each core.
% process_cost(T,C,Time): It takes 'Time' ms to execute task 'T' on core 'C'.

%In this benchmark c1 and c2 are 20% more efficient than c3, c4
process_cost(t1,c1,80).
process_cost(t1,c2,80).
process_cost(t1,c3,100).
process_cost(t1,c4,100).
process_cost(t2,c1,40).
process_cost(t2,c2,40).
process_cost(t2,c3,50).
process_cost(t2,c4,50).
process_cost(t3,c1,40).
process_cost(t3,c2,40).
process_cost(t3,c3,50).
process_cost(t3,c4,50).
process_cost(t4,c1,40).
process_cost(t4,c2,40).
process_cost(t4,c3,50).
process_cost(t4,c4,50).
process_cost(t5,c1,40).
process_cost(t5,c2,40).
process_cost(t5,c3,50).
process_cost(t5,c4,50).
process_cost(t6,c1,8).
process_cost(t6,c2,8).
process_cost(t6,c3,10).
process_cost(t6,c4,10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Channel Properties  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies the properties of the communication channel between each of the cores
% channel(Ca,Cb,Latency,Bandwidth): The channel to communicate from core 'Ca' to core 'Cb' has a latency 'Latency' and bandwidth 'Bandwidth'.
% Note that sending 'X' megabytes of data, over a channel, takes Latency + X/Bandwidth ms.

%In this benchmark c1, c3 and c2, c4 are on the same chip, allowing for faster inter-core communication between these cores.
channel(c1,c3,1,5) :- !.
channel(c1,C,5,1) :- C \= c3, core(C),!.
channel(c2,c4,1,5) :- !.
channel(c2,C,5,1) :- C \= c4, core(C),!.
channel(c3,c1,1,5) :- !.
channel(c3,C,5,1) :- C \= c1, core(C),!.
channel(c4,c2,1,5) :- !.
channel(c4,C,5,1) :- C \= c2, core(C),!.