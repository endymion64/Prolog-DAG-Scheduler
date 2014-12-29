%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          batch_small_homo.pl          %
%                                       %
%    Scheduling of a small batch of     %
%   independent tasks on a homogeoneous %
%                system                 %
%                                       %
%       Declarative Programming         %
%              2014-2015                %
%                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
task(t7).

%%%%%%%%%%%%%%%%%%
%% Dependencies %%
%%%%%%%%%%%%%%%%%%
% The execution order dependencies between tasks
% depends_on(Ta,Tb,Data): before task 'Ta' can be executed, 
% task 'Tb' must have been executed and thereafter 'Data' megabytes of data (result of/produced by 'Tb') must have been moved from the processor that executed 'Tb' to the processor that will execute 'Ta'.

%In this benchmark there are no dependencies between tasks.
depends_on(_,_,_) :- fail. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Processing Costs   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies how long the processing of each task takes on each core.
% process_cost(T,C,Time): It takes 'Time' ms to execute task 'T' on core 'C'.

%In this benchmark every core can execute a given task as fast (i.e. homogeneous system)
process_cost(t1,C,100) :- core(C).
process_cost(t2,C,20) :- core(C).
process_cost(t3,C,30) :- core(C).
process_cost(t4,C,40) :- core(C).
process_cost(t5,C,60) :- core(C).
process_cost(t6,C,70) :- core(C).
process_cost(t7,C,80) :- core(C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Channel Properties  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies the properties of the communication channel between each of the cores
% channel(Ca,Cb,Latency,Bandwidth): The channel to communicate from core 'Ca' to core 'Cb' has a latency 'Latency' and bandwidth 'Bandwidth'.
% Note that sending 'X' megabytes of data, over a channel, takes Latency + X/Bandwidth ms.

%In this benchmark (without task dependencies), no inter core communication is required, the channel properties therefore do not matter.
channel(C1,C2,0,1) :- core(C1), core(C2).