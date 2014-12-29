%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            fib_small_nc.pl            %
%                                       %
%   Scheduling of a parallel fibonacci  %
% computation on a homogeoneous system  %
%    (ignoring communication costs)     %
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
task(t7).
task(t1).
task(t6).
task(t2).
task(t4).
task(t3).
task(t5).

%%%%%%%%%%%%%%%%%%
%% Dependencies %%
%%%%%%%%%%%%%%%%%%
% The execution order dependencies between tasks
% depends_on(Ta,Tb,Data): before task 'Ta' can be executed, 
% task 'Tb' must have been executed and thereafter 'Data' megabytes of data (result of/produced by 'Tb') must have been moved from the processor that executed 'Tb' to the processor that will execute 'Ta'.

% %In this benchmark tasks are interdependent, but no data is communicated between tasks.

depends_on(t7,t2,0).
depends_on(t7,t6,0).
depends_on(t6,t4,0).
depends_on(t6,t5,0).
depends_on(t2,t1,0).
depends_on(t4,t3,0).
depends_on(t3,t1,0).
depends_on(t5,t3,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Processing Costs   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies how long the processing of each task takes on each core.
% process_cost(T,C,Time): It takes 'Time' ms to execute task 'T' on core 'C'.

%In this benchmark any task takes as long on any core (i.e. homogeneous system)

process_cost(T,C,10) :- task(T), core(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Channel Properties  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies the properties of the communication channel between each of the cores
% channel(Ca,Cb,Latency,Bandwidth): The channel to communicate from core 'Ca' to core 'Cb' has a latency 'Latency' and bandwidth 'Bandwidth'.
% Note that sending 'X' megabytes of data, over a channel, takes Latency + X/Bandwidth ms.

%In this benchmark inter-core communication is assumed to take no time (channel latency is 0)
channel(C1,C2,0,1) :- core(C1), core(C2).