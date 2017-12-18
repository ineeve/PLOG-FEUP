:- use_module(library(clpfd)).

%---------------------------------------------FACTS------------------------------------
%machines: [machine(id,TaskType1)]
machines([machine(1,type1),machine(2,type2)]).

%tasks: [task(id,TypeId,Duration,MachineRef),...]
tasks([task(1,type1,10,_),task(2,type1,5,_),task(3,type2,4,_),task(4,type1,2,_),task(5,type2,3,_)]).

%operations: [[task1,task3,task2],[task4,task5,task6]),...]
operations([[1,3,5],[2,4]]).

%---------------------------------------------CODE------------------------------------

%getTaskDuration(+Tasks,+Id,-Task)
getTask([task(Id,Type,Duration,Machine)|_],Id,task(Id,Type,Duration,Machine)).
getTask([_|TT],Id,Task):-
        getTask(TT,Id,Task).

restrictOperations(_,_,[]).
restrictOperations(Tasks,S,[[_]|ROps]):- restrictOperations(Tasks,S,ROps).
restrictOperations(Tasks,S, [[Task1Id, Task2Id|R]|ROps]):-
        getTask(Tasks,Task1Id,task(Task1Id,_,Dur1,_)),
        element(Task1Id,S,ST1),
        element(Task2Id,S,ST2),
        ST1+Dur1 #=< ST2,
        restrictOperations(Tasks,S, [[Task2Id|R]|ROps]).

sumDurations([task(_,_,Dur1,_)|Others],Sum,Accumulator):-
        Accumulator2 is Accumulator + Dur1,
        sumDurations(Others,Sum,Accumulator2).
sumDurations([],Sum,Sum).

restrictStartTimes(StartTimes,Sum):-
        domain(StartTimes,0,Sum).

restrictEndTimes([task(Id,_,Dur,_)|Others],StartTimes,EndTimes):-
        element(Id,StartTimes,TaskST),
        element(Id,EndTimes,TaskET),
        TaskET #= TaskST + Dur,
        restrictEndTimes(Others,StartTimes,EndTimes).
restrictEndTimes([],_,_).

restrictMachines([task(Task1Id,_,Dur1,Mach1Id),task(Task2Id,Task2Type,Dur2,Mach2Id)|Others],Machines,StartTimes,EndTimes):-
        element(Task1Id,StartTimes,ST1),
        element(Task2Id,StartTimes,ST2),
        disjoint2([f(ST1,Dur1,Mach1Id,0),f(ST2,Dur2,Mach2Id,0)]),
        restrictMachines([task(Task2Id,Task2Type,Dur2,Mach2Id)|Others],Machines,StartTimes,EndTimes).
restrictMachines([_],_,_,_).

start(ST) :- tasks(Tasks),operations(Operations),machines(Machines), plantaFabril(Machines,Tasks,Operations,ST).

plantaFabril(Machines,Tasks,Operations,StartTimes):-
        length(EndTimes,NumTasks),
        length(Tasks,NumTasks),
        length(StartTimes,NumTasks),
        sumDurations(Tasks,Sum,0),
        restrictStartTimes(StartTimes,Sum),
        restrictEndTimes(Tasks,StartTimes,EndTimes),
        restrictOperations(Tasks,StartTimes,Operations),
        restrictMachines(Tasks,Machines,StartTimes,EndTimes),
        maximum(End,EndTimes),
        labeling(minimize(End),StartTimes).
        
        






/*
scheduleOP(Ss, End) :-
        Ss = [S1,S2,S3,S4,S5,S6,S7],
        Es = [E1,E2,E3,E4,E5,E6,E7],
        Tasks = [
                task(S1, 16, E1, 2, 1),
                task(S2, 6, E2, 9, 2),
                task(S3, 13, E3, 3, 1),
                task(S4, 7, E4, 7, 2),
                task(S5, 5, E5, 10, 1),
                task(S6, 18, E6, 1, 1,2),
                task(S7, 4, E7, 11, 1)
        ],
        Machines = [machine(1,12), machine(2,10)],
        domain(Ss, 1, 100),
        maximum(End, Es),
        cumulatives(Tasks, Machines, [bound(upper)]),
        labeling([minimize(End)], Ss).
*/
