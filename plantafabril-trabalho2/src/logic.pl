:- use_module(library(clpfd)).

%machines: [machine(id,TaskType1),..] needsHuman is 0 || 1
%tasks: [task(id,typeId,duration,MachineRef),...] HumanRef is 0 if no Human was needed
%operations: [[task1,task3,task2],[task4,task5,task6]),...]


machines([machine(1,type1),machine(2,type2)]).
tasks([task(1,type1,10,_),task(2,type1,5,_),task(3,type2,4),task(4,type1,2),task(5,type2,3)]).
operations([[1,3,5],[2,4]]).
startTimes(_,_,_,_,_).


checkOp(_,_,[]).
checkOp(S,E,[[_]|ROps]):- checkOp(S,E,ROps).
checkOp(S,E,[[_,_]|ROps]) :- checkOp(S, E, ROps).
checkOp(S, E, [[Id1,Task1, Task2|R]|ROps]):-
        element(ST2,S,Task2),
        element(ET1,E,Task1),
        ET1 #<= ST2,
        checkOp(S, E, [[Id1|R]|ROps]).




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
