:- use_module(library(clpfd)).

%humans: [human(id,[Machine1,Machine2]),...].
%machines: [machine(id,[TaskType1,TaskType2],operator),..] operator is 0 || 1
%tasks: [task(id,typeId,duration,StartTime,EndTime),...]
%operations: [operation(id,[task1,task3,task2]),...]


%optimizar(humans,machines,tasks,operations).



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