:- use_module(library(clpfd)).
:- use_module(library(lists)).

%---------------------------------------------FACTS------------------------------------
%machines: [machine(id,TaskType1)]
machines([machine(1,type1),machine(2,type2)]).

%tasks: [task(id,TypeId,Duration,MachineRef),...]
tasks([task(1,type1,10,_),task(2,type1,5,_),task(3,type2,4,_),task(4,type1,2,_),task(5,type2,3,_)]).

%operations: [[task1,task3,task2],[task4,task5,task6]),...]
operations([[1,3,5],[2,4]]).

%---------------------------------------------CODE------------------------------------

getMachinesByType([],_,MachinesOut,MachinesOut).

getMachinesByType([machine(Id,Type)|OtherMachines],Type,MachinesOut,Aux):-
        append(Aux,[Id],Aux2),
        getMachinesByType(OtherMachines,Type,MachinesOut,Aux2).

getMachinesByType([machine(_,_)|OtherMachines],Type,MachinesOut,Aux):-
        getMachinesByType(OtherMachines,Type,MachinesOut,Aux).
        
assignMachines([task(_,TaskType,_,MachineRef)|OtherTasks],Machines):-
        getMachinesByType(Machines,TaskType,SelectedMachines,[]),
        list_to_fdset(SelectedMachines,FD),
        MachineRef in_set FD,
        assignMachines(OtherTasks,Machines).
        
assignMachines([],_).

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

restrictMachines([], RM, _, _, _):- disjoint2(RM).
restrictMachines([task(Task1Id,_,Dur1,Mach1Id)|Others], RM, Machines, StartTimes, EndTimes):-
        element(Task1Id,StartTimes,ST1),
        restrictMachines(Others, [f(ST1,Dur1,Mach1Id,1)| RM], Machines, StartTimes, EndTimes).

getMachinesUsed([task(_,_,_,Machine)|T],Machines,Aux):-
        append(Aux,[Machine],Aux2),
        getMachinesUsed(T,Machines,Aux2).

getMachinesUsed([],Machines,Machines).

start(ST) :- tasks(Tasks),operations(Operations),machines(Machines), plantaFabril(Machines,Tasks,Operations,ST).

plantaFabril(Machines,Tasks,Operations,StartTimes):-
        length(EndTimes,NumTasks),
        length(Tasks,NumTasks),
        length(StartTimes,NumTasks),
        sumDurations(Tasks,Sum,0),
        restrictStartTimes(StartTimes,Sum),
        restrictEndTimes(Tasks,StartTimes,EndTimes),
        restrictOperations(Tasks,StartTimes,Operations),
        assignMachines(Tasks,Machines),
        restrictMachines(Tasks, [],Machines,StartTimes,EndTimes),
        maximum(End,EndTimes),
        getMachinesUsed(Tasks,MachinesOut,[]),
        append(StartTimes,MachinesOut,Vars),
        labeling(minimize(End),Vars),
        printSolution(Tasks,StartTimes,1,End).
        
printSolution(_,[], _,End):- write('End time is: '), write(End), nl.
printSolution(Tasks,[H|T], I,End) :-
        getTask(Tasks,I,task(I,_,Dur,Machine)),
        EndTask #= H + Dur,
        write('Task '), write(I), write(' starts at '),
        write(H), write(' ends at '), write(EndTask), write('; Done on machine - '), write(Machine),
        nl, Y #= I+1, printSolution(Tasks,T, Y,End). 
