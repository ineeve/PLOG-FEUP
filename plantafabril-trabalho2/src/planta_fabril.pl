:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(timeout)).

%---------------------------------------------FACTS------------------------------------
%machines: [machine(id,TaskType1,ListOfHumansThatCanOperate),...]
machines1([machine(1,type1,[1]),machine(2,type2,[0])]).
machines2([machine(1,type1,[1,4]),machine(2,type2,[0]),machine(3,type3,[1,2,3])]).
machines3([machine(1,type1,[1]),machine(2,type2,[0]),machine(3,type3,[2,3]), machine(4,type4,[1,2,4])]).
machines4([machine(1,type1,[0]),machine(2,type2,[1,3]),machine(3,type3,[2,4]),machine(4,type4,[2,3]),machine(5,type1,[0]),machine(6,type3,[5])]).

%tasks: [task(id,TypeId,Duration,MachineRef,HumanRef),...]
tasks1([task(1,type1,5,_,_),task(2,type2,3,_,_),task(3,type1,7,_,_)]).
tasks2([task(1,type1,10,_,_),task(2,type3,5,_,_),task(3,type2,4,_,_),task(4,type1,2,_,_),task(5,type2,3,_,_)]).
tasks3([task(1,type1,3,_,_),task(2,type2,1,_,_),task(3,type3,5,_,_),task(4,type4,8,_,_),task(5,type2,4,_,_),task(6,type3,8,_,_),
       task(7,type1,3,_,_),task(8,type4,7,_,_),task(9,type3,3,_,_),task(10,type1,2,_,_)]).
tasks4([task(1,type3,2,_,_),task(2,type1,5,_,_),task(3,type2,7,_,_),task(4,type2,4,_,_),task(5,type3,3,_,_),task(6,type4,10,_,_),
       task(7,type4,13,_,_),task(8,type1,6,_,_),task(9,type2,9,_,_),task(10,type3,8,_,_),task(11,type3,3,_,_),task(12,type2,2,_,_),
       task(13,type4,5,_,_),task(14,type3,8,_,_),task(15,type3,2,_,_)]).

%operations: [[task1,task3,task2],[task4,task5,task6]),...]
operations1([[1],[2],[3]]).
operations2([[1,3,5],[2,4]]).
operations3([[1,2],[3,4,5],[9,7,6],[8,10]]).
operations4([[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15]]).
operations5([[1,2,3],[4],[5],[6],[7],[8],[9],[10],[11],[12],[13],[14],[15]]).


%---------------------------------------------CODE------------------------------------

getMachinesByType([],_,MachinesOut,MachinesOut).

getMachinesByType([machine(Id,Type,_)|OtherMachines],Type,MachinesOut,Aux):-
        append(Aux,[Id],Aux2),
        getMachinesByType(OtherMachines,Type,MachinesOut,Aux2).

getMachinesByType([machine(_,_,_)|OtherMachines],Type,MachinesOut,Aux):-
        getMachinesByType(OtherMachines,Type,MachinesOut,Aux).
        
assignMachines([task(_,TaskType,_,MachineRef,_)|OtherTasks],Machines):-
        getMachinesByType(Machines,TaskType,SelectedMachines,[]),
        list_to_fdset(SelectedMachines,FD),
        MachineRef in_set FD,
        assignMachines(OtherTasks,Machines).
        
assignMachines([],_).

getMachineById([machine(ID,Type,Humans)|_],ID,machine(ID,Type,Humans)).
getMachineById([machine(_,_,_)|OtherMachines],ID,Machine):-
        getMachineById(OtherMachines,ID,Machine).

%getListOfHumansForMachine(+Machine,-HumansList)
getListOfHumansForMachine(machine(_,_,HumansList),HumansList).

assignHumans([task(_,_,_,MachineRef,HumanRef)|OtherTasks],AllMachines):-
        getMachineById(AllMachines,MachineRef,TheMachine),
        getListOfHumansForMachine(TheMachine,HumansForMachine),
        list_to_fdset(HumansForMachine,FD),
        HumanRef in_set FD,
        assignHumans(OtherTasks,AllMachines).
assignHumans([],_).

%getTask(+Tasks,+Id,-Task)
getTask([task(Id,Type,Duration,Machine,Human)|_],Id,task(Id,Type,Duration,Machine,Human)).
getTask([_|TT],Id,Task):-
        getTask(TT,Id,Task).

restrictOperations(Tasks,S, [[Task1Id, Task2Id|R]|ROps]):-
        getTask(Tasks,Task1Id,task(Task1Id,_,Dur1,_,_)),
        element(Task1Id,S,ST1),
        element(Task2Id,S,ST2),
        ST1+Dur1 #=< ST2,
        restrictOperations(Tasks,S, [[Task2Id|R]|ROps]).
restrictOperations(Tasks,S,[[_]|ROps]):- restrictOperations(Tasks,S,ROps).
restrictOperations(_,_,[[]]).
restrictOperations(_,_,[]).


sumDurations([task(_,_,Dur1,_,_)|Others],Sum,Accumulator):-
        Accumulator2 is Accumulator + Dur1,
        sumDurations(Others,Sum,Accumulator2).
sumDurations([],Sum,Sum).

restrictStartTimes(StartTimes,Sum):-
        domain(StartTimes,0,Sum).

restrictEndTimes([task(Id,_,Dur,_,_)|Others],StartTimes,EndTimes):-
        element(Id,StartTimes,TaskST),
        element(Id,EndTimes,TaskET),
        TaskET #= TaskST + Dur,
        restrictEndTimes(Others,StartTimes,EndTimes).
restrictEndTimes([],_,_).

restrictMachines([], RM, _, _):- disjoint2(RM).
restrictMachines([task(Task1Id,_,Dur1,Mach1Id,_)|Others], RM, Machines, StartTimes):-
        element(Task1Id,StartTimes,ST1),
        restrictMachines(Others, [f(ST1,Dur1,Mach1Id,1)| RM], Machines, StartTimes).

restrictHumans([],RH,_,_):- disjoint2(RH).
restrictHumans([task(_,_,_,_,0)|Others],RH,Machines,StartTimes):-
        restrictHumans(Others,RH,Machines,StartTimes).
restrictHumans([task(Task1Id,_,Dur1,_,Human1Id)|Others],RH,Machines,StartTimes):-
        element(Task1Id,StartTimes,ST1),
        restrictHumans(Others,[f(ST1,Dur1,Human1Id,1)|RH],Machines,StartTimes).

getMachinesAndHumansVars([task(_,_,_,Machine,Human)|T],MachinesAndHumans,Aux):-
        append(Aux,[Machine,Human],Aux2),
        getMachinesAndHumansVars(T,MachinesAndHumans,Aux2).

getMachinesAndHumansVars([],Machines,Machines).

start(Machines,Tasks,Operations,Timeout,Flag):-
        plantaFabril(Machines,Tasks,Operations,ST,End,Timeout,Flag),
        printSolution(Tasks,ST,1,End).
        

startEx1(ST,Flag) :- 
        machines1(M),
        tasks1(T),
        operations1(O),
        plantaFabril(M, T, O, ST,End,16000,Flag),
        printSolution(T,ST,1,End).

startEx2(ST,Flag) :- 
        machines2(M),
        tasks2(T),
        operations2(O),
        plantaFabril(M, T, O, ST,End,16000,Flag),
        printSolution(T,ST,1,End).

startEx3(ST,Flag) :- 
        machines3(M),
        tasks3(T),
        operations3(O),
        plantaFabril(M, T, O, ST,End,16000,Flag),
        printSolution(T,ST,1,End).

startEx4(ST,Flag) :- 
        machines4(M),
        tasks4(T),
        operations4(O),
        plantaFabril(M, T, O, ST,End,16000,Flag),
        printSolution(T,ST,1,End).


startEx5(ST,Flag) :- 
        machines4(M), %it uses machines4 and tasks4 on purpose
        tasks4(T),
        operations5(O),
        plantaFabril(M, T, O, ST,End,1000,Flag),
        printSolution(T,ST,1,End).
        

plantaFabril(Machines,Tasks,Operations,StartTimes,End,Timeout,Flag):-
        length(EndTimes,NumTasks),
        length(Tasks,NumTasks),
        length(StartTimes,NumTasks),
        sumDurations(Tasks,Sum,0),
        restrictStartTimes(StartTimes,Sum),
        restrictEndTimes(Tasks,StartTimes,EndTimes),
        restrictOperations(Tasks,StartTimes,Operations),
        assignMachines(Tasks,Machines),
        restrictMachines(Tasks, [],Machines,StartTimes),
        assignHumans(Tasks,Machines),
        restrictHumans(Tasks,[], Machines,StartTimes),
        maximum(End,EndTimes),
        getMachinesAndHumansVars(Tasks,MachinesAndHumans,[]),
        append(StartTimes,MachinesAndHumans,Vars),
        labeling([minimize(End),time_out(Timeout,Flag)],Vars).
        %fd_statistics.

printSolution(_,[], _,End):- write('End time is: '), write(End), nl.
printSolution(Tasks,[H|T], I,End) :-
        getTask(Tasks,I,task(I,_,Dur,Machine,HumanRef)),
        EndTask is H + Dur,
        write('Task '), write(I), write(' starts at '),
        write(H), write(' ends at '), write(EndTask), 
        write('; Done on machine - '), write(Machine), 
        write('; human id - '), write(HumanRef),
        nl, Y is I+1, printSolution(Tasks,T, Y,End). 
