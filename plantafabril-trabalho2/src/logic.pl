:- use_module(library(clpfd)).

%humans: [human(id,[Machine1,Machine2]),...].
%machines: [machine(id,TaskType1,needsHuman),..] needsHuman is 0 || 1
%tasks: [task(id,typeId,duration,MachineRef,HumanRef),...] HumanRef is 0 if no Human was needed
%operations: [operation(id,[task1,task3,task2]),...]

%task(Si,Di,Ei,Ri,Mi)
%machine(id,1).
%human(id,[Machine1,Machine2]).


machines([machine(1,type1),machine(2,type2)]).
tasks([task(1,type1,10,_),task(2,type1,5,_),task(3,type2,4),task(4,type1,2),task(5,type2,3)]).
operations([[1,3,5],[2,4]]).
startTimes(_,_,_,_,_).