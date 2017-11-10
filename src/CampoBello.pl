/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:3; tab-width:8; -*- */

:- use_module(library(lists)).

boardMembers([mid,r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,y0,y1,y2,y3,y4,y5,y6,y7,y8,y9]).


validJump(mid,b1,b0).
validJump(mid,b2,b0).
validJump(b0,y2,y0).
validJump(b0,b3,b1).
validJump(b0,b5,b2).
validJump(b1,y0,b0).
validJump(b1,b6,b3).
validJump(b1,b8,b4).
validJump(b2,b7,b4).
validJump(b2,b9,b5).
validJump(b3,b5,b4).
validJump(b6,b8,b7).
validJump(b7,b9,b8).

validJump(mid,y1,y0).
validJump(mid,y2,y0).
validJump(y0,r2,r0).
validJump(y0,y3,y1).
validJump(y0,y5,y2).
validJump(y1,r0,y0).
validJump(y1,y6,y3).
validJump(y1,y8,y4).
validJump(y2,y7,y4).
validJump(y2,y9,y5).
validJump(y3,y5,y4).
validJump(y6,y8,y7).
validJump(y7,y9,y8).

validJump(mid,r1,r0).
validJump(mid,r2,r0).
validJump(r0,g2,g0).
validJump(r0,r3,r1).
validJump(r0,r5,r2).
validJump(r1,g0,r0).
validJump(r1,r6,r3).
validJump(r1,r8,r4).
validJump(r2,r7,r4).
validJump(r2,r9,r5).
validJump(r3,r5,r4).
validJump(r6,r8,r7).
validJump(r7,r9,r8).

validJump(mid,g1,g0).
validJump(mid,g2,g0).
validJump(g0,b2,b0).
validJump(g0,g3,g1).
validJump(g0,g5,g2).
validJump(g1,b0,g0).
validJump(g1,g6,g3).
validJump(g1,g8,g4).
validJump(g2,g7,g4).
validJump(g2,g9,g5).
validJump(g3,g5,g4).
validJump(g6,g8,g7).
validJump(g7,g9,g8).

validJump(b0, r0, mid).
validJump(y0, g0, mid).
validJump2(X,Y,Z):- validJump(X,Y,Z).
validJump2(X,Y,Z):- validJump(Y,X,Z).

/*
blueMoversInitialPos([r1,r2,r3,r4,r5,r6,r7,r8,r9,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowMoversInitialPos([y1,y2,y3,y4,y5,y6,y7,y8,y9,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

blueMoversMidGamePos([y1,r1,r5,r6,r7,r8,r9,b1,b5,b6,b7,mid]).
yellowMoversMidGamePos([r3,y2,y4,y5,y8,y9,g1,g4,g7,g8,g9]).

blueMoversFinalPos([]).
yellowMoversFinalPos([y2,y8,y9,g1,g9]).*/

blueMoversPos([r1,r2,r3,r4,r5,r6,r7,r8,r9,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowMoversPos([y1,y2,y3,y4,y5,y6,y7,y8,y9,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

blueArea([r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowArea([y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

initialBoard(Y,B) :- blueMoversPos(B), yellowMoversPos(Y).
                                                                     

bluePlayerPoints([],0).
bluePlayerPoints([H|T],P) :- bluePlayerPoints(T,P1), blueArea(B), member(H,B), P is P1 + 3.
bluePlayerPoints([H|T],P) :- bluePlayerPoints(T,P1), (yellowArea(Y), member(H,Y); H==mid), P is P1 + 1.

yellowPlayerPoints([],0).
yellowPlayerPoints([H|T],P) :- yellowPlayerPoints(T,P1), yellowArea(Y), member(H,Y), P is P1 + 3.
yellowPlayerPoints([H|T],P) :- yellowPlayerPoints(T,P1), (H==mid; blueArea(B), member(H,B)), P is P1 + 1.

points(b,BlueMoversPos,Points) :- bluePlayerPoints(BlueMoversPos,Points).
points(y,YellowMoversPos,Points) :- yellowPlayerPoints(YellowMoversPos,Points).

winner(BlueMovers,YellowMovers) :- points(b,BlueMovers,P1), points(y,YellowMovers,P2), whoWins(P1,P2).
whoWins(P1,P2) :- P1 > P2, write('Blue Player Wins').
whoWins(P1,P2) :- P1 < P2, write('Yellow Player Wins').
whoWins(P1,P2) :- P1 == P2, write('Draw').


displaySingleP(P, PiecesY, _) :- member(P,PiecesY), write(y), write('   '). 
displaySingleP(P, _, PiecesB) :- member(P,PiecesB), write(b), write('   ').
displaySingleP(_,_,_) :- write(e), write('   ').
displayPos([H|T],PiecesY,PiecesB) :- displaySingleP(H,PiecesY,PiecesB),
                                displayPos(T,PiecesY,PiecesB).
displayPos([],_,_).

displayLine(1,PiecesY,PiecesB) :- write('    g9--g8--g7--g6      '), 
                                put_code(186) ,
                                write('       '), displayPos([g9,g8,g7,g6],PiecesY,PiecesB), 
                                nl, 
                                write('      '), put_code(92), write(' / '), put_code(92), write(' / '), put_code(92), write(' /       '), 
                                put_code(186), 
                                write('        '), put_code(92), write('         /  '), 
                                nl.

displayLine(2,PiecesY,PiecesB) :- write('b6    g5--g4--g3    r9  '), 
                                put_code(186) ,
                                write('   '), displaySingleP(b6,PiecesY,PiecesB), write('  '),
                                displayPos([g5,g4,g3],PiecesY,PiecesB), write('  '),
                                displaySingleP(r9,PiecesY,PiecesB), 
                                nl, 
                                write('|'), put_code(92), write('      '), put_code(92), write(' / '), put_code(92), write(' /     /|  '), 
                                put_code(186), 
                                write('    '),put_code(92), write('     '), put_code(92), write('     /     /'), 
                                nl.

displayLine(3,PiecesY,PiecesB) :- write('| b3    g2--g1    r5 |  '), 
                                put_code(186) , 
                                write('     '), displaySingleP(b3,PiecesY,PiecesB), write('  '),
                                displayPos([g2,g1],PiecesY,PiecesB), write('  '),
                                displaySingleP(r5,PiecesY,PiecesB), 
                                nl, 
                                write('| /'), put_code(92), write('      '), put_code(92), write(' /     /'), put_code(92),write(' |  '), 
                                put_code(186), 
                                write('      '),put_code(92), write('     '), put_code(92), write(' /     /'),  
                                nl.

displayLine(4,PiecesY,PiecesB) :- write('b7  b1    g0    r2  r8  '), 
                                put_code(186) ,
                                write('   '), displayPos([b7,b1],PiecesY,PiecesB), write('  '),
                                displaySingleP(g0,PiecesY,PiecesB), write('  '),
                                displayPos([r2,r8],PiecesY,PiecesB), 
                                nl, 
                                write('|'), put_code(92), write('  /'),put_code(92),  write('  /  | '), put_code(92), write('  /'),put_code(92),
                                write('  /|  '), 
                                put_code(186), 
                                write('        '),put_code(92), write('  /   '), put_code(92), write('  /     '),
                                nl.

displayLine(5,PiecesY,PiecesB) :- write('| b4  b0--mid-r0  r4 |  '), 
                                put_code(186) ,
                                write('     '), displaySingleP(b4,PiecesY,PiecesB),
                                displayPos([b0,mid,r0],PiecesY,PiecesB),
                                displaySingleP(r4,PiecesY,PiecesB), 
                                nl,
                                write('|/  '),put_code(92), write('/  '),put_code(92),  write('  | /  '), put_code(92), write('/  '),
                                put_code(92),write('|  '), 
                                put_code(186), 
                                write('        /  '),put_code(92), write('   /  '), put_code(92), write('     '),
                                nl.

displayLine(6,PiecesY,PiecesB) :- write('b8  b2    y0    r1  r7  '), 
                                put_code(186),
                                write('   '), displayPos([b8,b2],PiecesY,PiecesB), write('  '),
                                displaySingleP(y0,PiecesY,PiecesB), write('  '),
                                displayPos([r1,r7],PiecesY,PiecesB), 
                                nl, 
                                write('| '), put_code(92), write('/      / '), put_code(92), write('     '), put_code(92),write('/ |  '), 
                                put_code(186), 
                                write('      /     / '),put_code(92), write('     '), put_code(92),    
                                nl.

displayLine(7,PiecesY,PiecesB) :- write('| b5    y1--y2    r3 |  '), 
                                put_code(186) ,
                                write('     '), displaySingleP(b5,PiecesY,PiecesB),
                                write('  '), displayPos([g1,g2],PiecesY,PiecesB),
                                write('  '), displaySingleP(r6,PiecesY,PiecesB), 
                                nl,
                                write('|'), write('/      / '), put_code(92), write(' / '), put_code(92), write('     '), 
                                put_code(92), write('|  '), 
                                put_code(186), 
                                write('    /     /     '),put_code(92), write('     '), put_code(92),  
                                nl.
                                
displayLine(8,PiecesY,PiecesB) :- write('b9    y3--y4--y5    r6  '), 
                                put_code(186) ,
                                write('   '), displaySingleP(b9,PiecesY,PiecesB), 
                                write('  '), displayPos([y3,y4,y5],PiecesY,PiecesB),
                                write('  '), displaySingleP(r6,PiecesY,PiecesB), 
                                nl,
                                write('     / '), put_code(92), write('  / '), put_code(92), write(' / '), put_code(92), write('       '), 
                                put_code(186), 
                                write('        /         '), put_code(92),  
                                nl.

displayLine(9,PiecesY,PiecesB) :- write('    y6--y7--y8--y9      '), 
                                put_code(186) ,
                                write('       '), displayPos([y6,y7,y8,y9],PiecesY,PiecesB), 
                                nl, 
                                nl.

displayBoard(Y,B) :- 
        nl,
        write('       Positions        '), put_code(186), write('        Game Board'), nl, 
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(206),put_code(205),put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),nl,
        displayLine(1,Y,B),
        displayLine(2,Y,B), 
        displayLine(3,Y,B), 
        displayLine(4,Y,B),
        displayLine(5,Y,B),
        displayLine(6,Y,B),
        displayLine(7,Y,B),
        displayLine(8,Y,B),
        displayLine(9,Y,B),
        nl,nl.



isValid(b,Yi,Bi,InitialPos,JumpPos,FinalPos) :-
        member(InitialPos,Bi),
        validJump2(InitialPos,FinalPos,JumpPos),
        isEmpty(FinalPos,Yi,Bi),
        isNotEmpty(JumpPos, Yi,Bi).
isValid(y,Yi,Bi,InitialPos,JumpPos,FinalPos) :-
        member(InitialPos,Yi),
        validJump2(InitialPos,FinalPos,JumpPos),
        isEmpty(FinalPos,Yi,Bi),
        isNotEmpty(JumpPos, Yi,Bi).
        
        
isNotEmpty(X,Yi,Bi):- boardMembers(L),!, member(X,L), (member(X,Yi) ; member(X,Bi)).
           
isEmpty(X,Yi,Bi):- boardMembers(L), !,member(X,L),!, \+ member(X,Yi), \+ member(X,Bi).
getEmptyElement(Y,B,[BoardHead|_],Empty):- \+member(BoardHead,Y), \+member(BoardHead,B), Empty = BoardHead.
getEmptyElement(Y,B,[_|BoardTail],Empty):- getEmptyElement(Y,B,BoardTail,Empty).

getEmptyPositions(Y,B,EmptyPositions):- boardMembers(Board), findall(X,(member(X,Board),\+member(X,Y),\+member(X,B)),EmptyPositions).
        
% Yellow jumps yellow mover   
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Yi),
        delete(Yi,InitialPos,Yo2),
        append([FinalPos],Yo2,Yo3),
        member(JumpPos,Yi),
        delete(Yo3,JumpPos,Yo),
        append([],Bi,Bo).

% Yellow jumps blue mover and selects valid mover to remove.     
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Yi),
        delete(Yi,InitialPos,Yo2),
        append([FinalPos],Yo2,Yo3),
        member(JumpPos,Bi),
        write('Select mover to remove'),
        read(MoverToRemove),
        member(MoverToRemove,Yo3),
        delete(Yo3,MoverToRemove,Yo),
        append([],Bi,Bo).

% Yellow jumps blue mover and selects invalid mover to remove.  
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Yi),
        delete(Yi,InitialPos,Yo2),
        append([FinalPos],Yo2,Yo3),
        member(JumpPos,Bi),
        write('Select mover to remove'),
        read(MoverToRemove),
        \+ member(MoverToRemove,Yo3),
        write('Invalid mover to remove'),
        move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo).

% Blue jumps blue mover   
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Bi),
        delete(Bi,InitialPos,Bo2),
        append([FinalPos],Bo2,Bo3),
        member(JumpPos,Bi),
        delete(Bo3,JumpPos,Bo),
        append([],Yi,Yo).

% Blue jumps yellow mover and selects valid mover to remove.     
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Bi),
        delete(Bi,InitialPos,Bo2),
        append([FinalPos],Bo2,Bo3),
        member(JumpPos,Yi),
        write('Select mover to remove'),
        read(MoverToRemove),
        member(MoverToRemove,Bo3),
        delete(Bo3,MoverToRemove,Bo),
        append([],Yi,Yo).

% Blue jumps yellow mover and selects invalid mover to remove.  
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Bi),
        delete(Bi,InitialPos,Bo2),
        append([FinalPos],Bo2,Bo3),
        member(JumpPos,Yi),
        write('Select mover to remove'),
        read(MoverToRemove),
        \+ member(MoverToRemove,Bo3),
        write('Invalid mover to remove'),
        move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo).

makePlay(Player,Yi,Bi,InitialPos,FinalPos,Yo,Bo) :- 
        isValid(Player,Yi,Bi,InitialPos,JumpPos,FinalPos),
        move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo).

makePlay(_,Yi,Bi,_,_,Yi,Bi) :- 
        write('Invalid Move, try again'),nl.

isPossibleToMoveAgain(Yi,Bi,FirstInitialPos,PrevFinalPos) :-
        validJump2(PrevFinalPos,NextFinalPos,JumpPos),
        \+ member(NextFinalPos,Yi),
        \+ member(NextFinalPos,Bi),
        (member(JumpPos,Yi); member(JumpPos,Bi)),
        NextFinalPos \= FirstInitialPos.

isPossibleToMoveAgain(Yi,Bi,FirstInitialPos,PrevFinalPos) :-
        validJump2(NextFinalPos,PrevFinalPos,JumpPos),
        \+ member(NextFinalPos,Yi),
        \+ member(NextFinalPos,Bi),
        (member(JumpPos,Yi); member(JumpPos,Bi)),
        NextFinalPos \= FirstInitialPos.

isGameOver(YellowMovers,BlueMovers):-
        getAllPossibleMoves(y,YellowMovers,YellowMovers,BlueMovers,Moves),!,
        list_empty(Moves,Result),
        Result == true.
isGameOver(YellowMovers,BlueMovers):-
        getAllPossibleMoves(b,BlueMovers,YellowMovers,BlueMovers,Moves),!,
        list_empty(Moves,Result),
        Result == true.

list_empty([], true).
list_empty([_|_], false).


getAllPossibleMovesAux(Player, [Start|Tail],Yi, Bi, CalcMoves, Moves) :-
        isValid(Player,Yi,Bi,Start, Mid, Final),
        getAllPossibleMovesAux(Player,Tail,Yi,Bi,[[Start,Final,Mid]|CalcMoves], Moves).
        
/* Used when previous condition fails*/        
getAllPossibleMovesAux(Player,[_|Tail],Yi,Bi,CalcMoves,Moves):- getAllPossibleMovesAux(Player,Tail,Yi,Bi,CalcMoves,Moves).

getAllPossibleMovesAux(_,[],_,_,Moves,Moves).

getAllPossibleMoves(Player,StartingPositions,YMovers,BMovers,Moves):-
        getAllPossibleMovesAux(Player,StartingPositions,YMovers,BMovers,[],Moves).


game(Yi,Bi,y,2):-
        displayBoard(Yi,Bi),
        write('Yellow turn'),nl,
        write(' initialPos, finalPos'),nl,
        read(InitialPos), read(FinalPos),
        makePlay(y,Yi,Bi,InitialPos, FinalPos,Yo,Bo),
        game(Yo,Bo,b,2).
game(Yi,Bi,b,2):-
        write('Blue Bot Playing'),nl,
        getAllPossibleMoves(b,Bi,Yi,Bi,[[Start,Final,Mid] | _]),
        write('Moving ' + Start + ' To ' + Final),nl,
        move(Yi,Bi,Start,Mid,Final,Yo,Bo),
        game(Yo,Bo,y,2).

game(Yi,Bi,y,1) :- 
        displayBoard(Yi,Bi),
        write('Yellow turn'),nl,
        write(' initialPos, finalPos'),nl,
        read(InitialPos), read(FinalPos),
        makePlay(y,Yi,Bi,InitialPos, FinalPos,Yo,Bo),
        game(Yo,Bo,b,1).

game(Yi,Bi,b,1) :- 
        displayBoard(Yi,Bi),
        write('Blue turn'),nl,
        write('initialPos, finalPos'),nl,
        read(InitialPos), read(FinalPos),
        makePlay(b,Yi,Bi,InitialPos, FinalPos,Yo,Bo),
        game(Yo,Bo,y,1).

game(_,_,_,_):- write('Game Over').

validStartOption(MODE) :- integer(MODE), MODE > 0, MODE < 4.

start :- nl, put_code(201), 
        put_code(205), put_code(205),put_code(205),put_code(205), put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        write('MENU'),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(187),nl,
        put_code(186), write('  1.Player vs Player  '), put_code(186),nl,
        put_code(186), write('  2.Player vs PC      '), put_code(186),nl,
        put_code(186), write('  3.PC vs PC          '), put_code(186),nl,
        put_code(200), 
        put_code(205), put_code(205),put_code(205),put_code(205), put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(205), put_code(205), put_code(205), put_code(205),
        put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),put_code(205),
        put_code(188),nl,nl,nl,
        repeat,
                write('Insert Valid Option: '),
                read(MODE),nl,
                validStartOption(MODE),
        !,
        initialBoard(Y,B),
        game(Y,B,y,MODE).
