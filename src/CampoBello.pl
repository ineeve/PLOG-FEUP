/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:3; tab-width:8; -*- */

:- use_module(library(lists)).

boardMember([mid,r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,y0,y1,y2,y3,y4,y5,y6,y7,y8,y9]).

connected(r0,[g0,r1,r2,mid,y0]).
connected(r1,[r2,r3,r4]).
connected(r2,[r4,r5]).
connected(r3,[r4,r6,r7]).
connected(r4,[r5,r7,r8]).
connected(r5,[r8,r9]).
connected(r6,[r7]).
connected(r7,[r8]).
connected(r8,[r9]).

connected(g0,[b0,g1,g2,mid]).
connected(g1,[g2,g3,g4]).
connected(g2,[g4,g5]).
connected(g3,[g4,g6,g7]).
connected(g4,[g5,g7,g8]).
connected(g5,[g8,g9]).
connected(g6,[g7]).
connected(g7,[g8]).
connected(g8,[g9]).

connected(b0,[y0,b1,b2,mid]).
connected(b1,[b2,b3,b4]).
connected(b2,[b4,b5]).
connected(b3,[b4,b6,b7]).
connected(b4,[b5,b7,b8]).
connected(b5,[b8,b9]).
connected(b6,[b7]).
connected(b7,[b8]).
connected(b8,[b9]).

connected(y0,[y1,y2,mid]).
connected(y1,[y2,y3,y4]).
connected(y2,[y4,y5]).
connected(y3,[y4,y6,y7]).
connected(y4,[y5,y7,y8]).
connected(y5,[y8,y9]).
connected(y6,[y7]).
connected(y7,[y8]).
connected(y8,[y9]).


blueMoversInitialPos([r1,r2,r3,r4,r5,r6,r7,r8,r9,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowMoversInitialPos([y1,y2,y3,y4,y5,y6,y7,y8,y9,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

blueMoversMidGamePos([y1,r1,r5,r6,r7,r8,r9,b1,b5,b6,b7,mid]).
yellowMoversMidGamePos([r3,y2,y4,y5,y8,y9,g1,g4,g7,g8,g9]).

blueMoversFinalPos([]).
yellowMoversFinalPos([y2,y8,y9,g1,g9]).

blueMoversPos([r1,r2,r3,r4,r5,r6,r7,r8,r9,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowMoversPos([y1,y2,y3,y4,y5,y6,y7,y8,y9,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

blueArea([r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
yellowArea([y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9]).
                                                                     

bluePlayerPoints([],0).
bluePlayerPoints([H|T],P) :- bluePlayerPoints(T,P1), blueArea(B), member(H,B), P is P1 + 3.
bluePlayerPoints([H|T],P) :- bluePlayerPoints(T,P1), (yellowArea(Y), member(H,Y); H==mid), P is P1 + 1.

yellowPlayerPoints([],0).
yellowPlayerPoints([H|T],P) :- yellowPlayerPoints(T,P1), yellowArea(Y), member(H,Y), P is P1 + 3.
yellowPlayerPoints([H|T],P) :- yellowPlayerPoints(T,P1), (H==mid; blueArea(B), member(H,B)), P is P1 + 1.

points(Player,Points) :- Player == b, blueMoversPos(X), bluePlayerPoints(X,Points).
points(Player,Points) :- Player == y, yellowMoversPos(X), yellowPlayerPoints(X,Points).

winner :- points(b,P1), points(y,P2), whoWins(P1,P2).
whoWins(P1,P2) :- P1 > P2, write('Blue Player Wins').
whoWins(P1,P2) :- P1 < P2, write('Yellow Player Wins').
whoWins(P1,P2) :- P1 == P2, write('Draw').

isConnected(X,Y) :- boardMember(B), member(X,B), member(Y,B), (connected(X,L), member(Y,L)) ; (connected(Y,L), member(X,L)).
removeMover(P) :- (blueMoversPos(B),member(P,B) -> delete(B, P, B); yellowMoversPos(Y), delete(Y, P, Y)).


displaySingleP(P, PiecesY, _) :- member(P,PiecesY), write(y), write(' '). 
displaySingleP(P, _, PiecesB) :- member(P,PiecesB), write(b), write(' ').
displaySingleP(_,_,_) :- write(e), write(' ').
displayPos([H|T],PiecesY,PiecesB) :- displaySingleP(H,PiecesY,PiecesB),
                                displayPos(T,PiecesY,PiecesB).
displayPos([],_,_).

displayLine(1,PiecesY,PiecesB) :- write('   '),displayPos([g9,g8,g7,g6],PiecesY,PiecesB),nl.

displayLine(2,PiecesY,PiecesB) :- displaySingleP(b6,PiecesY,PiecesB), write('  '),
                                displayPos([g5,g4,g3],PiecesY,PiecesB), write('  '),
                                displaySingleP(r9,PiecesY,PiecesB),nl.

displayLine(3,PiecesY,PiecesB) :- write(' '), displaySingleP(b3,PiecesY,PiecesB), write('  '),
                                displayPos([g2,g1],PiecesY,PiecesB), write('  '),
                                displaySingleP(r5,PiecesY,PiecesB),nl.

displayLine(4,PiecesY,PiecesB) :- displayPos([b7,b1],PiecesY,PiecesB), write('  '),
                                displaySingleP(g0,PiecesY,PiecesB), write('  '),
                                displayPos([r2,r8],PiecesY,PiecesB), nl.

displayLine(5,PiecesY,PiecesB) :- write(' '), displaySingleP(b4,PiecesY,PiecesB),
                                write(' '), displayPos([b0,mid,r0],PiecesY,PiecesB),
                                write(' '),displaySingleP(r4,PiecesY,PiecesB),nl.

displayLine(6,PiecesY,PiecesB) :- displayPos([b8,b2],PiecesY,PiecesB), write('  '),
                                displaySingleP(y0,PiecesY,PiecesB), write('  '),
                                displayPos([r1,r7],PiecesY,PiecesB), nl.

displayLine(7,PiecesY,PiecesB) :- write(' '), displaySingleP(b5,PiecesY,PiecesB),
                                write('  '), displayPos([g1,g2],PiecesY,PiecesB),
                                write('  '), displaySingleP(r6,PiecesY,PiecesB),nl.
                                
displayLine(8,PiecesY,PiecesB) :- displaySingleP(b9,PiecesY,PiecesB), write('  '),
                                displayPos([y3,y4,y5],PiecesY,PiecesB),
                                write('  '), displaySingleP(r6,PiecesY,PiecesB),nl.

displayLine(9,PiecesY,PiecesB) :- write('   '),displayPos([y6,y7,y8,y9],PiecesY,PiecesB),nl.
displayBoard(Y,B) :- 
        displayLine(1,Y,B),
        displayLine(2,Y,B), 
        displayLine(3,Y,B), 
        displayLine(4,Y,B),
        displayLine(5,Y,B),
        displayLine(6,Y,B),
        displayLine(7,Y,B),
        displayLine(8,Y,B),
        displayLine(9,Y,B).



isValid(y,Yi,Bi,PInicial,PJump,PFinal) :-
        PInicial \= PFinal,
        PInicial \= PJump,
        member(PInicial,Yi),
        \+ member(PFinal,Yi),
        \+ member(PFinal,Bi),
        (member(PJump,Bi) ; member(PJump,Yi)),
        isConnected(PInicial,PJump),
        isConnected(PJump,PFinal).
        

isValid(b,Yi,Bi,PInicial,PJump,PFinal) :- 
        PInicial \= PFinal,
        PInicial \= PJump,
        member(PInicial,Bi),
        \+ member(PFinal,Yi),
        \+ member(PFinal,Bi),
        (member(PJump,Bi) ; member(PJump,Yi)),
        isConnected(PInicial,PJump),
        isConnected(PJump,PFinal).

        
        
move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Yi),
        delete(Yi,InitialPos,Yo2),
        append([FinalPos],Yo2,Yo3),
        delete(Yo3,JumpPos,Yo),
        append([],Bi,Bo).


move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :-
        member(InitialPos,Bi),
        delete(Bi,InitialPos,Bo2),
        append([FinalPos],Bo2,Bo3),
        delete(Bo3,JumpPos,Bo),
        append([],Yi,Yo).

makePlay(Player,Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :- 
        isValid(Player,Yi,Bi,InitialPos,JumpPos,FinalPos),
        move(Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo).

makePlay(Player,Yi,Bi,InitialPos,JumpPos,FinalPos,Yo,Bo) :- 
        \+ isValid(Player,Yi,Bi,InitialPos,JumpPos,FinalPos),
        write('Invalid Move, try again'),nl,
        append([],Yi,Yo),
        append([],Bi,Bo).

game(Yi,Bi) :- 
        displayBoard(Yi,Bi),
        write('player (y or b)'),
        read(Player),
        write(' initialPos, jumpPos, finalPos'),
        read(InitialPos), read(JumpPos), read(FinalPos),
        makePlay(Player,Yi,Bi,InitialPos, JumpPos, FinalPos,Yo,Bo),
        game(Yo,Bo).
game([],_).
game(_,[]).

