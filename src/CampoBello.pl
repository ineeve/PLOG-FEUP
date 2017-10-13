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

pecasB([r1,r2,r3,r4,r5,r6,r7,r8,r9,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
pecasY([y1,y2,y3,y4,y5,y6,y7,y8,y9,g1,g2,g3,g4,g5,g6,g7,g8,g9]).

boardB([r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,b0,b1,b2,b3,b4,b5,b6,b7,b8,b9]).
boardY([y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9]).
                                                                     

pontosB([],0).
pontosB([H|T],P) :- pontosB(T,P1), boardB(B), member(H,B), P is P1 + 3.
pontosB([H|T],P) :- pontosB(T,P1), (boardY(Y), member(H,Y); H==mid), P is P1 + 1.

pontosY([],0).
pontosY([H|T],P) :- pontosY(T,P1), boardY(Y), member(H,Y), P is P1 + 3.
pontosY([H|T],P) :- pontosY(T,P1), (H==mid; boardB(B), member(H,B)), P is P1 + 1.

pontos(Player,Pontos) :- Player == b, pecasB(X), pontosB(X,Pontos).
pontos(Player,Pontos) :- Player == y, pecasY(X), pontosY(X,Pontos).

winner :- pontos(b,P1), pontos(y,P2), whoWins(P1,P2).
whoWins(P1,P2) :- P1 > P2, write('Blue Player Wins').
whoWins(P1,P2) :- P1 < P2, write('Yellow Player Wins').
whoWins(P1,P2) :- P1 == P2, write('Draw').

isConnected(X,Y) :- boardMember(B), member(X,B), member(Y,B), (connected(X,L), member(Y,L)) ; (connected(Y,L), member(X,L)).
removePiece(P) :- (pecasB(B),member(P,B) -> delete(B, P, B); pecasY(Y), delete(Y, P, Y)).


displaySingleP(P, PiecesY, _) :- member(P,PiecesY), write(y), write(' '). 
displaySingleP(P, _, PiecesB) :- member(P,PiecesB), write(b), write(' ').
displaySingleP(_,_,_) :- write(e), write(' ').
displayPos([H|T],PiecesY,PiecesB) :- displaySingleP(H,PiecesY,PiecesB), displayPos(T,PiecesY,PiecesB).
displayPos([],_,_).
displayLine(1,PiecesY,PiecesB) :- write('   '),displayPos([g9,g8,g7,g6],PiecesY,PiecesB),nl.
displayLine(2,PiecesY,PiecesB) :- displaySingleP(b6,PiecesY,PiecesB), write('  '), displayPos([g5,g4,g3],pecasY,pecasB), write('  '), displaySingleP(r9,pecasY,pecasB),nl.
displayLine(3,PiecesY,PiecesB) :- write(' '), displaySingleP(b3,PiecesY,PiecesB), write('  '), displayPos([g2,g1],PiecesY,PiecesB), write('  '), displaySingleP(r5,PiecesY,PiecesB),nl.
displayLine(4,PiecesY,PiecesB) :- displayPos([b7,b1],PiecesY,PiecesB), write('  '), displaySingleP(g0,PiecesY,PiecesB), write('  '),displayPos([r2,r8],PiecesY,PiecesB), nl.
displayLine(5,PiecesY,PiecesB) :- write(' '), displaySingleP(b4,PiecesY,PiecesB), write(' '), displayPos([b0,mid,r0],PiecesY,PiecesB),write(' '),displaySingleP(r4,PiecesY,PiecesB),nl.
displayLine(6,PiecesY,PiecesB) :- displayPos([b8,b2],PiecesY,PiecesB), write('  '), displaySingleP(y0,PiecesY,PiecesB), write('  '),displayPos([r1,r7],PiecesY,PiecesB), nl.
displayLine(7,PiecesY,PiecesB) :- write(' '), displaySingleP(b5,PiecesY,PiecesB), write('  '), displayPos([g1,g2],PiecesY,PiecesB), write('  '), displaySingleP(r6,PiecesY,PiecesB),nl.
displayLine(8,PiecesY,PiecesB) :- displaySingleP(b9,PiecesY,PiecesB), write('  '), displayPos([y3,y4,y5],PiecesY,PiecesB), write('  '), displaySingleP(r6,PiecesY,PiecesB),nl.
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
        (member(PJump,Bi) ; member(PJump,Yi)).
        

isValid(b,Yi,Bi,PInicial,PJump,PFinal) :- 
        PInicial \= PFinal,
        PInicial \= PJump,
        member(PInicial,Bi),
        \+ member(PFinal,Yi),
        \+ member(PFinal,Bi),
        (member(PJump,Bi) ; member(PJump,Yi)).

        
        
move(Yi,_,PosI,PosJ,PosF,Yo,_) :-
        member(PosI,Yi),
        delete(Yi,PosI,Yo2),
        append(Yo2,PosF,Yo3),
        delete(Yo3,PosJ,Yo).


move(_,Bi,PosI,PosJ,PosF,_,Bo) :-
        member(PosI,Bi),
        delete(Bi,PosI,Bo2),
        append(Bo2,PosF,Bo3),
        delete(Bo3,PosJ,Bo).

jogada(Player,Yi,Bi,PosI,PosJump,PosF,Yo,Bo) :- 
        isValid(Player,Yi,Bi,PosI,PosJump,PosF),
        move(Yi,Bi,PosI,PosJump,PosF,Yo,Bo).

jogo(Yi,Bi) :- 
        displayBoard(Yi,Bi),
        write('player (y or b)'),
        read(Player),
        write(' posInicial, posJump, posFinal'),
        read(PosInicial), read(PosJump), read(PosFinal),
        jogada(Player,Yi,Bi,PosInicial, PosJump, PosFinal,Yo,Bo),
        jogo(Yo,Bo).
jogo([],_).
jogo(_,[]).

