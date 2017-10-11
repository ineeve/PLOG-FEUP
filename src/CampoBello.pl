/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:3; tab-width:8; -*- */


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
pontosB([H|T],P) :- pontosB(T,P), (member(H,boardB) -> P is P + 3; P is P + 1).

pontosY([],0).
pontosY([H|T],P) :- pontosY(T,P), (member(H,boardY) -> P is P + 3; P is P + 1).

pontos(Player,Pontos) :- (Player == b -> pecasB(X), pontosB(X,Pontos); pecasY(X),pontosY(X,Pontos)).

isConnected(X,Y) :- boardMember(B), member(X,B), member(Y,B), (connected(X,L), member(Y,L)) ; (connected(Y,L), member(X,L)).

displaySingleP(P) :-(pecasY(Y), member(P,Y) -> write(y); pecasB(B), member(P,B) -> write(b); write(e)), write(' ').
displayPos([H|T]) :- displaySingleP(H), displayPos(T).
displayPos([]).
displayLine(1) :- write('   '),displayPos([g9,g8,g7,g6]),nl.
displayLine(2) :- displaySingleP(b6), write('  '), displayPos([g5,g4,g3]), write('  '), displaySingleP(r9),nl.
displayLine(3) :- write(' '), displaySingleP(b3), write('  '), displayPos([g2,g1]), write('  '), displaySingleP(r5),nl.
displayLine(4) :- displayPos([b7,b1]), write('  '), displaySingleP(g0), write('  '),displayPos([r2,r8]), nl.
displayLine(5) :- write(' '), displaySingleP(b4), write(' '), displayPos([b0,mid,r0]),write(' '),displaySingleP(r4),nl.
displayLine(6) :- displayPos([b8,b2]), write('  '), displaySingleP(y0), write('  '),displayPos([r1,r7]), nl.
displayLine(7) :- write(' '), displaySingleP(b5), write('  '), displayPos([g1,g2]), write('  '), displaySingleP(r6),nl.
displayLine(8) :- displaySingleP(b9), write('  '), displayPos([y3,y4,y5]), write('  '), displaySingleP(r6),nl.
displayLine(9) :- write('   '),displayPos([y6,y7,y8,y9]),nl.
displayBoard :- displayLine(1), displayLine(2), displayLine(3), displayLine(4),displayLine(5),displayLine(6),displayLine(7),displayLine(8),displayLine(9).
