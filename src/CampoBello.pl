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