board(0,0,e).
board(0,1,e).
board(0,2,e).
board(0,3,e).
board(0,4,e).
board(0,5,e).
board(0,6,e).
board(0,7,e).

board(1,0,e).
board(1,1,e).
board(1,2,e).
board(1,3,e).
board(1,4,e).
board(1,5,e).
board(1,6,e).
board(1,7,e).

board(2,0,e).
board(2,1,e).
board(2,2,e).
board(2,3,e).
board(2,4,e).
board(2,5,e).
board(2,6,e).
board(2,7,e).

board(3,0,e).
board(3,1,e).
board(3,2,e).
board(3,3,e).
board(3,4,e).
board(3,5,e).
board(3,6,e).
board(3,7,e).

board(4,0,e).
board(4,1,e).
board(4,2,e).
board(4,3,e).
board(4,4,e).
board(4,5,e).
board(4,6,e).
board(4,7,e).

board(5,0,e).
board(5,1,e).
board(5,2,e).
board(5,3,e).
board(5,4,e).
board(5,5,e).
board(5,6,e).
board(5,7,e).

board(6,0,e).
board(6,1,e).
board(6,2,e).
board(6,3,e).
board(6,4,e).
board(6,5,e).
board(6,6,e).
board(6,7,e).

board(7,0,e).
board(7,1,e).
board(7,2,e).
board(7,3,e).
board(7,4,e).
board(7,5,e).
board(7,6,e).
board(7,7,e).

writeLine(Num) :- board(Num,0,_).

showTable :- writeLine(0),
    		writeLine(1),
    		writeLine(2),
    		writeLine(3),
    		writeLine(4),
    		writeLine(5),
    		writeLine(6),
    		writeLine(7).