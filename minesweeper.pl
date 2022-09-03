% global variables
% ----------------------------------------------------------------------------------------------------
totalNumMines(10).

% To differentiate the tiles without indexing
% we'll use format ID-number/mine (e.g. a2-7)

miniBoard([[a-1, b-_],[c-1, d-_]]).

testBoard([[a-_, b-_, c-_],[d-_, e-_, f-_],[g-_, h-2, i-_]]).
gameBoard(
    [
        [a1-_, a2-_, a3-_, a4-_, a5-_, a6-_, a7-_, a8-_],
        [b1-_, b2-_, b3-_, b4-_, b5-_, b6-_, b7-_, b8-_],
        [c1-_, c2-_, c3-_, c4-_, c5-_, c6-_, c7-_, c8-_],
        [d1-_, d2-_, d3-_, d4-_, d5-_, d6-_, d7-_, d8-_],
        [e1-_, e2-_, e3-_, e4-_, e5-_, e6-_, e7-_, e8-_],
        [f1-_, f2-_, f3-_, f4-_, f5-_, f6-_, f7-_, f8-_],
        [g1-_, g2-_, g3-_, g4-_, g5-_, g6-_, g7-_, g8-_],
        [h1-_, h2-_, h3-_, h4-_, h5-_, h6-_, h7-_, h8-_]
    ]
    ).
gameBoard2(
    [
        [a1-0, a2-0, a3-0, a4-0, a5-0, a6-0, a7-0, a8-0],
        [b1-0, b2-0, b3-0, b4-0, b5-0, b6-0, b7-0, b8-0],
        [c1-0, c2-0, c3-0, c4-0, c5-0, c6-0, c7-0, c8-0],
        [d1-0, d2-0, d3-0, d4-0, d5-0, d6-0, d7-0, d8-0],
        [e1-0, e2-0, e3-0, e4-0, e5-0, e6-0, e7-0, e8-0],
        [f1-0, f2-0, f3-0, f4-0, f5-0, f6-1, f7-2, f8-2],
        [g1-2, g2-3, g3-3, g4-3, g5-3, g6-4, g7-_, g8-_],
        [h1-_, h2-_, h3-_, h4-_, h5-_, h6-_, h7-_, h8-_]
    ]
    ).
gameBoard3(
    [ 
        [a1-_, a2-2, a3-_, a4-_, a5-_, a6-_, a7-_, a8-_],
        [b1-_, b2-_, b3-_, b4-_, b5-5, b6-_, b7-_, b8-_],
        [c1-_, c2-_, c3-_, c4-_, c5-_, c6-_, c7-_, c8-_],
        [d1-_, d2-_, d3-_, d4-_, d5-_, d6-_, d7-_, d8-_],
        [e1-_, e2-_, e3-_, e4-_, e5-_, e6-_, e7-_, e8-_],
        [f1-_, f2-_, f3-0, f4-_, f5-_, f6-_, f7-_, f8-_],
        [g1-_, g2-_, g3-_, g4-_, g5-_, g6-_, g7-_, g8-_],
        [h1-_, h2-_, h3-_, h4-_, h5-_, h6-_, h7-_, h8-_]
    ]
    ).

gameBoard4(
    [ 
        [a1-2, a2-2, a3-_, a4-_, a5-_, a6-_, a7-_, a8-_],
        [b1-_, b2-_, b3-_, b4-_, b5-5, b6-_, b7-_, b8-_],
        [c1-_, c2-_, c3-_, c4-_, c5-_, c6-_, c7-_, c8-_],
        [d1-_, d2-_, d3-_, d4-_, d5-_, d6-_, d7-_, d8-_],
        [e1-_, e2-_, e3-_, e4-_, e5-_, e6-_, e7-_, e8-_],
        [f1-_, f2-_, f3-1, f4-_, f5-_, f6-_, f7-_, f8-_],
        [g1-_, g2-_, g3-_, g4-_, g5-_, g6-_, g7-_, g8-_],
        [h1-_, h2-_, h3-_, h4-_, h5-_, h6-_, h7-_, h8-_]
    ]
    ).

gameBoard5(
    [
        [a1-_, a2-_, a3-_, a4-_, a5-1, a6-_, a7-_, a8-_],
        [b1-_, b2-_, b3-_, b4-_, b5-_, b6-_, b7-_, b8-_],
        [c1-_, c2-_, c3-_, c4-1, c5-_, c6-_, c7-_, c8-_],
        [d1-_, d2-_, d3-_, d4-_, d5-_, d6-_, d7-_, d8-2],
        [e1-_, e2-_, e3-_, e4-_, e5-_, e6-_, e7-1, e8-_],
        [f1-_, f2-3, f3-_, f4-_, f5-_, f6-_, f7-1, f8-_],
        [g1-4, g2-_, g3-_, g4-_, g5-_, g6-_, g7-_, g8-1],
        [h1-_, h2-_, h3-_, h4-_, h5-_, h6-_, h7-_, h8-_]
    ]
    ).

% ------------------------------------------------------------------------------------------

show([]).
show([Row | Rest]) :-
    showRow(Row), 
    nl,
    show(Rest).

showRow([]).
showRow([_-A | Rest]) :-
    write(A),
    write(' '),
    showRow(Rest).

isNotMine(_-0).
isNotMine(_-1).
isNotMine(_-2).
isNotMine(_-3).
isNotMine(_-4).
isNotMine(_-5).
isNotMine(_-6).
isNotMine(_-7).
isNotMine(_-8).

isNumber( _-Tile) :-
     Tile == 0;
     Tile == 1;
     Tile == 2;
     Tile == 3;
     Tile == 4;
     Tile == 5;
     Tile == 6;
     Tile == 7;
     Tile == 8.

listWithElem(List, Elem, List) :-
    member(Elem, List),
    !.
listWithElem(List, Elem, [Elem|List]).


% predicates for viable solution generation
% ----------------------------------------------------------------------------------------------------------------------------
countMines([],0, _).
countMines([_-A | Rest], Result, NotMines) :-
    (
        A == x,
        countMines(Rest, CountRest, NotMines),
        Result is CountRest + 1
    );
    (
        A \== x,
        countMines(Rest, Result, NotMines)
    ).

%satisfyNumbers(+FullBoard, +MaximumNumberOfMines, +BoardYetToSatisfy, -RemainingNumberOfMines, +NotMinesOld, -NotMinesNew)
satisfyNumbers(_, K, [], K, N, N).
satisfyNumbers(Board, N, [Row|Rest], M, NotMinesOld, NotMinesNew) :-
     satisfyNumbersRow(Board, N, Row, O, NotMinesOld, NotMines),
     satisfyNumbers(Board, O, Rest, M, NotMines, NotMinesNew).
 
satisfyNumbersRow(_, K, [], K, N, N).
satisfyNumbersRow(Board, N, [Coords-Count|Rest], M, NotMinesOld, NotMinesNew) :-
     isNumber(Coords-Count),
     listWithElem(NotMinesOld, Coords-Count, NotMinesWithTile),
     !,
     neighbours(Board, Coords-Count, Neighbours),
     addNeighbourMines(Count, Neighbours, NotMinesWithTile, NotMines, Added),
     O is N - Added,
     O >= 0,
     satisfyNumbersRow(Board, O, Rest, M, NotMines, NotMinesNew).

satisfyNumbersRow(Board, N, [_|Rest], M, NotMinesOld, NotMinesNew) :-
    satisfyNumbersRow(Board, N, Rest, M, NotMinesOld, NotMinesNew).

%addNeighbourMines(+Count, +Neighbours, +NotMines, -NewNotMines, +NumOfMinesAdded)
addNeighbourMines(0, [], N, N, 0).

addNeighbourMines(N, [_-Tail|Others], NotMines, NewNotMines, Added) :-
    N >= 0,
    Tail == x,
    !,
    M is N-1,
    addNeighbourMines(M, Others, NotMines, NewNotMines, Added).

addNeighbourMines(N, [Tile-Tail|Others], NotMines, NewNotMines, NewAdded) :-
    N >= 0,
    \+ member(Tile-Tail, NotMines),
    Tail = x,
    O is N-1,
    addNeighbourMines(O, Others, NotMines, NewNotMines, Added),
    NewAdded is Added + 1.

addNeighbourMines(N, [Tile-Tail|Others], NotMines, NewNewNotMines, Added) :-
    N >= 0,
    Tail \== x,
    listWithElem(NotMines, Tile-Tail, NewNotMines),
    addNeighbourMines(N, Others, NewNotMines, NewNewNotMines, Added).

addNumbers(_, [], _).
addNumbers(Board, [Row|Rows], NotMines) :-
    addNumbersRow(Board, Row, NotMines),
    addNumbers(Board, Rows, NotMines).

addNumbersRow(_,[], _).
addNumbersRow(Board, [_-Tail|Rest], NotMines) :-
    Tail == x,
    !,
    addNumbersRow(Board, Rest, NotMines).
addNumbersRow(Board, [Tile-Tail|Rest], NotMines) :-
    isNumber(Tile-Tail),
    !,
    addNumbersRow(Board, Rest, NotMines).
addNumbersRow(Board, [Tile-Tail|Rest], NotMines) :-
    neighbours(Board, Tile-Tail, N),
    countMines(N, Tail, NotMines),
    addNumbersRow(Board, Rest, NotMines).

% it's much faster to take care of the known numbers first
solution(Board, Forbidden) :-
    totalNumMines(N),
    satisfyNumbers(Board, N, Board, Rest, Forbidden, NotMines),
    distributeRandomly(Board, Rest, NotMines, 50),
    addNumbers(Board, Board, NotMines).

% distributeRandomly(+Board, +NumMines, +NotMines, +MaxRecursionDepth)
% if MaxRecursionDepth is reached, distribute the rest of the mines deterministically
distributeRandomly(_, 0,_, _) :- !.
distributeRandomly(Board, N, NotMines, 0) :-
    !,
    distributeRest(Board, N, NotMines).
distributeRandomly(Board, N, NotMines, M) :-
    random_member(Row, Board),
    random_member(Tile-Tail, Row),
    NewMax is M - 1,
    (
        (
            Tail \== x,
            \+ member(Tile-Tail, NotMines),
            Tail = x,
            O is N-1,
            !,
            distributeRandomly(Board, O, NotMines, NewMax)
        );
        (
            distributeRandomly(Board, N, NotMines, NewMax)
        )
    ).

distributeRest(_, 0, _) :- !.

distributeRest([Row|Rest], N, NotMines) :-
    distributeRow(Row, M, NotMines),
    O is N - M,
    O >= 0,
    distributeRest(Rest, O, NotMines).

distributeRow([], 0, _).
distributeRow([_|Xs], N, NotMines) :-
    distributeRow(Xs, N, NotMines).
distributeRow([Tile-X | Xs], N, NotMines) :-
    \+ X == x,
    \+ member(Tile-X, NotMines),
    X = x,
    distributeRow(Xs, M, NotMines),
    N is M+1.

% get neighbours of a tile
% it would be much simpler to just use findall but
% findall didn't unify the variables
neighbours(Board, Elem, Result) :- 
    topLeft(Board, Elem, A),
    topMid(Board, Elem, B),
    topRight(Board, Elem, C),
    midLeft(Board, Elem, D),
    midRight(Board, Elem, E),
    botLeft(Board, Elem, F),
    botMid(Board, Elem, G),
    botRight(Board, Elem, H),
    clearEmpty([A,B,C,D,E,F,G,H], Result).

clearEmpty([], []) :- !.
clearEmpty([empty|Rest], Result) :-
    clearEmpty(Rest, Result),
    !.
clearEmpty([X|Rest], [X|Result]) :-
    X \== empty,
    clearEmpty(Rest, Result),
    !.

midLeft([Row|_], Elem, OtherElem) :-
    member(Elem, Row),
    !,
    rowLeft(Row, Elem, OtherElem).
midLeft([_|Rest], Elem, OtherElem) :-
    midLeft(Rest, Elem, OtherElem).

rowLeft([Elem|_], Elem, empty) :- !.
rowLeft([OtherElem, Elem | _], Elem, OtherElem) :- !.
rowLeft([_|Rest], Elem, OtherElem) :-
    rowLeft(Rest, Elem, OtherElem).

midRight([Row|_], Elem, OtherElem) :-
    member(Elem, Row),
    !,
    rowRight(Row, Elem, OtherElem).
midRight([_|Rest], Elem, OtherElem) :-
    midRight(Rest, Elem, OtherElem).

rowRight([Elem], Elem, empty) :- !.
rowRight([Elem, OtherElem | _], Elem, OtherElem) :- !.
rowRight([_|Rest], Elem, OtherElem) :-
    rowRight(Rest, Elem, OtherElem).

topLeft([Row1 | _], Elem, empty) :-
    member(Elem, Row1),
    !.
topLeft([_, Row2 | _], Elem, empty) :-
    nth0(0, Row2, Elem),
    !.
topLeft([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row2, Elem),
    M is N-1,
    nth0(M, Row1, OtherElem),
    !.
topLeft([_|Rest], Elem, OtherElem) :-
    topLeft(Rest, Elem, OtherElem).

topMid([Row1 | _], Elem, empty) :-
    member(Elem, Row1),
    !.
topMid([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row2, Elem),
    nth0(N, Row1, OtherElem),
    !.
topMid([_| Rest], Elem, OtherElem) :-
    topMid(Rest, Elem, OtherElem).

topRight([Row1 | _], Elem, empty) :-
    member(Elem, Row1),
    !.
topRight([_, Row2 | _], Elem, empty) :-
    nth0(7, Row2, Elem),
    !.
topRight([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row2, Elem),
    M is N+1,
    nth0(M, Row1, OtherElem),
    !.
topRight([_|Rest], Elem, OtherElem) :-
    topRight(Rest, Elem, OtherElem).

botLeft([_], _, empty) :-!.
botLeft([Row1 | _], Elem, empty) :-
    nth0(0, Row1, Elem),
    !.
botLeft([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    M is N-1,
    nth0(M, Row2, OtherElem),
    !.
botLeft([_|Rest], Elem, OtherElem) :-
    botLeft(Rest, Elem, OtherElem).

botMid([_], _, empty) :- !.
botMid([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    nth0(N, Row2, OtherElem),
    !.
botMid([_| Rest], Elem, OtherElem) :-
    botMid(Rest, Elem, OtherElem).

botRight([_], _, empty) :- !.
botRight([Row1 | _], Elem, empty) :-
    nth0(7, Row1, Elem),
    !.
botRight([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    M is N+1,
    nth0(M, Row2, OtherElem),
    !.
botRight([_|Rest], Elem, OtherElem) :-
    botRight(Rest, Elem, OtherElem).

% gameplay predicates
% ------------------------------------------------------------------------------------

showCertain([]).
showCertain([Row | Rest]) :-
    showRowCertain(Row), 
    nl,
    showCertain(Rest).

showRowCertain([]).
showRowCertain([_-A | Rest]) :-
    A == x,
    !,
    write(' '),
    write(x),
    write('  '),
    showRowCertain(Rest).

showRowCertain([Tile-A | Rest]) :-
    isNumber(Tile-A),
    !,
    write(' '),
    write(A),
    write('  '),
    showRowCertain(Rest).

showRowCertain([Tile-_ | Rest]) :-
    write(Tile),
    write('  '),
    showRowCertain(Rest).

getTile(Tile-_) :-
    read(Tile),
    member(Tile, 
        ['a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8',
         'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8',
         'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8',
         'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8',
         'e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8',
         'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8',
         'g1', 'g2', 'g3', 'g4', 'g5', 'g6', 'g7', 'g8',
         'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8'
        ]),
        !.

getTile(Tile) :- getTile(Tile).

win(Visible) :-
    totalNumMines(N),
    WinCount is 64 - N,
    length(Visible, WinCount).

% copyVisible(+Board, +Visible, -NewBoard)
copyVisible([], _, []).
copyVisible([Row|Rest], Visible, [NewRow|NewRest]) :-
    copyRow(Row, Visible, NewRow),
    copyVisible(Rest, Visible, NewRest).

copyRow([], _, []).
copyRow([X | Rest], Visible, [X | NewRest]) :-
    member(X, Visible),
    !,
    copyRow(Rest, Visible, NewRest).
copyRow([Tile-_ | Rest], Visible, [Tile-_ | NewRest]) :-
    copyRow(Rest, Visible, NewRest).


play(Board, Visible) :- 
    win(Visible), 
    write("You won!"),
    nl,
    solution(Board, []),
    show(Board),
    !.

play(Board, Visible) :-
    getTile(Tile),
    solution(Board, [Tile]),
    listWithElem(Visible, Tile, NewVisible),
    copyVisible(Board, NewVisible, NewBoard),
    showCertain(NewBoard),
    play(NewBoard, NewVisible).

play(Board, _) :-
    write("You lose."),
    nl,
    solution(Board, []),
    show(Board).

minesweeper() :-
    gameBoard(B),
    showCertain(B),
    play(B, []).