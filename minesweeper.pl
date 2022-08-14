% Políčka musí být rozlišitelná, i když budou obsahovat stejné číslo
% -> budeme používat formát ID-číslo/mina (např a2-7)
mineFine(Board, Tile-Tail) :-
    (
        Tail == x
    );
    (
        \+ Tail == x,
        neighbours(Board, Tile-Tail, Neighbours),
        countMines(Neighbours, N),
        Tail = N
    ).

boardFine(Board) :- boardFine(Board, Board).
boardFine(_, []).
boardFine(FullBoard, [Row | Rest]) :-
    rowFine(FullBoard, Row),
    boardFine(FullBoard, Rest).

rowFine(_, []).
rowFine(FullBoard, [Tile | Rest]) :-
    rowFine(FullBoard, Rest),
    mineFine(FullBoard, Tile).

countMines([],0).
countMines([_-A | Rest], Result) :-
    (
        A == x,
        countMines(Rest, CountRest),
        Result is CountRest + 1
    );
    (
        \+ A == x,
        countMines(Rest, Result)
    ).

testBoard([[a-A, b-B, c-C],[d-D, e-E, f-F],[g-G, h-H, i-I]]).
gameBoard(
    [
        [a1-A1, a2-A2, a3-A3, a4-A4, a5-A5, a6-A6, a7-A7, a8-A8],
        [b1-B1, b2-B2, b3-B3, b4-B4, b5-B5, b6-B6, b7-B7, b8-B8],
        [c1-C1, c2-C2, c3-C3, c4-C4, c5-C5, c6-C6, c7-C7, c8-C8],
        [d1-D1, d2-D2, d3-D3, d4-D4, d5-D5, d6-D6, d7-D7, d8-D8],
        [e1-E1, e2-E2, e3-E3, e4-E4, e5-E5, e6-E6, e7-E7, e8-E8],
        [f1-F1, f2-F2, f3-F3, f4-F4, f5-F5, f6-F6, f7-F7, f8-F8],
        [g1-G1, g2-G2, g3-G3, g4-G4, g5-G5, g6-G6, g7-G7, g8-G8],
        [h1-H1, h2-H2, h3-H3, h4-H4, h5-H5, h6-H6, h7-H7, h8-H8]
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
        [g1-2, g2-3, g3-3, g4-3, g5-3, g6-4, g7-G7, g8-G8],
        [h1-H1, h2-H2, h3-H3, h4-H4, h5-H5, h6-H6, h7-H7, h8-H8]
    ]
    ).
gameBoard3(
    [
        [a1-A1, a2-2, a3-A3, a4-A4, a5-A5, a6-A6, a7-A7, a8-A8],
        [b1-B1, b2-B2, b3-B3, b4-B4, b5-5, b6-B6, b7-B7, b8-B8],
        [c1-C1, c2-C2, c3-C3, c4-C4, c5-C5, c6-C6, c7-C7, c8-C8],
        [d1-D1, d2-D2, d3-D3, d4-D4, d5-D5, d6-D6, d7-D7, d8-D8],
        [e1-E1, e2-E2, e3-E3, e4-E4, e5-E5, e6-E6, e7-E7, e8-E8],
        [f1-F1, f2-F2, f3-F3, f4-F4, f5-F5, f6-F6, f7-F7, f8-F8],
        [g1-G1, g2-G2, g3-G3, g4-G4, g5-G5, g6-G6, g7-G7, g8-G8],
        [h1-H1, h2-H2, h3-H3, h4-H4, h5-H5, h6-H6, h7-H7, h8-H8]
    ]
    ).

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

countBoard([], 0).
countBoard([Row | Rest], Mines) :-
    countMines(Row, MinesRow),
    countBoard(Rest, MinesRest),
    Mines is MinesRow + MinesRest.

%generateRow(Row, Count) :- % hack, I want to nondeterministically choose, which pattern match to try first
%    random_between(0, 1, X),
%    (
%        (
%            X == 0,
%            generateRow1(Row, Count)
%        );
%        (
%            X == 1,
%            generateRow2(Row, Count)
%        )
%    ).
%generateRow1([], 0).
%generateRow1([_-A | Rest], Count) :-
%    \+ A == x,
%    generateRow(Rest, Count).
%generateRow1([_-x | Rest], Count) :-
%    Count > 0,
%    RestCount is Count - 1,
%    generateRow(Rest, RestCount).
%generateRow2([], 0).
%generateRow2([_-x | Rest], Count) :-
%    Count > 0,
%    RestCount is Count - 1,
%    generateRow(Rest, RestCount).
%generateRow2([_-A | Rest], Count) :-
%    \+ A == x,
%    generateRow(Rest, Count).
%
%generateBoard([], 0).
%generateBoard([Row | Rest], Count) :-
%    Minimum is min(Count, 8),
%    findall(X, between(0, Minimum, X), Result),
%    random_permutation(Result, Perm), % random yields just one result, but we might want to backtrack
%    member(CountRow, Perm),
%    generateRow(Row, CountRow),
%    CountRest is Count - CountRow,
%    generateBoard(Rest, CountRest).

addMines(_, 0).
addMines(Board, N) :-
    satisfyNeighbours(Board, N),
    random_permutation(Board, Perm),
    member(Row, Perm),
    addMineRow(Row),
    M is N-1,
    addMines(Perm, M).

addMineRow(Row) :-
    random_permutation(Row, Perm),
    member(_-A, Perm),
    \+ A == x,
    A = x.

solution(Board) :-
    addMines(Board, 10),
    boardFine(Board).

% get neighbours of a tile
neighbours(Board, Elem, Result) :- 
    findall(OtherElem, neighbour(Board, Elem, OtherElem), Result).

neighbour(Board, Elem, OtherElem) :-
    isRowNeighbour(Board, Elem, OtherElem).
neighbour(Board, Elem, OtherElem) :- 
    collumnNeighbour(Board, Elem, OtherElem);
    collumnNeighbour(Board, OtherElem, Elem).

isRowNeighbour([Row | _ ], Elem, OtherElem) :- 
    member(Elem, Row),
    rowNeighbour(Row, Elem, OtherElem).
isRowNeighbour([_ | Rest], Elem, OtherElem) :-
    isRowNeighbour(Rest, Elem, OtherElem).

collumnNeighbour([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    M is N-1,
    nth0(M, Row2, OtherElem).
collumnNeighbour([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    nth0(N, Row2, OtherElem).
collumnNeighbour([Row1, Row2 | _], Elem, OtherElem) :-
    nth0(N, Row1, Elem),
    M is N+1,
    nth0(M, Row2, OtherElem).
collumnNeighbour([_|Rest], Elem, OtherElem) :-
    collumnNeighbour(Rest, Elem, OtherElem).

rowNeighbour([Elem, OtherElem | _], Elem, OtherElem).
rowNeighbour([OtherElem, Elem | _], Elem, OtherElem).
rowNeighbour([ _ | Rest], Elem, OtherElem) :- rowNeighbour(Rest, Elem, OtherElem).