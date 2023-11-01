steps_between_pieces(T, PieceToMove, EndCoord, NumberOfSteps) :-
    PieceToMove esta_em Xi-Yi no_tabuleiro T,
    not(check_all_invalid(T, EndCoord)),
    steps_between_pieces(T, PieceToMove, [Xi-Yi], [Xi-Yi], [0], NumberOfSteps, EndCoord), !.
% steps_between_pieces(Tabuleiro, PieceToMove, Queue, Visited, NumberOfStepsList, TotalNumberOfSteps, EndCoordinates).
steps_between_pieces(T, PieceToMove, [XHead-YHead | Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    memberchk(XHead-YHead, EndCoord), TotalNumberOfSteps is CurrentNumberOfSteps, !.

steps_between_pieces(T, PieceToMove, [HeadCoord|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    setof(Coord,
    ( posso_mover_aux(T, PieceToMove, HeadCoord, Coord),
    not(member(Coord, Visited))), FoundList),

    append(FoundList, Visited, NewVisited),
    append(Remainder, FoundList, NewRemainder),

    NewCurrentNumberOfSteps is CurrentNumberOfSteps + 1,
    length(FoundList, Size),
    build_list(NewCurrentNumberOfSteps, Size, NewCurrentNumberOfStepsList),
    append(RestList, NewCurrentNumberOfStepsList, NewRestList),

    steps_between_pieces(T, PieceToMove, NewRemainder, NewVisited, NewRestList, TotalNumberOfSteps, EndCoord), !.

steps_between_pieces(T, PieceToMove, [HeadCoord|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    steps_between_pieces(T, PieceToMove, Remainder, Visited, RestList, TotalNumberOfSteps, EndCoord), !.

build_list(X, N, List)  :- 
    findall(X, between(1, N, _), List).

check_all_invalid(T, []).
check_all_invalid(T, [HeadCoord | Remainder]) :-
    Peca esta_em HeadCoord no_tabuleiro T,
    Peca = -1,
    check_all_invalid(T, Remainder).
check_all_invalid(T, [X-Y | Remainder]) :-
    (X < 1; X > 7; Y < 1; Y > 7),
    check_all_invalid(T, Remainder).

valid_moves(T, Player, ListOfMoves) :- 
    Player is 1,
    setof(Xf-Yf, posso_mover(T, n, Xf, Yf), NListOfMoves),
    setof(Xf-Yf, posso_mover(T, p, Xf, Yf), PListOfMoves),
    setof(Xf-Yf, posso_mover(T, q, Xf, Yf), QListOfMoves),
    ListOfMoves = [[n, NListOfMoves], [p, PListOfMoves], [q, QListOfMoves]].

valid_moves(T, Player, ListOfMoves) :-
    Player is 2,
    setof(Xf-Yf, posso_mover(T, u, Xf, Yf), UListOfMoves),
    setof(Xf-Yf, posso_mover(T, b, Xf, Yf), BListOfMoves),
    setof(Xf-Yf, posso_mover(T, d, Xf, Yf), DListOfMoves),
    ListOfMoves = [[u, UListOfMoves], [b, BListOfMoves], [d, DListOfMoves]].

value(T, PieceToMove, ListCoord, Value) :-
    steps_between_pieces(T, PieceToMove, ListCoord, NumberOfSteps),
    Value is 1 / NumberOfSteps.
value(T, PieceToMove, ListCoord, 0).

value(T, 1, Value) :- 
    not(u esta_em Xu-Yu no_tabuleiro T),
    Value is 100.

value(T, 1, Value) :-
    not(n esta_em Xn-Yn no_tabuleiro T),
    Value is -100.

value(T, 1, Value) :-
    u esta_em Xu-Yu no_tabuleiro T,
    n esta_em Xn-Yn no_tabuleiro T,
    value(T, p, [Xu-Yu], PValue),
    value(T, q, [Xu-Yu], QValue),
    value(T, b, [Xn-Yn], BValue),
    value(T, d, [Xn-Yn], DValue),
    Value is PValue + QValue - BValue - DValue.

value(T, 2, Value) :-
    value(T, 1, Value1),
    Value is -Value1.


:- op(1000, xfy, e).
:- op(1200, xfx, se).

X e Y :- X, Y.
X se Y.

:- op(600, xfy, esta_em).
:- op(570, xfy, no_tabuleiro).
:- op(600, xfy, pode_ir_para).
:- op(560, xf, ser_rei).
:- op(560, xf, ser_guerreiro).
:- op(560, xf, ser_peca).
:- op(560, xf, ser_valido).
:- op(560, xf, ser_preto).
:- op(560, xf, ser_branco).

Nome esta_em X-Y no_tabuleiro T :- procurar_peca(T, Nome, X, Y).

n ser_rei.
u ser_rei.
p ser_guerreiro.
q ser_guerreiro.
b ser_guerreiro.
d ser_guerreiro.
Peca ser_peca :- Peca ser_rei; Peca ser_guerreiro.

n ser_preto.
p ser_preto.
q ser_preto.
u ser_branco.
b ser_branco.
d ser_branco.

X-Y ser_valido :- X >= 1, X =< 7, Y >= 1, Y =< 7.

direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX = 0, Dir = norte.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX = 0, Dir = sul.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY = 0, DeltaX > 0, Dir = este.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY = 0, DeltaX < 0, Dir = oeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX > 0, DeltaX is -DeltaY, Dir = nordeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX < 0, DeltaX is DeltaY, Dir = noroeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX > 0, DeltaX is DeltaY, Dir = sudeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX < 0, DeltaX is -DeltaY, Dir = sudoeste.

mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = norte, Xf is Xi, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sul, Xf is Xi, Yf is Yi + 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = este, Xf is Xi + 1, Yf is Yi.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = oeste, Xf is Xi - 1, Yf is Yi.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = nordeste, Xf is Xi + 1, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = noroeste, Xf is Xi - 1, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sudeste, Xf is Xi + 1, Yf is Yi + 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sudoeste, Xf is Xi - 1, Yf is Yi + 1.

posso_mover_aux(T, Peca, Xi-Yi, Xf-Yf) :- PecaDestino esta_em Xf-Yf no_tabuleiro T,
                                          PecaDestino \= -1, 
                                          direcao(Xi-Yi, Xf-Yf, Dir), 
                                        ((Peca ser_guerreiro, posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, 2));
                                         (Peca ser_rei, posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, 1))),
                                          Xf-Yf ser_valido.

posso_mover(T, Peca, Xf, Yf) :- Peca esta_em Xi-Yi no_tabuleiro T,
                                posso_mover_aux(T, Peca, Xi-Yi, Xf-Yf).

posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- Xi-Yi = Xf-Yf.
posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- NumberStepsLeft > 0,
                                                      mover_um_na_direcao(Xi-Yi, Xi1-Yi1, Dir),
                                                      PecaDestino esta_em Xi1-Yi1 no_tabuleiro T,
                                                    ((PecaDestino = -1, posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft));
                                                     (PecaDestino = 0, posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft - 1));
                                                     posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, 0)).
