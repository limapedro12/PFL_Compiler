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
min(X, Y, X) :- X < Y, !.
min(X, Y, Y) :- X >= Y, !.

value(T, PieceToMove, ListCoord, Value) :-
    steps_between_pieces(T, PieceToMove, ListCoord, NumberOfSteps),
    Value is 1 / NumberOfSteps.
value(T, PieceToMove, ListCoord, 0).

value(T, preto, Value) :- 
    not(u esta_em Xu-Yu no_tabuleiro T),
    Value is 100, !.

value(T, preto, Value) :-
    not(n esta_em Xn-Yn no_tabuleiro T),
    Value is -100, !.

print_q44(T) :-
    q esta_em 4-4 no_tabuleiro T,
    not(p esta_em 2-2 no_tabuleiro T),
    display_game(T), 
    trace.

print_q44(_T).

value(T, preto, Value) :- 
    n esta_em Xn-Yn no_tabuleiro T,
    valid_moves(T, branco, ListOfMoves),
    % se n esta em movimentos validos, entao o valor deve ser -90
    memberchk([_, Xn-Yn], ListOfMoves),
    % write('n: '), write(Xn-Yn), nl,
    % write(ListOfMoves), nl,
    Value is -90, !.

value(T, preto, Value) :-
    u esta_em Xu-Yu no_tabuleiro T,
    n esta_em Xn-Yn no_tabuleiro T,

    value(T, p, [Xu-Yu], PValue),
    value(T, q, [Xu-Yu], QValue),
    value(T, b, [Xn-Yn], BValue),
    value(T, d, [Xn-Yn], DValue),

    valid_moves(T, branco, ListOfMoves),
    % se p e q estao em movimentos validos, entao o valor o maximo entre os dois deve ser retirado
    ((p esta_em Xp-Yp no_tabuleiro T, q esta_em Xq-Yq no_tabuleiro T,
      memberchk([_, Xp-Yp], ListOfMoves), memberchk([_, Xq-Yq], ListOfMoves), 
      min(PValue, QValue, MinValue), Value is MinValue - BValue - DValue);
    % se apenas p esta em movimentos validos, entao o valor de p deve ser retirado
     (p esta_em Xp-Yp no_tabuleiro T, memberchk([_, Xp-Yp], ListOfMoves), Value is PValue - BValue - DValue);
    % se apenas q esta em movimentos validos, entao o valor de q deve ser retirado
     (q esta_em Xq-Yq no_tabuleiro T, memberchk([_, Xq-Yq], ListOfMoves), Value is QValue - BValue - DValue);
    % se nenhum esta em movimentos validos
     (Value is PValue + QValue - BValue - DValue)), !.

value(T, branco, Value) :-
    value(T, 1, Value1),
    Value is -Value1.

choose_move(T, Equipa, 1, [Peca, Xf-Yf]):- valid_moves(T, Equipa, L),
                                           random_select(M, L, _R),
                                           nth0(0, M, Peca),
                                           nth0(1, M, Xf-Yf).

choose_move(T, Player, 2, Move) :-
    valid_moves(T, Player, ListOfMoves),
    % write(ListOfMoves), nl,
    choose_move_aux(T, Player, ListOfMoves, -101, [], Value, Move).

choose_move_aux(T, Player, [], Value, Move, Value, Move) :- !.
choose_move_aux(T, Player, [Head | Remainder], CurrValue, CurrMove, Value, Move) :-
    Head = [PieceToMove, X-Y],
    % write(Head), nl,
    move(T, PieceToMove, X, Y, T1),
    % write('Move done'),
    value(T1, Player, CalculatedValue),
    % write('Value: '), write(CalculatedValue), nl,
    CurrValue < CalculatedValue,
    choose_move_aux(T, Player, Remainder, CalculatedValue, Head, Value, Move).
choose_move_aux(T, Player, [Head | Remainder], CurrValue, CurrMove, Value, Move) :-
    choose_move_aux(T, Player, Remainder, CurrValue, CurrMove, Value, Move).

% choose_move_aux_aux(T, Player, PieceToMove, [], Value, Move, Value, Move).
% choose_move_aux_aux(T, Player, PieceToMove, [X-Y | Remainder], CurrValue, CurrMove, Value, Move) :-
%     move(T, PieceToMove, X, Y, T1),
%     value(T1, Player, CalculatedValue),
%     CurrValue < CalculatedValue,
%     choose_move_aux_aux(T, Player, PieceToMove, Remainder, CalculatedValue, [PieceToMove, X-Y], Value, Move).
% choose_move_aux_aux(T, Player, PieceToMove, [X-Y | Remainder], CurrValue, CurrMove, Value, Move) :-
%     choose_move_aux_aux(T, Player, PieceToMove, Remainder, CurrValue, CurrMove, Value, Move).

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
