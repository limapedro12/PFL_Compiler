:-use_module(library(lists)).

not(X) :- X, !, fail.
not(_X).

connects_bfs(S, F):-
    connects_bfs([S], F, [S]).

% connects_bfs(Queue, Final, Visited).
connects_bfs([F|_], F, _V).
connects_bfs([S|R], F, V):-
    findall(N,
    ( connected(S, N),
    not(member(N, V)),
    not(member(N, [S|R]))), L),
    append(R, L, NR),
    connects_bfs(NR, F, [S|V]).

connected(1, 2).
connected(1, 3).
connected(1, 4).
connected(2, 3).
connected(2, 5).
connected(3, 4).
connected(5, 6).

my_bfs(StartingNode) :-
    my_bfs([StartingNode], [StartingNode]).
% my_bfs(Queue, Visited).
my_bfs([], _V).
my_bfs([Head|Remainder], Visited) :-
    write(Head), nl,
    findall(Node,
    ( connected(Head, Node),
    not(member(Node, Visited))), FoundList),
    append(Visited, FoundList, NewVisited),
    append(Remainder, FoundList, NewRemainder),
    my_bfs(NewRemainder, NewVisited).

path(StartingNode, EndNode, ResultingPath) :-
    path([StartingNode], [StartingNode], [[StartingNode]], ResultingPath, EndNode).
% path(Queue, Visited, PreviousPaths, ResultingPath, EndNode).
path([], _V, _P, [], _E).
path([Head|Remainder], Visited, [PreviousPath|_R], ResultingPath, EndNode) :-
    Head = EndNode, reverse(PreviousPath, ResultingPath).
path([Head|Remainder], Visited, [PreviousPath|PathsRemainder], ResultingPath, EndNode) :-
    write(Head), nl,
    findall(Node,
    ( connected(Head, Node),
    not(member(Node, Visited))), FoundList),

    append(Visited, FoundList, NewVisited),
    append(Remainder, FoundList, NewRemainder),

    add_the_begining(FoundList, PreviousPath, NewPreviousPath),
    append(PathsRemainder, NewPreviousPath, NewPathsRemainder),

    path(NewRemainder, NewVisited, NewPathsRemainder, ResultingPath, EndNode).

add_the_begining([], ListToAppend, []).
add_the_begining([Head | Rest], ListToAppend, [HeadList | RestLists]) :-
    HeadList = [Head|ListToAppend],
    add_the_begining(Rest, ListToAppend, RestLists).

number_of_steps(StartingNode, EndNode, NumberOfSteps) :-
    number_of_steps([StartingNode], [StartingNode], [0], NumberOfSteps, EndNode).
% number_of_steps(Queue, Visited, NumberOfStepsList, TotalNumberOfSteps, EndNode).
number_of_steps([Head|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndNode) :-
    Head = EndNode, TotalNumberOfSteps is CurrentNumberOfSteps.
number_of_steps([Head|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndNode) :-
    write(Head), nl,
    findall(Node,
    ( connected(Head, Node),
    not(member(Node, Visited))), FoundList),

    append(Visited, FoundList, NewVisited),
    append(Remainder, FoundList, NewRemainder),

    NewCurrentNumberOfSteps is CurrentNumberOfSteps + 1,
    length(FoundList, Size),
    build_list(NewCurrentNumberOfSteps, Size, NewCurrentNumberOfStepsList),
    append(RestList, NewCurrentNumberOfStepsList, NewRestList),

    number_of_steps(NewRemainder, NewVisited, NewRestList, TotalNumberOfSteps, EndNode).
    
build_list(X, N, List)  :- 
    findall(X, between(1, N, _), List).

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

posso_mover(T, Peca, Xf, Yf) :- Peca esta_em Xi-Yi no_tabuleiro T,
                                PecaDestino esta_em Xf-Yf no_tabuleiro T,
                                PecaDestino \= -1, 
                                direcao(Xi-Yi, Xf-Yf, Dir),
                             ((Peca ser_guerreiro, posso_mover(T, Xi-Yi, Xf-Yf, Dir, 2));
                              (Peca ser_rei, posso_mover(T, Xi-Yi, Xf-Yf, Dir, 1))),
                               Xf-Yf ser_valido.

posso_mover(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- Xi-Yi = Xf-Yf.
posso_mover(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- NumberStepsLeft > 0,
                                                      mover_um_na_direcao(Xi-Yi, Xi1-Yi1, Dir),
                                                      PecaDestino esta_em Xi1-Yi1 no_tabuleiro T,
                                                    ((PecaDestino = -1, posso_mover(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft));
                                                     (PecaDestino = 0, posso_mover(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft - 1));
                                                     posso_mover(T, Xi1-Yi1, Xf-Yf, Dir, 0)).
