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

% procurar_peca(T, Nome, X, Y):- (nth1(1, T, Linha1), nth1(X, Linha1, Nome), Y = 1);
%                                (nth1(2, T, Linha2), nth1(X, Linha2, Nome), Y = 2);
%                                (nth1(3, T, Linha3), nth1(X, Linha3, Nome), Y = 3);
%                                (nth1(4, T, Linha4), nth1(X, Linha4, Nome), Y = 4);
%                                (nth1(5, T, Linha5), nth1(X, Linha5, Nome), Y = 5);
%                                (nth1(6, T, Linha6), nth1(X, Linha6, Nome), Y = 6);
%                                (nth1(7, T, Linha7), nth1(X, Linha7, Nome), Y = 7).

% inicializar_tabuleiro(T) :- T = [[0,   0, 0,  0, 0,   0, 0],
%                                  [0, gp1, 0, Peca, 0, gp2, 0],
%                                  [0,   0, 0,  0, 0,   0, 0],
%                                  [0,   0, 0,  0, 0,   0, 0],
%                                  [0,   0, 0,  0, 0,   0, 0],
%                                  [0, gb1, 0, rb, 0, gb2, 0],
%                                  [0,   0, 0,  0, 0,   0, 0]].

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

rp ser_rei.
rb ser_rei.
gp1 ser_guerreiro.
gp2 ser_guerreiro.
gb1 ser_guerreiro.
gb2 ser_guerreiro.
Peca ser_peca :- Peca ser_rei; Peca ser_guerreiro.

rp ser_preto.
gp1 ser_preto.
gp2 ser_preto.
rb ser_branco.
gb1 ser_branco.
gb2 ser_branco.

X-Y ser_valido :- X >= 1, X =< 7, Y >= 1, Y =< 7.

Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 1, Y is Yi + 1 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 1, Y is Yi - 1 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 1, Y is Yi + 1 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 1, Y is Yi - 1 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 1, Y is Yi e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 1, Y is Yi e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi, Y is Yi + 1 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_peca, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi, Y is Yi - 1 e X-Y ser_valido.

Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 2, Y is Yi + 2 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 2, Y is Yi - 2 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 2, Y is Yi + 2 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 2, Y is Yi - 2 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi + 2, Y is Yi e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi - 2, Y is Yi e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi, Y is Yi + 2 e X-Y ser_valido.
Peca pode_ir_para X-Y no_tabuleiro T :- Peca ser_guerreiro, Peca esta_em Xi-Yi no_tabuleiro T, X is Xi, Y is Yi - 2 e X-Y ser_valido.



