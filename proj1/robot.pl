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

all_paths(StartingNode, EndNode, ResultingPath) :-
    all_paths([StartingNode], [StartingNode], [[StartingNode]], ResultingPath, EndNode).
% all_paths(Queue, Visited, PreviousPaths, ResultingPath, EndNode).
all_paths([], _V, _P, [], _E).
all_paths([Head|Remainder], Visited, [PreviousPath|_R], ResultingPath, EndNode) :-
    Head = EndNode, reverse([Head|PreviousPath], ResultingPath).
all_paths([Head|Remainder], Visited, [PreviousPath|PathsRemainder], ResultingPath, EndNode) :-
    write(Head), nl,
    findall(Node,
    ( connected(Head, Node),
    not(member(Node, Visited))), FoundList),

    append(Visited, FoundList, NewVisited),
    append(Remainder, FoundList, NewRemainder),

    add_the_begining(FoundList, PreviousPath, NewPreviousPath),
    append(PathsRemainder, NewPreviousPath, NewPathsRemainder),

    all_paths(NewRemainder, NewVisited, NewPathsRemainder, ResultingPath, EndNode).

add_the_begining(List1, ListToAppend, ResultingList) :- add_the_begining_aux(List1, ListToAppend, ResultingList, []).
add_the_begining_aux([], ListToAppend, ResultingList, ResultingList).
add_the_begining_aux([Head | Rest], ListToAppend, ResultingList, TempList) :-
    Aux_List = [Head|ListToAppend],
    TempList = [Aux_List|TempList],
    add_the_begining_aux(Rest, ListToAppend, ResultingList, TempList).

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



