:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).

:- op(600, xfy, esta_em).
:- op(570, xfy, no_tabuleiro).
:- op(600, xfy, pode_ir_para).
:- op(560, xf, ser_rei).
:- op(560, xf, ser_guerreiro).
:- op(560, xf, ser_peca).
:- op(560, xf, ser_valido).
:- op(560, xf, ser_preto).
:- op(560, xf, ser_branco).

:- consult('ai.pl').


initial_state(T) :- T = [[0, 0, 0, 0, 0, 0, 0],
                         [0, p, 0, n, 0, q, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, b, 0, u, 0, d, 0],
                         [0, 0, 0, 0, 0, 0, 0]].


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


substituir(Pos, List, NewElem, ListFinal, OldElem) :- nth1(Pos, List, OldElem, R),
                                                      nth1(Pos, ListFinal, NewElem, R).

substituir(X, Y, T, NewElem, TabuleiroFinal, OldElem) :- nth1(Y, T, Linha),
                                                         substituir(X, Linha, NewElem, LinhaTemp, OldElem),
                                                         substituir(Y, T, LinhaTemp, TabuleiroFinal, _).

mover_peca_aux(T, Xi, Yi, Xf, Yf, T2) :- nth1(Yi, T, LinhaInicial),
                                         substituir(Xi, LinhaInicial, 0, LinhaInicialTemp, Peca),
                                         substituir(Yi, T, LinhaInicialTemp, T3, X1),
                                         nth1(Yf, T3, LinhaFinal),
                                         substituir(Xf, LinhaFinal, Peca, LinhaFinalTemp, X2),
                                         substituir(Yf, T3, LinhaFinalTemp, T4, X3),
                                         T2 = T4.

procurar_peca(T, Nome, X, Y):- (nth1(1, T, Linha1), nth1(X, Linha1, Nome), Y = 1);
                               (nth1(2, T, Linha2), nth1(X, Linha2, Nome), Y = 2);
                               (nth1(3, T, Linha3), nth1(X, Linha3, Nome), Y = 3);
                               (nth1(4, T, Linha4), nth1(X, Linha4, Nome), Y = 4);
                               (nth1(5, T, Linha5), nth1(X, Linha5, Nome), Y = 5);
                               (nth1(6, T, Linha6), nth1(X, Linha6, Nome), Y = 6);
                               (nth1(7, T, Linha7), nth1(X, Linha7, Nome), Y = 7).

move(T, Nome, Xf, Yf, T3):- posso_mover(T, Nome, Xf, Yf),
                            procurar_peca(T, Nome, Xi, Yi),
                            eliminar_caminho(T, Nome, Xf, Yf, T2),
                            mover_peca_aux(T2, Xi, Yi, Xf, Yf, T3), !.

move(T, Nome, Xf, Yf, T2) :- write('Movimento Invalido.'), nl, T = T2, fail.

% ainda usamos???
%valid_moves(T, preto, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_preto, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).
%valid_moves(T, branco, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_branco, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).
%

valid_moves(T, preto, L):- setof([Nome, Xf-Yf], ((Nome ser_preto), (procurar_peca(T, Nome, Xi, Yi) ^ posso_mover(T, Nome, Xf, Yf))), L).
valid_moves(T, branco, L):- setof([Nome, Xf-Yf], (Nome ser_branco, (procurar_peca(T, Nome, Xi, Yi) ^ posso_mover(T, Nome, Xf, Yf))), L).

modulo(X, X) :- X >= 0.
modulo(X, Y) :- Y is -X.
ordenar(A, B, Maior, Menor):- A >= B, Maior = A, Menor = B.
ordenar(A, B, Maior, Menor):- A < B, Maior = B, Menor = A.

eliminar_caminho(T, Nome, Xf, Yf, T2) :- procurar_peca(T, Nome, Xi, Yi),
                                         direcao(Xi-Yi, Xf-Yf, Dir),
                                         findall(X-Y, posicao_percorrida(Xi-Yi, Xf-Yf, X-Y, Dir), Lista),
                                         eliminar_caminho_aux(T, Lista, T2).

posicao_percorrida(Xi-Yi, Xf-Yf, X-Y, Dir):- ordenar(Xi, Xf, Xmaior, Xmenor), ordenar(Yi, Yf, Ymaior, Ymenor),
                                             between(Xmenor, Xmaior, I), between(Ymenor, Ymaior, J),
                                             not((I = Xi, J = Yi)), not((I = Xf, J = Yf)),
                                             direcao(Xi-Yi, I-J, Dir),
                                             X is I, Y is J.

eliminar_caminho_aux(T, [], T).
eliminar_caminho_aux(T, [X-Y|Lista], T2) :- substituir(X, Y, T, -1, T3, _),
                                            eliminar_caminho_aux(T3, Lista, T2).

not(X) :- X, !, fail.
not(_X).

final(T):- (not(procurar_peca(T, n, _X1, _Y1)), write('Rei "u" Ganha!!!')) ; (not(procurar_peca(T, u, _X2, _Y2)), write('Rei "n" Ganha!!!')).

display_game(T):- print_divisoria, nl,
                  between(1, 7, _N),
                  nth1(_N, T, Linha),
                  write(_N), write(' '),
                  print_linha(Linha), nl,
                  print_divisoria, nl,
                  fail.
display_game(T):- write('     1   2   3   4   5   6   7   '), nl,
                  check_check(T).

print_divisoria :- write('   ----------------------------- '). 

print_linha(L):- write(' | '),
                 between(1, 7, _N),
                 nth1(_N, L, Elem),
                 print_elem(Elem), write(' | '),
                 fail.
print_linha(_L).

print_elem(p) :- write('p'). 
print_elem(q) :- write('q').
print_elem(n) :- write('n').

print_elem(b) :- write('b'). 
print_elem(d) :- write('d').
print_elem(u) :- write('u').

print_elem(-1) :- write('X').
print_elem(0) :- write(' ').

check_check(T):- n esta_em Xn-Yn no_tabuleiro T,
                 valid_moves(T, branco, Lb), memberchk([_, Xn-Yn], Lb),
                 write('Rei "n" em check!'), nl, fail.
check_check(T):- u esta_em Xu-Yu no_tabuleiro T,
                 valid_moves(T, preto, Lp), memberchk([_, Xu-Yu], Lp),
                 write('Rei "u" em check!'), nl.
check_check(_T).

traduz_peca('p', p).
traduz_peca('q', q).
traduz_peca('n', n).
traduz_peca('b', b).
traduz_peca('d', d).
traduz_peca('u', u).
traduz_peca(_, inv).

play_1v1(T, _):- final(T), !.

play_1v1(T, u):- nl, write('E a vez da equipa de rei "u" | 1 - prosseguir | 0 - sair'), nl,
                 read(O), nl,
                 ((O = 0, write('A sair...'), nl);
                 (O = 1, nl,
                 write('Qual peca deseja mover?'), nl,
                 read(P), traduz_peca(P, Peca),
                 ((Peca = b; Peca = u; Peca = d) ->
                 (write('Introduza a coordenada X do destino:'), nl,
                 read(X), nl,
                 write('Introduza a coordenada Y do destino:'), nl,
                 read(Y), nl,
                 (move(T, Peca, X, Y, T2) -> (nl, display_game(T2), play_1v1(T2, n));
                 (write('Jogada invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, u))));
                 (write('Peca invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, u))))).

play_1v1(T, n):- nl, write('E a vez da equipa de rei "n" | 1 - prosseguir | 0 - sair'), nl,
                 read(O), nl,
                 ((O = 0, write('A sair...'), nl);
                 (O = 1, nl,
                 write('Qual peca deseja mover?'), nl,
                 read(P), traduz_peca(P, Peca),
                 ((Peca = p; Peca = n; Peca = q) ->
                 (write('Introduza a coordenada X do destino:'), nl,
                 read(X), nl,
                 write('Introduza a coordenada Y do destino:'), nl,
                 read(Y), nl,
                 (move(T, Peca, X, Y, T2) -> (nl, display_game(T2), play_1v1(T2, u));
                 (write('Jogada invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, n))));
                 (write('Peca invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, n))))).

play_1_ai_1(T, _):- final(T), !.

play_1_ai_1(T, h):- nl, write('E a sua vez de jogar | 1 - prosseguir | 0 - sair'), nl,
                    read(O), nl,
                    ((O = 0), write('A sair...'), nl);
                    (O = 1, nl,
                    write('Qual peca deseja mover?'), nl,
                    read(P), traduz_peca(P, Peca),
                    ((Peca = b; Peca = u; Peca = d) ->
                    (write('Introduza a coordenada X do destino:'), nl,
                    read(X), nl,
                    write('Introduza a coordenada Y do destino:'), nl,
                    read(Y), nl,
                    (move(T, Peca, X, Y, T2) -> (nl, display_game(T2), play_1_ai_1(T2, c));
                    (write('Jogada invalida. Vamos tentar outra vez.'), nl,
                    play_1_ai_1(T, h))));
                    (write('Peca invalida. Vamos tentar outra vez.'), nl,
                    play_1_ai_1(T, h)))).

play_1_ai_1(T, c):- nl, write('O computador jogara agora...'), nl,
                    sleep(2),
                    choose_move(T, preto, 1, [Peca,Xf-Yf]),
                    move(T, Peca, Xf, Yf, T2),
                    write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                    nl, display_game(T2), play_1_ai_1(T2, h).

play_1_ai_2(T, _):- final(T), !.

play_1_ai_2(T, h):- nl, write('E a sua vez de jogar | 1 - prosseguir | 0 - sair'), nl,
                    read(O), nl,
                    ((O = 0), write('A sair...'), nl);
                    (O = 1, nl,
                    write('Qual peca deseja mover?'), nl,
                    read(P), traduz_peca(P, Peca),
                    ((Peca = b; Peca = u; Peca = d) ->
                    (write('Introduza a coordenada X do destino:'), nl,
                    read(X), nl,
                    write('Introduza a coordenada Y do destino:'), nl,
                    read(Y), nl,
                    (move(T, Peca, X, Y, T2) -> (nl, display_game(T2), play_1_ai_2(T2, c));
                    (write('Jogada invalida. Vamos tentar outra vez.'), nl,
                    play_1_ai_2(T, h))));
                    (write('Peca invalida. Vamos tentar outra vez.'), nl,
                    play_1_ai_2(T, h)))).

play_1_ai_2(T, c):- nl, write('O computador jogara agora...'), nl,
                    sleep(1),
                    choose_move(T, preto, 2, [Peca,Xf-Yf]),
                    move(T, Peca, Xf, Yf, T2),
                    write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                    nl, display_game(T2), play_1_ai_2(T2, h).

play_ai_ai_1(T, _):- final(T), !.

play_ai_ai_1(T, u):- nl, write('A equipa de reu "u" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, branco, 1, [Peca,Xf-Yf]),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_1(T2, n).

play_ai_ai_1(T, n):- nl, write('A equipa de reu "n" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, preto, 1, [Peca,Xf-Yf]),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_1(T2, u).

play_ai_ai_2(T, _):- final(T), !.

play_ai_ai_2(T, u):- nl, write('A equipa de rei "u" jogara agora...'), nl,
                     sleep(1),
                     choose_move(T, branco, 2, [Peca,Xf-Yf]),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_2(T2, n).

play_ai_ai_2(T, n):- nl, write('A equipa de rei "n" jogara agora...'), nl,
                     sleep(1),
                     choose_move(T, preto, 2, [Peca,Xf-Yf]),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_2(T2, u).

escolher_modo(T) :- write('1 - jogar 1v1 | 2 - jogar contra IA (nivel 1) | 3 - jogar contra IA (nivel 2) | 4 - IA vs IA (nivel 1) | 5 - IA vs IA (nivel 2) | 0 - sair'), nl,
                    read(X), nl,
                    ((X = 0, write('A sair...'), nl);
                    (X = 1, play_1v1(T, u));
                    (X = 2, write('A sua equipa tem "u" como rei e "b" e "d" como guerreiros'),
                    play_1_ai_1(T, h));
                    (X = 3, write('A sua equipa tem "u" como rei e "b" e "d" como guerreiros'),
                    play_1_ai_2(T, h));
                    (X = 4, play_ai_ai_1(T, u));
                    (X = 5, play_ai_ai_2(T, u))).

play :- initial_state(T), display_game(T), escolher_modo(T).