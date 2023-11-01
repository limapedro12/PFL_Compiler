:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).

:- consult('robot.pl').

initial_state(T) :- T = [[0, 0, 0, 0, 0, 0, 0],
                         [0, p, 0, n, 0, q, 0],
                         [0, -1, 0, 0, 0, 0, 0],
                         [0, -1, 0, 0, 0, 0, 0],
                         [0, -1, 0, 0, 0, 0, 0],
                         [0, b, 0, u, 0, d, 0],
                         [0, 0, 0, 0, 0, 0, 0]].

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
                            mover_peca_aux(T2, Xi, Yi, Xf, Yf, T3).

move(T, Nome, Xf, Yf, T2) :- write('Movimento Invalido.'), nl, T = T2, fail.

% ainda usamos???
%valid_moves(T, preto, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_preto, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).
%valid_moves(T, branco, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_branco, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).
%

valid_moves(T, preto, L):- findall([Nome, Xf-Yf], (Nome ser_preto, procurar_peca(T, Nome, Xi, Yi), posso_mover(T, Nome, Xf, Yf)), L).
valid_moves(T, branco, L):- findall([Nome, Xf-Yf], (Nome ser_branco, procurar_peca(T, Nome, Xi, Yi), posso_mover(T, Nome, Xf, Yf)), L).

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
display_game(_):- write('     1   2   3   4   5   6   7   '), nl.

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

traduz_peca('p', p).
traduz_peca('q', q).
traduz_peca('n', n).
traduz_peca('b', b).
traduz_peca('d', d).
traduz_peca('u', u).
traduz_peca(_, inv).

choose_move(T, Peca, 1, Equipa, Xf-Yf):- valid_moves(T, Equipa, L),
                                         random_select(M, L, _R),
                                         nth0(0, M, Peca),
                                         nth0(1, M, Xf-Yf).

%choose_move(T, Peca, 2, Equipa, Xf-Yf):- para aqui viria a escolha inteligente...

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
                    choose_move(T, Peca, 1, preto, Xf-Yf),
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
                    sleep(2),
                    choose_move(T, Peca, 2, preto, Xf-Yf),
                    move(T, Peca, Xf, Yf, T2),
                    write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                    nl, display_game(T2), play_1_ai_2(T2, h).

play_ai_ai_1(T, _):- final(T), !.

play_ai_ai_1(T, u):- nl, write('A equipa de reu "u" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, Peca, 1, branco, Xf-Yf),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_1(T2, n).

play_ai_ai_1(T, n):- nl, write('A equipa de reu "n" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, Peca, 1, preto, Xf-Yf),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_1(T2, u).

play_ai_ai_2(T, _):- final(T), !.

play_ai_ai_2(T, u):- nl, write('A equipa de rei "u" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, Peca, 2, branco, Xf-Yf),
                     move(T, Peca, Xf, Yf, T2),
                     write('O computador moveu a peca '), write(Peca), write(' para '), write(Xf-Yf), nl,
                     nl, display_game(T2), play_ai_ai_2(T2, n).

play_ai_ai_2(T, n):- nl, write('A equipa de rei "n" jogara agora...'), nl,
                     sleep(2),
                     choose_move(T, Peca, 2, preto, Xf-Yf),
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
