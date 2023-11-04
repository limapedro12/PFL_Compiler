:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).

:- op(600, xfy, esta_em).
:- op(570, xfy, no_tabuleiro).
:- op(600, xfy, pode_ir_para).
:- op(560, xf, ser_rei).
:- op(560, xf, ser_guerreiro).
:- op(560, xf, ser_valido).
:- op(560, xf, ser_preto).
:- op(560, xf, ser_branco).

:- consult('ai.pl').

% initial_state(-T)
% Retorna o estado (tabuleiro) com a disposição de peças inicial.
initial_state(T) :- T = [[0, 0, 0, 0, 0, 0, 0],
                         [0, p, 0, n, 0, q, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 0],
                         [0, b, 0, u, 0, d, 0],
                         [0, 0, 0, 0, 0, 0, 0]].

% ?Nome esta_em ?Coords no_tabuleiro +T
% Faz corresponder o nome de uma peça às suas coordenadas no tabuleiro de jogo.
Nome esta_em X-Y no_tabuleiro T :- procurar_peca(T, Nome, X, Y).

% +Peca ser_rei
% Diz se uma dada peça é um rei.
n ser_rei.
u ser_rei.

% +Peca ser_guerreiro
% Diz se uma dada peça é um guerreiro.
p ser_guerreiro.
q ser_guerreiro.
b ser_guerreiro.
d ser_guerreiro.

% +Peca ser_preto
% Diz se uma dada peça é da equipa preta ("p"/"n"/"q")
n ser_preto.
p ser_preto.
q ser_preto.

% +Peca ser_branco
% Diz se uma dada peça é da equipa branca ("b"/"u"/"d")
u ser_branco.
b ser_branco.
d ser_branco.

% +Coords ser_valido
% Verifica se um par de coordenadas está dentro dos limites do tabuleiro de jogo.
X-Y ser_valido :- X >= 1, X =< 7, Y >= 1, Y =< 7.

% direcao(+CInicial, ?CFinal, ?Dir)
% Faz corresponder um movimento entre uma posição inicial e uma final no tabuleiro a uma direção e sentido.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX = 0, Dir = norte.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX = 0, Dir = sul.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY = 0, DeltaX > 0, Dir = este.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY = 0, DeltaX < 0, Dir = oeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX > 0, DeltaX is -DeltaY, Dir = nordeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY < 0, DeltaX < 0, DeltaX is DeltaY, Dir = noroeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX > 0, DeltaX is DeltaY, Dir = sudeste.
direcao(Xi-Yi, Xf-Yf, Dir) :- DeltaX is Xf - Xi, DeltaY is Yf - Yi, DeltaY > 0, DeltaX < 0, DeltaX is -DeltaY, Dir = sudoeste.

% mover_um_na_direcao(+CInicial, -CFinal, +Dir)
% Retorna as coordenadas finais correspondentes a um passo desde as coordenadas iniciais na direção e sentido especificados.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = norte, Xf is Xi, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sul, Xf is Xi, Yf is Yi + 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = este, Xf is Xi + 1, Yf is Yi.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = oeste, Xf is Xi - 1, Yf is Yi.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = nordeste, Xf is Xi + 1, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = noroeste, Xf is Xi - 1, Yf is Yi - 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sudeste, Xf is Xi + 1, Yf is Yi + 1.
mover_um_na_direcao(Xi-Yi, Xf-Yf, Dir) :- Dir = sudoeste, Xf is Xi - 1, Yf is Yi + 1.

% posso_mover(+T, ?Peca, ?Xf, ?Yf)
% Verifica a validade de um movimento de uma dada peça entre a sua posição inicial no tabuleiro e uma posição final.
posso_mover(T, Peca, Xf, Yf) :- Peca esta_em Xi-Yi no_tabuleiro T,
                                posso_mover_aux(T, Peca, Xi-Yi, Xf-Yf).

% posso_mover_aux(+T, ?Peca, ?CInicial, ?CFinal)
% Predicado auxiliar a posso_mover/4.
posso_mover_aux(T, Peca, Xi-Yi, Xf-Yf) :- PecaDestino esta_em Xf-Yf no_tabuleiro T,
                                          PecaDestino \= -1, 
                                          direcao(Xi-Yi, Xf-Yf, Dir), 
                                        ((Peca ser_guerreiro, posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, 2));
                                         (Peca ser_rei, posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, 1))),
                                          Xf-Yf ser_valido.

% posso_mover_aux(+T, ?CInicial, ?CFinal, ?Dir, ?NumberStepsLeft)
% Predicado auxiliar a posso_mover_aux/4.
posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- Xi-Yi = Xf-Yf.
posso_mover_aux(T, Xi-Yi, Xf-Yf, Dir, NumberStepsLeft) :- NumberStepsLeft > 0,
                                                      mover_um_na_direcao(Xi-Yi, Xi1-Yi1, Dir),
                                                      PecaDestino esta_em Xi1-Yi1 no_tabuleiro T,
                                                    ((PecaDestino = -1, posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft));
                                                     (PecaDestino = 0, posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, NumberStepsLeft - 1));
                                                     posso_mover_aux(T, Xi1-Yi1, Xf-Yf, Dir, 0)).

% substituir(+Pos, +List, +NewElem, -ListFinal, -OldElem)
% Substitui um elemento numa lista.
substituir(Pos, List, NewElem, ListFinal, OldElem) :- nth1(Pos, List, OldElem, R),
                                                      nth1(Pos, ListFinal, NewElem, R).

% substituir(+X, +Y, +T, +NewElem, -TabuleiroFinal, -OldElem)
% Substitui uma peça no tabuleiro.
substituir(X, Y, T, NewElem, TabuleiroFinal, OldElem) :- nth1(Y, T, Linha),
                                                         substituir(X, Linha, NewElem, LinhaTemp, OldElem),
                                                         substituir(Y, T, LinhaTemp, TabuleiroFinal, _).

% procurar_peca(+T, +Nome, -X, -Y)
% Retorna as coordenadas de uma dada peça no tabuleiro.
procurar_peca(T, Nome, X, Y):- (nth1(1, T, Linha1), nth1(X, Linha1, Nome), Y = 1);
                               (nth1(2, T, Linha2), nth1(X, Linha2, Nome), Y = 2);
                               (nth1(3, T, Linha3), nth1(X, Linha3, Nome), Y = 3);
                               (nth1(4, T, Linha4), nth1(X, Linha4, Nome), Y = 4);
                               (nth1(5, T, Linha5), nth1(X, Linha5, Nome), Y = 5);
                               (nth1(6, T, Linha6), nth1(X, Linha6, Nome), Y = 6);
                               (nth1(7, T, Linha7), nth1(X, Linha7, Nome), Y = 7).

% move(+T, +Nome, +Xf, +Yf, -T3)
% Verifica a legalidade do movimento pretendido para uma dada peça e, em caso de ser permitido, executa-o.
% Invalida as posições no tabuleiro conforme as regras do jogo e o movimento efetuado.
move(T, Nome, Xf, Yf, T3):- posso_mover(T, Nome, Xf, Yf),
                            procurar_peca(T, Nome, Xi, Yi),
                            eliminar_caminho(T, Nome, Xf, Yf, T2),
                            mover_peca_aux(T2, Xi, Yi, Xf, Yf, T3), !.
move(T, Nome, Xf, Yf, T2) :- write('Movimento Invalido.'), nl, T = T2, fail.

% mover_peca_aux(+T, +Xi, +Yi, +Xf, +Yf, -T2)
% Predicado auxiliar a move/5.
mover_peca_aux(T, Xi, Yi, Xf, Yf, T2) :- nth1(Yi, T, LinhaInicial),
                                         substituir(Xi, LinhaInicial, 0, LinhaInicialTemp, Peca),
                                         substituir(Yi, T, LinhaInicialTemp, T3, X1),
                                         nth1(Yf, T3, LinhaFinal),
                                         substituir(Xf, LinhaFinal, Peca, LinhaFinalTemp, X2),
                                         substituir(Yf, T3, LinhaFinalTemp, T4, X3),
                                         T2 = T4.

% valid_moves(+T, +Equipa, -L)
% Retorna os movimentos válidos para uma dada equipa sob a forma de uma lista de listas [Nome da Peça, Coordenadas Finais].
valid_moves(T, preto, L):- setof([Nome, Xf-Yf], ((Nome ser_preto), (procurar_peca(T, Nome, Xi, Yi) ^ posso_mover(T, Nome, Xf, Yf))), L).
valid_moves(T, branco, L):- setof([Nome, Xf-Yf], (Nome ser_branco, (procurar_peca(T, Nome, Xi, Yi) ^ posso_mover(T, Nome, Xf, Yf))), L).

% ordenar(+A, +B, -Maior, -Menor)
% Ordena dois elementos.
ordenar(A, B, Maior, Menor):- A >= B, Maior = A, Menor = B.
ordenar(A, B, Maior, Menor):- A < B, Maior = B, Menor = A.

% posicao_percorrida(+CInicial, +CFinal, -CInterm, +Dir)
% Retorna a posição intermédia "saltada" por um guerreiro.
posicao_percorrida(Xi-Yi, Xf-Yf, X-Y, Dir):- ordenar(Xi, Xf, Xmaior, Xmenor), ordenar(Yi, Yf, Ymaior, Ymenor),
                                             between(Xmenor, Xmaior, I), between(Ymenor, Ymaior, J),
                                             not((I = Xi, J = Yi)), not((I = Xf, J = Yf)),
                                             direcao(Xi-Yi, I-J, Dir),
                                             X is I, Y is J.

% eliminar_caminho(+T, +Nome, +Xf, +Yf, -T2)
% Elimina a posição intermédia "saltada" por um guerreiro.
eliminar_caminho(T, Nome, Xf, Yf, T2) :- procurar_peca(T, Nome, Xi, Yi),
                                         direcao(Xi-Yi, Xf-Yf, Dir),
                                         findall(X-Y, posicao_percorrida(Xi-Yi, Xf-Yf, X-Y, Dir), Lista),
                                         eliminar_caminho_aux(T, Lista, T2).

% eliminar_caminho_aux(+T, +L, -T2)
% Predicado auxiliar a eliminar_caminho/5.
eliminar_caminho_aux(T, [], T).
eliminar_caminho_aux(T, [X-Y|Lista], T2) :- substituir(X, Y, T, -1, T3, _),
                                            eliminar_caminho_aux(T3, Lista, T2).

% not(+X)
% Implementa a negação lógica de um termo.
not(X) :- X, !, fail.
not(_X).

% game_over(+T, -Winner)
% Verifica se o jogo (dado pelo seu tabuleiro) se encontra no fim, i.e. se alguma das equipas ganhou.
game_over(T, branco):- not(procurar_peca(T, n, _X1, _Y1)), write('Rei "u" Ganha!!!').
game_over(T, preto):- not(procurar_peca(T, u, _X2, _Y2)), write('Rei "n" Ganha!!!').

% game_over(+T)
% Wrapper para game_over/2.
game_over(T):- game_over(T, _).

% display_game(+T)
% Imprime o tabuleiro de jogo no terminal.
display_game(T):- print_divisoria, nl,
                  between(1, 7, _N),
                  nth1(_N, T, Linha),
                  write(_N), write(' '),
                  print_linha(Linha), nl,
                  print_divisoria, nl,
                  fail.
display_game(T):- write('     1   2   3   4   5   6   7   '), nl,
                  check_check(T).

% print_divisoria
% Predicado auxiliar a display_game/1 que imprime no terminal uma divisória entre duas linhas do tabuleiro.
print_divisoria :- write('   ----------------------------- '). 

% print_linha(+L)
% Predicado auxiliar a display_game/1 que imprime no terminal uma linha do tabuleiro.
print_linha(L):- write(' | '),
                 between(1, 7, _N),
                 nth1(_N, L, Elem),
                 print_elem(Elem), write(' | '),
                 fail.
print_linha(_L).

% print_elem(+Elem)
% Imprime no terminal uma peça/casa pelo seu caracter.
print_elem(Elem) :- (Elem ser_branco; Elem ser_preto), write(Elem). 
print_elem(-1) :- write('X').
print_elem(0) :- write(' ').

% check_check(+T)
% Verifica se algum dos reis se encontra em check.
check_check(T):- n esta_em Xn-Yn no_tabuleiro T,
                 valid_moves(T, branco, Lb), memberchk([_, Xn-Yn], Lb),
                 write('Rei "n" em check!'), nl, fail.
check_check(T):- u esta_em Xu-Yu no_tabuleiro T,
                 valid_moves(T, preto, Lp), memberchk([_, Xu-Yu], Lp),
                 write('Rei "u" em check!'), nl.
check_check(_T).

% play_1v1(+T, +Equipa)
% Permite jogar uma partida 1v1, especificando a equipa a quem pertence o turno.
play_1v1(T, _):- game_over(T), !.
play_1v1(T, u):- nl, write('E a vez da equipa de rei "u" | 1 - prosseguir | 0 - sair'), nl,
                 read(O), nl,
                 ((O = 0, write('A sair...'), nl);
                 (O = 1, nl,
                 write('Qual peca deseja mover?'), nl,
                 read(Peca),
                 ((Peca ser_branco) ->
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
                 read(Peca),
                 ((Peca ser_preto) ->
                 (write('Introduza a coordenada X do destino:'), nl,
                 read(X), nl,
                 write('Introduza a coordenada Y do destino:'), nl,
                 read(Y), nl,
                 (move(T, Peca, X, Y, T2) -> (nl, display_game(T2), play_1v1(T2, u));
                 (write('Jogada invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, n))));
                 (write('Peca invalida. Vamos tentar outra vez.'), nl,
                 play_1v1(T, n))))).

% play_1_ai_1(+T, +Equipa)
% Permite jogar uma partida contra a IA (movimento aleatório), especificando a equipa a quem pertence o turno.
play_1_ai_1(T, _):- game_over(T), !.
play_1_ai_1(T, h):- nl, write('E a sua vez de jogar | 1 - prosseguir | 0 - sair'), nl,
                    read(O), nl,
                    ((O = 0), write('A sair...'), nl);
                    (O = 1, nl,
                    write('Qual peca deseja mover?'), nl,
                    read(Peca),
                    ((Peca ser_branco) ->
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

% play_1_ai_2(+T, +Equipa)
% Permite jogar uma partida contra a IA (movimento inteligente), especificando a equipa a quem pertence o turno.
play_1_ai_2(T, _):- game_over(T), !.
play_1_ai_2(T, h):- nl, write('E a sua vez de jogar | 1 - prosseguir | 0 - sair'), nl,
                    read(O), nl,
                    ((O = 0), write('A sair...'), nl);
                    (O = 1, nl,
                    write('Qual peca deseja mover?'), nl,
                    read(Peca),
                    ((Peca ser_branco) ->
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

% play_ai_ai_1(+T, +Equipa)
% Permite jogar uma partida entre duas IAs (movimento aleatório), especificando a equipa a quem pertence o turno.
play_ai_ai_1(T, _):- game_over(T), !.
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

% play_ai_ai_2(+T, +Equipa)
% Permite jogar uma partida entre duas IAs (movimento inteligente), especificando a equipa a quem pertence o turno.
play_ai_ai_2(T, _):- game_over(T), !.
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

% escolher_modo(+T)
% Permite escolher o modo de jogo e inicia-o.
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

% play
% Inicia o jogo.
play :- initial_state(T), display_game(T), escolher_modo(T).
