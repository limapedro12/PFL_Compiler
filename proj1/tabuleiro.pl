:-use_module(library(lists)).
:-consult('robot.pl').
:-use_module(library(between)).

initial_state(T) :- T = [[0,   0, 0,  0, 0,   0, 0],
                                 [0, gp1, 0, rp, 0, gp2, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, -1,  0, 0,   0, 0],
                                 [0, gb1, 0, rb, 0, gb2, 0],
                                 [0,   0, 0,  0, 0,   0, 0]].

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
                                  %eliminar_caminho(T, Nome, Xf, Yf, T2),
                                  mover_peca_aux(T2, Xi, Yi, Xf, Yf, T3).

move(T, Nome, Xf, Yf, T2) :- write('Movimento Invalido.'), nl, T = T2.

valid_moves(T, preto, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_preto, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).
valid_moves(T, branco, ListMoves) :- findall([Nome, Xf-Yf], (Nome ser_branco, procurar_peca(T, Nome, Xi, Yi), Nome pode_ir_para Xf-Yf no_tabuleiro T), ListMoves).

% posso_mover(T, Nome, Xf, Yf):- Xf >= 1, Xf =< 7, Yf >= 1, Yf =< 7,
%                                procurar_peca(T, Nome1, Xf, Yf),
%                                Nome1 \= -1,
%                                procurar_peca(T, Nome, Xi, Yi),
%                                (((Nome = rp; Nome = rb),
%                                 (1 >= Xf - Xi, -1 =< Xf - Xi),
%                                 (1 >= Yf - Yi, -1 =< Yf - Yi));
%                                 ((Nome = gp1; Nome = gp2; Nome = gb1; Nome = gb2),
%                                 ((1 >= Xf - Xi, -1 =< Xf - Xi),
%                                 (1 >= Yf - Yi, -1 =< Yf - Yi));
%                                 (2 is Xf - Xi, 2 is Yf - Yi);
%                                 (2 is Xf - Xi, 0 is Yf - Yi);
%                                 (2 is Xf - Xi, -2 is Yf - Yi);
%                                 (0 is Xf - Xi, 2 is Yf - Yi);
%                                 (0 is Xf - Xi, -2 is Yf - Yi);
%                                 (-2 is Xf - Xi, 2 is Yf - Yi);
%                                 (-2 is Xf - Xi, 0 is Yf - Yi);
%                                 (-2 is Xf - Xi, -2 is Yf - Yi))).

modulo(X, X) :- X >= 0.
modulo(X, Y) :- Y is -X.
ordenar(A, B, Maior, Menor):- A >= B, Maior = A, Menor = B.
ordenar(A, B, Maior, Menor):- A < B, Maior = B, Menor = A.

eliminar_caminho(T, Nome, Xf, Yf, T2) :- procurar_peca(T, Nome, Xi, Yi),
                                         direcao(Xi-Yi, Xf-Yf, Dir),
                                         findall(X-Y, posicao_percorrida(Xi-Yi, Xf-Yf, X-Y, Dir), Lista),
                                         trace, eliminar_caminho_aux(T, Nome, Lista, T2), notrace.

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

final(T):- (not(procurar_peca(T, rp, _X1, _Y1)), write('Branco Ganha!!!')) ; (not(procurar_peca(T, rb, _X2, _Y2)), write('Preto Ganha!!!')).

display_game(T):- print_divisoria, nl,
           between(1, 7, _N),
           nth1(_N, T, Linha),
           write(_N), write(' '),
           print_linha(Linha), nl,
           print_divisoria, nl,
           fail.
display_game(_T) :- write('     1   2   3   4   5   6   7   '), nl.

print_divisoria :- write('   ----------------------------- '). 

print_linha(L):- write(' | '),
                 between(1, 7, _N),
                 nth1(_N, L, Elem),
                 print_elem(Elem), write(' | '),
                 fail.
print_linha(_L).

print_elem(gp1) :- write('p'). 
print_elem(gp2) :- write('q').
print_elem(rp) :- write('n').

print_elem(gb1) :- write('b'). 
print_elem(gb2) :- write('d').
print_elem(rb) :- write('u').

print_elem(-1) :- write('X').
print_elem(0) :- write(' ').

traduz_peca('p', gp1).
traduz_peca('q', gp2).
traduz_peca('n', rp).
traduz_peca('b', gb1).
traduz_peca('d', gb2).
traduz_peca('u', rb).

play_aux(T):- final(T), !.
play_aux(T):- write('1 - jogar | 0 - sair'), nl,
              read(X), nl,
                (X = 1, write('jogar'), nl, 
                write('Qual peca deseja mover?'), nl,
                read(_P), traduz_peca(_P, Peca), nl,
                write('Introduza a coordenada X:'), nl,
                read(_X), nl, 
                write('Introduza a coordenada Y:'), nl,
                read(_Y), nl, 
                move(T, Peca, _X, _Y, T2), nl, 
                display_game(T2),
                play_aux(T2));
              (X = 0, write('sair')).

escolher_modo(T) :- write('1 - jogar 1v1 | 2 - jogar contra AI(nivel 1) | 2 - jogar contra AI(nivel 2) | 0 - sair'), nl,
                    read(X), nl,
                      (X = 1, play_aux(T));
                      (X = 2, play_ai_1(T));
                      (X = 3, play_ai_2(T)).

play_ai_1(T).
play_ai_2(T).

play :- initial_state(T), display_game(T), escolher_modo(T).
