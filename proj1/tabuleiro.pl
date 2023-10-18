:-use_module(library(lists)).

inicializar_tabuleiro(T) :- T = [[0,   0, 0,  0, 0,   0, 0],
                                 [0, gp1, 0, rp, 0, gp2, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
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

mover_peca(T, Nome, Xf, Yf, T2):- procurar_peca(T, Nome, Xi, Yi),
                                  mover_peca_aux(T, Xi, Yi, Xf, Yf, T2).

posso_mover(T, Nome, Xf, Yf):- Xf >= 1, Xf =< 7, Yf >= 1, Yf =< 7,
                               procurar_peca(T, Nome1, Xf, Yf),
                               Nome1 \= -1,
                               procurar_peca(T, Nome, Xi, Yi),
                               (((Nome = rp; Nome = rb),
                                (1 >= Xf - Xi, -1 =< Xf - Xi),
                                (1 >= Yf - Yi, -1 =< Yf - Yi));
                                ((Nome = gp1; Nome = gp2; Nome = gb1; Nome = gb2),
                                ((1 >= Xf - Xi; -1 =< Xf - Xi),
                                (1 >= Yf - Yi; -1 =< Yf - Yi));
                                (2 is Xf - Xi, 2 is Yf - Yi);
                                (2 is Xf - Xi, 0 is Yf - Yi);
                                (2 is Xf - Xi, -2 is Yf - Yi);
                                (0 is Xf - Xi, 2 is Yf - Yi);
                                (0 is Xf - Xi, -2 is Yf - Yi);
                                (-2 is Xf - Xi, 2 is Yf - Yi);
                                (-2 is Xf - Xi, 0 is Yf - Yi);
                                (-2 is Xf - Xi, -2 is Yf - Yi))).

eliminar_caminho(T, Nome, Xf, Yf, T2) :- procurar_peca(T, Nome, Xi, Yi),
                                      XiM is Xi + 1, YiM is Yi + 1, Xim is Xi - 1, Yim is Yi - 1,
                                     (((2 is Xf - Xi, 2 is Yf - Yi), substituir(XiM, YiM, T, -1, T2, _));
                                      ((2 is Xf - Xi, 0 is Yf - Yi), substituir(XiM, Yi, T, -1, T2, _));
                                      ((2 is Xf - Xi, -2 is Yf - Yi), substituir(XiM, Yim, T, -1, T2, _));
                                      ((0 is Xf - Xi, 2 is Yf - Yi), substituir(Xi, YiM, T, -1, T2, _));
                                      ((0 is Xf - Xi, -2 is Yf - Yi), substituir(Xi, Yim, T, -1, T2, _));
                                      ((-2 is Xf - Xi, 2 is Yf - Yi), substituir(Xim, YiM, T, -1, T2, _));
                                      ((-2 is Xf - Xi, 0 is Yf - Yi), substituir(Xim, Yi, T, -1, T2, _));
                                      ((-2 is Xf - Xi, -2 is Yf - Yi), substituir(Xim, Yim, T, -1, T2, _))).

final(T):- (not(procurar_peca(T, rp, _X1, _Y1)), write('Branco Ganha!!!')) ; (not(procurar_peca(T, rb, _X2, _Y2)), write('Preto Ganha!!!')).

print(T):- between(1, 7, _N),
           nth(_N, T, Linha),
           write(Linha),
           fail.

play_aux(T):- final(T), !.
play_aux(T):- write('1 - jogar | 0 - sair'), nl,
              read(X), nl,
              (X = 1, write('jogar'), nl, read(_X), nl, read(_Y), nl, mover_peca(T, gb1, _X, _Y, T2), nl, play_aux(T2));
              (X = 0, write('sair')).

play :- inicializar_tabuleiro(T), play_aux(T).
