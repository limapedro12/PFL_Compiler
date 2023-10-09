:-use_module(library(lists)).

inicializar_tabuleiro(T) :- T = [[0,   0, 0,  0, 0,   0, 0],
                                 [0, gp1, 0, rp, 0, gp2, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0,   0, 0,  0, 0,   0, 0],
                                 [0, gb1, 0, rb, 0, gb2, 0],
                                 [0,   0, 0,  0, 0,   0, 0]].

substituir(Pos, List, NewElem, ListFinal, OldElem) :- nth1(Pos, List, OldElem, _R),
                                                      nth1(Pos, ListFinal, NewElem, _R).

mover_peca(T, Xi, Yi, Xf, Yf, T2) :-   nth1(Yi, T, LinhaInicial),
                                        substituir(Xi, LinhaInicial, 0, LinhaInicialTemp, Peca),
                                        substituir(Yi, T, LinhaInicialTemp, T3, _X1),
                                        nth1(Yf, T3, LinhaFinal),
                                        substituir(Xf, LinhaFinal, Peca, LinhaFinalTemp, _X2),
                                        substituir(Yf, T3, LinhaFinalTemp, T4, _X3),
                                        T2 = T4.
                                     

play :- write('1 - jogar | 0 - sair'),nl,
        read(_X),nl,
        write(_X).
