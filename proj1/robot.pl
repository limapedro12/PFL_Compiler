connects_bfs(S, F):-
    connects_bfs([S], F, [S]).
connects_bfs([F|_], F, _V).
connects_bfs([S|R], F, V):-
    findall(N,
    ( connected(S, N),
    not(member(N, V)),
    not(member(N, [S|R]))), L),
    append(R, L, NR),
    connects_bfs(NR, F, [S|V]).
