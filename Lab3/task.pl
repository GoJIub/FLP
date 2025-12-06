:- dynamic counter/2.

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

inc_counter(Name) :-
    ( retract(counter(Name,V)) ->
        NV is V + 1, assertz(counter(Name,NV))
    ; assertz(counter(Name,1))
    ).

num_moves(Path, Moves) :-
    length(Path, L),
    Moves is L - 1.

initial([b,b,b,e,w,w,w]).
goal([w,w,w,e,b,b,b]).

move(State, Next) :-
    nth0(I, State, b),
    I1 is I + 1, I1 < 7,
    nth0(I1, State, e),
    replace(State, I, e, Temp),
    replace(Temp, I1, b, Next).

move(State, Next) :-
    nth0(I, State, b),
    I2 is I + 2, I3 is I + 1, I2 < 7,
    nth0(I3, State, X), X \= e,
    nth0(I2, State, e),
    replace(State, I, e, Temp),
    replace(Temp, I2, b, Next).

move(State, Next) :-
    nth0(I, State, w),
    I1 is I - 1, I1 >= 0,
    nth0(I1, State, e),
    replace(State, I, e, Temp),
    replace(Temp, I1, w, Next).

move(State, Next) :-
    nth0(I, State, w),
    I2 is I - 2, I3 is I - 1, I2 >= 0,
    nth0(I3, State, X), X \= e,
    nth0(I2, State, e),
    replace(State, I, e, Temp),
    replace(Temp, I2, w, Next).

% ---------------- DFS -----
solve_dfs(Solution) :-
    initial(Start),
    goal(Goal),
    get_time(T0),
    dfs([Start], Goal, [], RevPath),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    num_moves(Solution, Len),
    format('Time ~3f sec~n', [Time]),
    format('Path length: ~d steps~n', [Len]).

dfs([State|_], Goal, Visited, [State|Visited]) :-
    State == Goal, !.
dfs([State|Rest], Goal, Visited, Path) :-
    findall(Next, (move(State, Next), \+ member(Next, [State|Visited])), Nexts),
    append(Nexts, Rest, NewStack),
    dfs(NewStack, Goal, [State|Visited], Path).

% ---------------- BFS ----------------
solve_bfs(Solution) :-
    initial(Start),
    goal(Goal),
    get_time(T0),
    bfs([[Start]], Goal, [], RevPath),
    reverse(RevPath, Solution),
    get_time(T1),
    Time is T1 - T0,
    num_moves(Solution, Len),
    format('Time ~3f sec~n', [Time]),
    format('Path length: ~d steps~n', [Len]).

bfs([[State|Path]|_], Goal, _, [State|Path]) :-
    State == Goal, !.
bfs([[State|Path]|RestPaths], Goal, Visited, Solution) :-
    findall([Next,State|Path], (move(State, Next), \+ member(Next, Visited)), NewPaths),
    append(RestPaths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, Goal, [State|Visited], Solution).

% ---------------- IDDFS ----------------
solve_iddfs(Path) :-
    initial(Start),
    goal(Goal),
    get_time(T1),
    iddfs_main(Start, Goal, Path),
    get_time(T2), Time is T2 - T1,
    num_moves(Path, Len),
    format('Time ~3f sec~n', [Time]),
    format('Path length: ~d steps~n', [Len]).

iddfs_main(Start, Goal, Path) :-
    between(0, 50, D),
    dls_search(Start, Goal, D, D, [Start], RevPath),
    !,
    reverse(RevPath, Path).

dls_search(Goal, Goal, _, _, Acc, Acc) :- !.
dls_search(Start, Goal, D, D, Visited, Result) :-
    D > 0,
    inc_counter(nodes_expanded_iddfs),
    move(Start, Next),
    \+ member(Next, Visited),
    D1 is D - 1,
    dls_search(Next, Goal, D1, D1, [Next|Visited], Result).
