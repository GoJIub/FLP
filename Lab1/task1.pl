% length_my(List, Len)
length_my([], 0).
length_my([_|T], N) :-
    length_my(T, N1),
    N is N1 + 1.

% member_my(Elem, List)
member_my(X, [X|_]).
member_my(X, [_|T]) :-
    member_my(X, T).

% append_my(List1, List2, Result)
append_my([], L, L).
append_my([H|T], L2, [H|R]) :-
    append_my(T, L2, R).

% remove_my(Elem, List, Result)
remove_my(_, [], []).
remove_my(X, [X|T], T) :- !.
remove_my(X, [H|T], [H|R]) :-
    remove_my(X, T, R).

% select_my(Elem, List, Rest)
select_my(X, [X|T], T).
select_my(X, [H|T], [H|R]) :-
    select_my(X, T, R).

% permute_my(List, Perm)
permute_my([], []).
permute_my(L, [H|T]) :-
    select_my(H, L, R),
    permute_my(R, T).

% sublist_my(Sub, List)
sublist_my(Sub, List) :-
    append_my(_, Tail, List),
    append_my(Sub, _, Tail),
    Sub \= [].

% --- Удаление элемента с заданным номером ---
% Вариант 1: на основе append_my/length_my
delete_at_std(Index, List, Result) :-
    Index >= 1,
    % найдем префикс длины Index-1
    Index1 is Index - 1,
    length_prefix(Index1, Prefix),
    append_my(Prefix, Suffix, List),
    Suffix = [_Elem|RestSuffix],
    append_my(Prefix, RestSuffix, Result).

% вспомогательный предикат: создаёт список-фиктивный префикс заданной длины
length_prefix(0, []).
length_prefix(N, [_|T]) :-
    N > 0,
    N1 is N - 1,
    length_prefix(N1, T).

% Вариант 2: рекурсивно, без использования append/length
delete_at_rec(1, [_|T], T) :- !.
delete_at_rec(K, [H|T], [H|R]) :-
    K > 1,
    K1 is K - 1,
    delete_at_rec(K1, T, R).

% --- Позиция первого отрицательного элемента ---
% Вариант 1: на основе append_my/length_my
first_neg_std(List, Pos) :-
    append_my(Prefix, [H|_], List),
    H < 0,
    length_my(Prefix, L),
    Pos is L + 1,
    !.

% Вариант 2: рекурсивно
first_neg_rec(List, Pos) :-
    first_neg_rec_acc(List, 1, Pos).

first_neg_rec_acc([], _) :-
    fail.
first_neg_rec_acc([H|_], K, K) :-
    H < 0, !.
first_neg_rec_acc([_|T], K, Pos) :-
    K1 is K + 1,
    first_neg_rec_acc(T, K1, Pos).