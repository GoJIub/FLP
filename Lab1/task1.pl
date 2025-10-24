% task1.pl
% Предикаты для работы со списками.
% Реализованы: length_my/2, member_my/2, append_my/3, remove_my/3 (удаление первого вхождения),
% select_my/3, permute_my/2, sublist_my/2 (континуальный подсписок).
% Дополнительно: два варианта удаления по индексу (1-based):
%   delete_at_std(Index, List, Result) - на основе стандартных предикатов (append_my, length_my)
%   delete_at_rec(Index, List, Result) - рекурсивно без использования append_my/length_my
% И два варианта поиска позиции первого отрицательного:
%   first_neg_std(List, Pos) - на основе append/length
%   first_neg_rec(List, Pos) - рекурсивно с аккумулятором

% -----------------------
% length_my(List, Len)
length_my([], 0).
length_my([_|T], N) :-
    length_my(T, N1),
    N is N1 + 1.

% -----------------------
% member_my(Elem, List)
member_my(X, [X|_]).
member_my(X, [_|T]) :-
    member_my(X, T).

% -----------------------
% append_my(List1, List2, Result)
append_my([], L, L).
append_my([H|T], L2, [H|R]) :-
    append_my(T, L2, R).

% -----------------------
% remove_my(Elem, List, Result) - удаляет первое вхождение Elem
remove_my(_, [], []).
remove_my(X, [X|T], T) :- !.
remove_my(X, [H|T], [H|R]) :-
    remove_my(X, T, R).

% -----------------------
% select_my(Elem, List, Rest) - полезен для permute
select_my(X, [X|T], T).
select_my(X, [H|T], [H|R]) :-
    select_my(X, T, R).

% -----------------------
% permute_my(List, Perm) - все перестановки
permute_my([], []).
permute_my(L, [H|T]) :-
    select_my(H, L, R),
    permute_my(R, T).

% -----------------------
% sublist_my(Sub, List) - континуальный (смежный) подсписок
sublist_my(Sub, List) :-
    append_my(_, Tail, List),
    append_my(Sub, _, Tail),
    Sub \= [].

% -----------------------
% Удаление элемента с заданным номером (1-based)
% Вариант 1: на основе append_my/length_my (стандартные предикаты)
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

% -----------------------
% Позиция первого отрицательного элемента (1-based)
% Вариант 1: на основе append_my/length_my
first_neg_std(List, Pos) :-
    append_my(Prefix, [H|_], List),
    H < 0,
    length_my(Prefix, L),
    Pos is L + 1,
    !.

% Вариант 2: рекурсивно с аккумулятором
first_neg_rec(List, Pos) :-
    first_neg_rec_acc(List, 1, Pos).

first_neg_rec_acc([], _) :-
    % не найдено, задача может требовать failure; здесь завершаемся с false
    fail.
first_neg_rec_acc([H|_], K, K) :-
    H < 0, !.
first_neg_rec_acc([_|T], K, Pos) :-
    K1 is K + 1,
    first_neg_rec_acc(T, K1, Pos).

% -----------------------
% Примеры запросов (вводите в интерактивной сессии):
% ?- length_my([a,b,c], L).             % L = 3.
% ?- member_my(x, [a,x,b]).             % true.
% ?- append_my([1,2], [3,4], R).        % R = [1,2,3,4].
% ?- remove_my(2, [1,2,3,2], R).        % R = [1,3,2].
% ?- permute_my([1,2,3], P).            % перебирает все перестановки.
% ?- sublist_my([2,3], [1,2,3,4]).      % true.
% ?- delete_at_std(3, [a,b,c,d], R).    % R = [a,b,d].
% ?- delete_at_rec(1, [x,y], R).        % R = [y].
% ?- first_neg_std([1,2,-3,4], P).      % P = 3.
% ?- first_neg_rec([1,2,-3,4], P).      % P = 3.