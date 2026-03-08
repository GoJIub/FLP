:- encoding(utf8).

% Словари
q_list(['Кто','Что','Где']).

v_list([
    ['любит','любить'],
    ['лежат','лежать']
]).

n_list(['шоколад','деньги']).

name_dict([
    'Даша'
]).

% Нормализация входных данных
normalize_words([], []).
normalize_words([H|T], [A|T2]) :-
    to_atom(H, A),
    normalize_words(T, T2).

to_atom(A, A) :- atom(A), !.
to_atom(S, A) :- string(S), atom_string(A, S).

% Анализаторы терминальных символов
an_qw([X], qw(X)) :-
    q_list(L),
    member(X, L).

an_v([X], v(V)) :-
    v_list(L),
    member([X, V], L), !.

an_n([X], n(X)) :-
    n_list(L),
    member(X, L).

an_name([X], name(X)) :-
    name_dict(L),
    member(X, L).

is_question_word(X) :-
    q_list(L),
    member(X, L).

is_verb(X) :-
    v_list(L),
    member([X,_], L).

is_noun(X) :-
    n_list(L),
    member(X, L).

% Предикат решения
an_q(RawWords, Res) :-
    normalize_words(RawWords, Words),
    append(WordsNoQ, ['?'], Words),
    an_q_phrase(WordsNoQ, Res), !.

% Правила разбора вопросительных фраз
an_q_phrase(L, Res) :-
    append(P1, P2, L),
    an_qw(P1, qw('Кто')),
    append(P3, P4, P2),
    an_v(P3, v(V)),
    an_n(P4, n(Obj)),
    Res =.. [V, agent(_), object(Obj)].

an_q_phrase(L, Res) :-
    append(P1, P2, L),
    an_qw(P1, qw('Что')),
    append(P3, P4, P2),
    an_v(P3, v(V)),
    an_name(P4, name(Name)),
    Res =.. [V, agent(Name), object(_)].

an_q_phrase(L, Res) :-
    append(P1, P2, L),
    an_qw(P1, qw('Где')),
    append(P3, P4, P2),
    an_v(P3, v(V)),
    an_n(P4, n(Obj)),
    Res =.. [V, object(Obj), loc(_)].
