% task2.pl
% Лабораторная работа: функционально-логическое программирование
% Часть 2. Реляционное представление предметной области
% Данные о студентах и их оценках + вычисления по заданиям:
%  1. Средний балл по каждому предмету
%  2. Количество не сдавших студентов по каждой группе
%  3. Количество не сдавших студентов по каждому предмету

% ------------------------------------------------------------
% Исходные данные в two.pl

% ------------------------------------------------------------
% Порог сдачи
pass_mark(3).

% ------------------------------------------------------------
% Вспомогательная сумма элементов списка
sum_list(List, Sum) :- sum_list(List, 0, Sum).
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Sum) :-
    Acc1 is Acc + H,
    sum_list(T, Acc1, Sum).

% ------------------------------------------------------------
% 1️⃣ Средний балл по каждому предмету
average_subject(Subject, Avg) :-
    findall(Mark, grade(_, _, Subject, Mark), Marks),
    Marks \= [],
    sum_list(Marks, Sum),
    length(Marks, Count),
    Avg is Sum / Count.

print_average_subjects :-
    setof(Subject, G^S^M^grade(G, S, Subject, M), Subjects),
    forall(member(Subj, Subjects),
           (average_subject(Subj, Avg),
            format('~w — средний балл: ~2f~n', [Subj, Avg]))).

% ------------------------------------------------------------
% 2️⃣ Количество не сдавших студентов по каждой группе
not_passed(Group, Student) :-
    pass_mark(P),
    grade(Group, Student, _, Mark),
    Mark < P.

count_not_passed_group(Group, Count) :-
    findall(Student, not_passed(Group, Student), L),
    sort(L, Unique),
    length(Unique, Count).

print_not_passed_groups :-
    setof(G, S^Subj^M^grade(G, S, Subj, M), Groups),
    forall(member(G, Groups),
           (count_not_passed_group(G, Count),
            format('Группа ~w — не сдали: ~w студентов~n', [G, Count]))).

% ------------------------------------------------------------
% 3️⃣ Количество не сдавших студентов по каждому предмету
count_not_passed_subject(Subject, Count) :-
    pass_mark(P),
    findall(Student, (grade(_, Student, Subject, Mark), Mark < P), L),
    sort(L, Unique),
    length(Unique, Count).

print_not_passed_subjects :-
    setof(Subj, G^S^M^grade(G, S, Subj, M), Subjects),
    forall(member(Subj, Subjects),
           (count_not_passed_subject(Subj, Count),
            format('Предмет "~w" — не сдали: ~w студентов~n', [Subj, Count]))).

% ------------------------------------------------------------
% Примеры использования:
% ?- print_average_subjects.
% ?- print_not_passed_groups.
% ?- print_not_passed_subjects.
