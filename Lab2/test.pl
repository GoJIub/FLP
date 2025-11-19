% talented_contact.pl
% Решение задачи о талантливых людях.
% contact(PersonA, PersonB) выводится через роли + таблицу role_contact/2 и role_person_contact/2.

:- module(talented_contact, [solve/1, contact/2]).

% --- role-contact таблица (контакты между ролями) ---
% painter контактирует с writer (художник работал с писателем)
role_contact(painter, writer).

% --- role-person контакты (когда роль контактирует с конкретным человеком) ---
% художеник (painter) контактировал с Павловым (Павлов позировал художнику)
role_person_contact(painter, pavlov).

% --- контакт на уровне людей (симметричен) ---
% Контакт истинный, если:
%  - роль PersonA контактирует с ролью PersonB (role_contact)
%  - или роль PersonA контактирует с конкретным PersonB (role_person_contact)
%  - или роль PersonB контактирует с конкретным PersonA
contact(A, B) :-
    person_role(A, RA),
    person_role(B, RB),
    ( role_contact(RA, RB)
    ; role_contact(RB, RA)
    ; role_person_contact(RA, B)
    ; role_person_contact(RB, A)
    ),
    A \= B.   % исключаем тривиальный контакт с самим собой

% person_role/2 — вспомогательный предикат, объявляется динамически в solve/1
% (мы не фиксируем роли глобально — они будут заданы локально для конкретного решения)

% Основной предикат: вычисляет Assign = [voronov-RoleV, pavlov-RoleP, levitsky-RoleL, sakharov-RoleS]
solve(Assign) :-
    Roles = [dancer, painter, singer, writer],

    % Переменные ролей
    Assign = [voronov-RV, pavlov-RP, levitsky-RL, sakharov-RS],

    % Перебираем все перестановки ролей
    permutation(Roles, [RV, RP, RL, RS]),

    % Ограничения (текст задачи):
    % 1) Воронов и Левицкий сидели в зале, когда певец дебютировал
    RV \= singer,
    RL \= singer,

    % 2) Павлов и писатель вместе позировали художнику.
    %    => Павлов ≠ painter (он позировал), Павлов ≠ writer (он — другой человек)
    RP \= painter,
    RP \= writer,

    % 3) Писатель написал биографию о Сахарове и собирается написать о Воронове.
    %    => писатель ≠ sakharov, писатель ≠ voronov
    RS \= writer,
    RV \= writer,

    % Установим отношения person_role/2 локально (используется contact/2)
    % Чтобы contact/2 мог использовать текущие роли, мы объявим временные факты через
    % вспомогательный предикат person_role/2 (он связан с текущими переменными).
    asserta_person_roles([voronov-RV, pavlov-RP, levitsky-RL, sakharov-RS]),

    % 4) "Воронов никогда не слышал о Левицком" — моделируем как отсутствие контакта
    \+ contact(voronov, levitsky),

    % очищаем временные факты (чтобы не засорять БД при backtrack/повторных вызовах)
    retract_person_roles,
    !.  % один-единственный (детерминированный) ответ — можно убрать, если хотим все варианты

% Вспомогательные предикаты для временных person_role фактов
:- dynamic person_role/2.

asserta_person_roles([]).
asserta_person_roles([Name-Role | T]) :-
    assertz(person_role(Name, Role)),   % assertz чтобы найти их потом (можно assertz или asserta)
    asserta_person_roles(T).

% Удаляем все имевшиеся person_role факты (используется после решения или при backtrack)
retract_person_roles :-
    retractall(person_role(_, _)).
