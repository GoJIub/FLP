solve(Assign) :-
    People = [voronov, pavlov, levitsky, sakharov],
    Roles  = [dancer, painter, singer, writer],

    % Список пар Person-Role
    Assign = [voronov-RV, pavlov-RP, levitsky-RL, sakharov-RS],
    permutation(Roles, [RV, RP, RL, RS]),

    % --- Ограничения из текста задачи ---
    
    % 1) Певец выступал, а Воронов и Левицкий сидели в зале
    RV \= singer,
    RL \= singer,

    % 2) Контакты в задаче
    %    Художник контактирует с писателем и Павловым
    Contacts = [
        painter-writer,
        painter-RP
    ],

    % 3) Павлов и писатель позировали художнику:
    RP \= painter,
    RP \= writer,

    % 4) Писатель писал о Сахарове и Воронове
    RS \= writer,
    RV \= writer,

    % 5) Воронов никогда не слышал о Левицком
    %    → роли Воронова и Левицкого не вступали в контакт
    \+ roles_contact(RV, RL, Contacts).

% Проверка контакта: contact(X,Y) или contact(Y,X)
roles_contact(R1, R2, Contacts) :-
    member(R1-R2, Contacts);
    member(R2-R1, Contacts).