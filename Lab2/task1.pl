roles_contact(R1, R2, Contacts) :-
    member(R1-R2, Contacts);
    member(R2-R1, Contacts).

all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

solve(Solution) :-
    RV_range = [dancer, painter],
    RP_range = [dancer, singer],
    RL_range = [dancer, painter, writer],
    RS_range = [dancer, painter, singer],

    Solution = [voronov-RV, pavlov-RP, levitsky-RL, sakharov-RS],

    member(RV, RV_range),
    member(RP, RP_range),
    member(RL, RL_range),
    member(RS, RS_range),

    all_different([RV, RP, RL, RS]),

    Contacts = [painter-writer, painter-RP],
    \+ roles_contact(RV, RL, Contacts).

% solve_with_permutation(Solution) :-
%     People = [voronov, pavlov, levitsky, sakharov],
%     Roles  = [dancer, painter, singer, writer],

%     Solution = [voronov-RV, pavlov-RP, levitsky-RL, sakharov-RS],
%     permutation(Roles, [RV, RP, RL, RS]),
    
%     RV \= singer,
%     RL \= singer,

%     Contacts = [
%         painter-writer,
%         painter-RP
%     ],

%     RP \= painter,
%     RP \= writer,

%     RS \= writer,
%     RV \= writer,

%     \+ roles_contact(RV, RL, Contacts).
