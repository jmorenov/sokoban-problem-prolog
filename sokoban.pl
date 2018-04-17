:-include(initial_state).
:-include(depth_first_solver).

final_state(state(_Sokoban, Boxes)) :-
    solutions_found(Boxes), !.

solutions_found([]).
solutions_found([Box|Boxes]) :-
    solution(Box),
    solutions_found(Boxes).

adj(P1, P2, up) :- top(P1, P2).
adj(P1, P2, down) :- top(P2, P1).
adj(P1, P2, right) :- right(P1, P2).
adj(P1, P2, left) :- right(P2, P1).

corner(X) :- \+ noncorner(X).
noncorner(X) :- top(_,X),top(X,_).
noncorner(X) :- right(_,X),right(X,_).

movement(state(Sokoban, Boxes), move(Box, Dir)) :-
    select(Box, Boxes, BoxesRemain),
    adj(Box, NextLoc, Dir),
    \+ member(NextLoc,BoxesRemain),
    \+ stuck(NextLoc),
    foreach(member(Box, BoxesRemain), \+ stuck(NextLoc, Box)),
    adj(PushPosition, Box, Dir),
    can_reach(Sokoban, PushPosition, Boxes, []),
    \+ member(PushPosition, Boxes).

update(state(_Sokoban, Boxes), move(Box, Dir), state(NewSokoban, NewBoxes)) :-
    NewSokoban = Box,
    subtract(Boxes,[Box],TempList),
    adj(Box, NewPosition, Dir),
    append(TempList, [NewPosition], NewBoxes).

stuck(X) :-
    \+ solution(X),
    corner(X).

stuck(X, Y) :-
    (right(X,Y); right(Y,X)),
    (\+ solution(X); \+ solution(Y)),
    (\+ top(X,_); \+ top(_,X)),
    (\+ top(Y,_); \+ top(_,Y)).

stuck(X, Y) :-
    (top(X,Y); top(Y,X)),
    (\+ solution(X); \+ solution(Y)),
    (\+ right(X,_); \+ right(_,X)),
    (\+ right(Y,_); \+ right(_,Y)).

can_reach(P1, P1, _Boxes, _Visited).
can_reach(P1, P2, Boxes, _Visited) :-
    adj(P1, P2, _),
    \+ member(P2, Boxes).
can_reach(P1, P2, Boxes, Visited) :-
    adj(P1, P3, _),
    P3 \== P2,
    \+ member(P3, Visited),
    \+ member(P3, Boxes),
    can_reach(P3, P2, Boxes, [P3|Visited]).

solve_problem(Solution) :-
    initial_state(Initial),
    solve_dfs(Initial, [Initial], Solution).
