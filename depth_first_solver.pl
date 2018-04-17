solve_dfs(State, _History, []) :-
    final_state(State).

solve_dfs(State, History, [Move|Moves]) :-
    movement(State, Move),
    update(State, Move, NewState),
    \+ member(NewState, History),
    solve_dfs(NewState, [NewState|History], Moves).