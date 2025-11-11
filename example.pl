% ============================================================================
% Finite State Automata Implementation in Prolog
% Purely declarative implementation (except I/O)
% ============================================================================

% ----------------------------------------------------------------------------
% Data Structure Representation
% ----------------------------------------------------------------------------
% An FSA is represented as a compound term:
% fsa(States, Alphabet, Transitions, StartState, AcceptStates)
%
% States: list of state identifiers
% Alphabet: list of input symbols (not including epsilon)
% Transitions: list of trans(FromState, Symbol, ToState)
%              Symbol can be epsilon for empty transitions
% StartState: single state identifier
% AcceptStates: list of accepting state identifiers

% Example FSA: accepts (a|b)*abb
% This is the NFA from the specification
example_fsa(
    fsa([0,1,2,3,4,5,6,7,8,9,10],
        [a,b],
        [trans(0,epsilon,1), trans(0,epsilon,7),
         trans(1,epsilon,2), trans(1,epsilon,4),
         trans(2,a,3), trans(4,b,5),
         trans(3,epsilon,6), trans(5,epsilon,6),
         trans(6,epsilon,1), trans(6,epsilon,7),
         trans(7,a,8), trans(8,b,9), trans(9,b,10)],
        0,
        [10])
).

% ----------------------------------------------------------------------------
% Operation 1: addState
% Adds a new state to the FSA
% addState(+FSA, +NewState, +IsStart, +IsAccept, -NewFSA)
% ----------------------------------------------------------------------------
addState(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
         NewState, IsStart, IsAccept,
         fsa(NewStates, Alphabet, Transitions, NewStartState, NewAcceptStates)) :-
    % Add state to states list if not already present
    (member(NewState, States) -> NewStates = States ; NewStates = [NewState|States]),
    % Update start state if IsStart is true
    (IsStart = true -> NewStartState = NewState ; NewStartState = StartState),
    % Add to accept states if IsAccept is true
    (IsAccept = true -> 
        (member(NewState, AcceptStates) -> NewAcceptStates = AcceptStates ; 
         NewAcceptStates = [NewState|AcceptStates])
    ; NewAcceptStates = AcceptStates).

% ----------------------------------------------------------------------------
% Operation 2: addTransition
% Adds a new transition to the FSA
% addTransition(+FSA, +FromState, +Symbol, +ToState, -NewFSA)
% ----------------------------------------------------------------------------
addTransition(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
              FromState, Symbol, ToState,
              fsa(States, NewAlphabet, NewTransitions, StartState, AcceptStates)) :-
    % Verify states exist
    member(FromState, States),
    member(ToState, States),
    % Add transition
    NewTransitions = [trans(FromState, Symbol, ToState)|Transitions],
    % Update alphabet if symbol is not epsilon and not already present
    (Symbol = epsilon -> NewAlphabet = Alphabet ;
     (member(Symbol, Alphabet) -> NewAlphabet = Alphabet ; 
      NewAlphabet = [Symbol|Alphabet])).

% ----------------------------------------------------------------------------
% Operation 3: closure
% Returns the epsilon-closure of a given state
% closure(+FSA, +State, -ClosureSet)
% ----------------------------------------------------------------------------
closure(fsa(_, _, Transitions, _, _), State, ClosureSet) :-
    epsilon_closure([State], [], Transitions, ClosureSet).

% Helper: compute epsilon closure from a set of states
epsilon_closure([], Visited, _, Visited).
epsilon_closure([State|Rest], Visited, Transitions, ClosureSet) :-
    member(State, Visited), !,
    epsilon_closure(Rest, Visited, Transitions, ClosureSet).
epsilon_closure([State|Rest], Visited, Transitions, ClosureSet) :-
    % Find all states reachable via epsilon from State
    findall(ToState, member(trans(State, epsilon, ToState), Transitions), EpsilonStates),
    append(EpsilonStates, Rest, NewStates),
    epsilon_closure(NewStates, [State|Visited], Transitions, ClosureSet).

% ----------------------------------------------------------------------------
% Operation 4: next
% Returns states reachable from given state on given input symbol
% Includes epsilon closure before and after the transition
% next(+FSA, +State, +Symbol, -NextStates)
% ----------------------------------------------------------------------------
next(FSA, State, Symbol, NextStates) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % Get epsilon closure of starting state
    closure(FSA, State, ClosureOfState),
    % Find all states reachable via Symbol from closure
    findall(ToState, 
            (member(FromState, ClosureOfState),
             member(trans(FromState, Symbol, ToState), Transitions)),
            DirectStates),
    % Get epsilon closure of all reached states
    compute_closure_set(FSA, DirectStates, NextStates).

% Helper: compute epsilon closure of a set of states
compute_closure_set(_, [], []).
compute_closure_set(FSA, [State|Rest], Result) :-
    closure(FSA, State, StateClosure),
    compute_closure_set(FSA, Rest, RestClosure),
    union(StateClosure, RestClosure, Result).

% ----------------------------------------------------------------------------
% Operation 5: accepts
% Determines if FSA accepts the given input string
% accepts(+FSA, +String)
% ----------------------------------------------------------------------------
accepts(FSA, String) :-
    fsa(_, _, _, StartState, AcceptStates) = FSA,
    closure(FSA, StartState, InitialStates),
    process_string(FSA, InitialStates, String, FinalStates),
    % Check if any final state is an accepting state
    member(AcceptState, AcceptStates),
    member(AcceptState, FinalStates).

% Process input string symbol by symbol
process_string(_, CurrentStates, [], CurrentStates).
process_string(FSA, CurrentStates, [Symbol|Rest], FinalStates) :-
    % Get all next states from current states on Symbol
    findall(NextState,
            (member(State, CurrentStates),
             next(FSA, State, Symbol, NextSet),
             member(NextState, NextSet)),
            AllNextStates),
    list_to_set(AllNextStates, NextStates),
    process_string(FSA, NextStates, Rest, FinalStates).

% ----------------------------------------------------------------------------
% Operation 6: deterministic
% Checks if FSA is deterministic (DFA)
% deterministic(+FSA)
% ----------------------------------------------------------------------------
deterministic(fsa(States, _, Transitions, _, _)) :-
    % No epsilon transitions
    \+ member(trans(_, epsilon, _), Transitions),
    % Each state has at most one transition per symbol
    forall(member(State, States),
           check_state_deterministic(State, Transitions)).

% Check if a state has at most one transition per symbol
check_state_deterministic(State, Transitions) :-
    findall(Symbol-ToState, member(trans(State, Symbol, ToState), Transitions), Pairs),
    check_unique_symbols(Pairs).

% Verify each symbol appears at most once
check_unique_symbols([]).
check_unique_symbols([Symbol-_|Rest]) :-
    \+ member(Symbol-_, Rest),
    check_unique_symbols(Rest).

% ----------------------------------------------------------------------------
% Operation 7: toDFA
% Converts NFA to equivalent DFA using subset construction
% toDFA(+NFA, -DFA)
% ----------------------------------------------------------------------------
toDFA(NFA, DFA) :-
    fsa(_, Alphabet, _, StartState, NFAAcceptStates) = NFA,
    % Start with epsilon closure of start state
    closure(NFA, StartState, InitialDFAState),
    % Build DFA states using subset construction
    subset_construction(NFA, [InitialDFAState], [], Alphabet, DFAStates, DFATransitions),
    % Determine which DFA states are accepting
    findall(DFAState,
            (member(DFAState, DFAStates),
             member(NFAState, DFAState),
             member(NFAState, NFAAcceptStates)),
            DFAAcceptStates),
    list_to_set(DFAAcceptStates, DFAAcceptStatesSet),
    % Create DFA
    DFA = fsa(DFAStates, Alphabet, DFATransitions, InitialDFAState, DFAAcceptStatesSet).

% Subset construction algorithm
subset_construction(_, [], Visited, _, Visited, []).
subset_construction(NFA, [CurrentSet|Unprocessed], Visited, Alphabet, AllStates, AllTransitions) :-
    member(CurrentSet, Visited), !,
    subset_construction(NFA, Unprocessed, Visited, Alphabet, AllStates, AllTransitions).
subset_construction(NFA, [CurrentSet|Unprocessed], Visited, Alphabet, AllStates, AllTransitions) :-
    % For each symbol, compute next state set
    findall(Symbol-NextSet,
            (member(Symbol, Alphabet),
             compute_next_set(NFA, CurrentSet, Symbol, NextSet),
             NextSet \= []),
            SymbolNextPairs),
    % Extract new states and transitions
    findall(NextSet, member(_-NextSet, SymbolNextPairs), NewSets),
    findall(trans(CurrentSet, Symbol, NextSet), member(Symbol-NextSet, SymbolNextPairs), NewTransitions),
    % Add new states to unprocessed
    append(NewSets, Unprocessed, UpdatedUnprocessed),
    append(NewTransitions, RestTransitions, AllTransitions),
    % Recursively process remaining states
    subset_construction(NFA, UpdatedUnprocessed, [CurrentSet|Visited], Alphabet, AllStates, RestTransitions).

% Compute next state set for a DFA state (which is a set of NFA states) on a symbol
compute_next_set(NFA, StateSet, Symbol, NextSet) :-
    findall(NextState,
            (member(State, StateSet),
             next(NFA, State, Symbol, ReachableStates),
             member(NextState, ReachableStates)),
            AllNextStates),
    list_to_set(AllNextStates, NextSet).

% ----------------------------------------------------------------------------
% Utility Predicates
% ----------------------------------------------------------------------------

% Set union (removes duplicates)
union([], L, L).
union([H|T], L, Result) :-
    member(H, L), !,
    union(T, L, Result).
union([H|T], L, [H|Result]) :-
    union(T, L, Result).

% Convert list to set (remove duplicates)
list_to_set([], []).
list_to_set([H|T], [H|Rest]) :-
    delete_all(T, H, T1),
    list_to_set(T1, Rest).

% Delete all occurrences of element from list
delete_all([], _, []).
delete_all([H|T], H, Result) :- !,
    delete_all(T, H, Result).
delete_all([H|T], X, [H|Result]) :-
    delete_all(T, X, Result).

% ----------------------------------------------------------------------------
% Test Queries (examples)
% ----------------------------------------------------------------------------

% Test closure operation
test_closure :-
    example_fsa(FSA),
    closure(FSA, 3, Closure),
    write('Closure of state 3: '), write(Closure), nl.

% Test next operation
test_next :-
    example_fsa(FSA),
    next(FSA, 4, b, NextStates),
    write('Next states from 4 on b: '), write(NextStates), nl.

% Test accepts operation
test_accepts :-
    example_fsa(FSA),
    (accepts(FSA, [a,b,b]) -> write('Accepts "abb": yes') ; write('Accepts "abb": no')), nl,
    (accepts(FSA, [a,a,b,b]) -> write('Accepts "aabb": yes') ; write('Accepts "aabb": no')), nl,
    (accepts(FSA, [a,b,a]) -> write('Accepts "aba": yes') ; write('Accepts "aba": no')), nl.

% Test deterministic check
test_deterministic :-
    example_fsa(FSA),
    (deterministic(FSA) -> write('FSA is deterministic') ; write('FSA is non-deterministic')), nl.

% Run all tests
run_tests :-
    write('=== Testing Closure ==='), nl,
    test_closure, nl,
    write('=== Testing Next ==='), nl,
    test_next, nl,
    write('=== Testing Accepts ==='), nl,
    test_accepts, nl,
    write('=== Testing Deterministic ==='), nl,
    test_deterministic, nl.

% Example usage:
% ?- run_tests.