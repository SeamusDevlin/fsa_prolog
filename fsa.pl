% addState
% adds a new state to the FSA
% IsStart and IsAccepts are booleans indicating if the new state is a start or accept state
% The new state is added to the state lists, if IsStart is true, it will replace current start
% same with the isAccept

addState(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
         NewState, IsStart, IsAccept,
         fsa(NewStates, Alphabet, Transitions, NewStartState, NewAcceptStates)) :-
    (   member(NewState, States)
    ->  NewStates = States  % State already exists, no change
    ;   NewStates = [NewState | States] % Add new state
    ),

    (   IsStart == true % If new state is to be start state
    ->  NewStartState = NewState % Update start state
    ;   NewStartState = StartState % No change
    ),

    (   IsAccept == true % If new state is to be accept state
    ->  (  member(NewState, AcceptStates)
        -> NewAcceptStates = AcceptStates % Already an accept state
        ;  NewAcceptStates = [NewState | AcceptStates]  % Add to accept states
        )
    ;    NewAcceptStates = AcceptStates % No change
    ).

% addTransition
% adds a new transition to the FSA

addTransition(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
              FromState, Symbol, ToState,
              fsa(States, NewAlphabet, NewTransitions, StartState, AcceptStates)) :-
              % Check if FromState and ToState exist
              member(FromState, States),
              member(ToState, States),
              % Add transition
              NewTransitions = [trans(FromState, Symbol, ToState) | Transitions],
              % Update alphabet if necessary
              (Symbol = epsilon -> NewAlphabet = Alphabet ;
                (member(Symbol, Alphabet) -> NewAlphabet = Alphabet ;
                 NewAlphabet = [Symbol | Alphabet])).

% accepts
% checks if the FSA accepts the given string

accepts(FSA, String) :-
    fsa(_, _, _, StartState, AcceptStates) = FSA,
    % Get initial states
    epsilon_closure(FSA, [StartState], InitialStates),
    % Process the input string
    process_string(FSA, InitialStates, String, FinalStates),
    % Check if any of the final states is an accept state
    member(AcceptState, AcceptStates),
    member(AcceptState, FinalStates).

% process input string symbol by symbol
process_string(_, CurrentStates, [], CurrentStates). % No more input, return current states

process_string(FSA, CurrentStates, [Symbol | Rest], FinalStates) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % Find all states reachable from CurrentStates on Symbol
    findall(ToState,
        (   member(FromState, CurrentStates),
            member(trans(FromState, Symbol, ToState), Transitions)
        ),
        NextStatesUnclosed),
    % Get epsilon closure of all reached states
    epsilon_closure_set(FSA, NextStatesUnclosed, NextStates),
    % Continue processing rest of the symbols
    process_string(FSA, NextStates, Rest, FinalStates).

% Helper wrapper for epsilon_closure (used by accepts and process_string)
epsilon_closure(FSA, StateList, ClosureSet) :-
    epsilon_closure_set(FSA, StateList, ClosureSet).

% closure
% returns the epsilon closure of a set of states reachable via epsilon transitions
% using depth-first search

closure(FSA, State, ClosureSet) :-
    epsilon_closure_set(FSA, [State], ClosureSet).

% Compute epsilon-closure for a set of states
epsilon_closure_set(FSA, StateSet, ClosureSet) :-
    epsilon_closure_helper(FSA, StateSet, [], ClosureSet).

% Helper predicate: accumulator-based epsilon-closure computation
epsilon_closure_helper(_, [], Visited, Visited).

epsilon_closure_helper(FSA, [State|Rest], Visited, ClosureSet) :-
    member(State, Visited),
    !,
    epsilon_closure_helper(FSA, Rest, Visited, ClosureSet).

epsilon_closure_helper(FSA, [State|Rest], Visited, ClosureSet) :-
    fsa(_, _, Transitions, _, _) = FSA,
    % Find all states reachable via epsilon from State
    findall(ToState,
            member(trans(State, epsilon, ToState), Transitions),
            EpsilonReachable),
    % Add newly discovered states to process
    append(EpsilonReachable, Rest, UpdatedRest),
    % Continue with State added to visited set
    epsilon_closure_helper(FSA, UpdatedRest, [State|Visited], ClosureSet).

% next
% returns the set of states reachable from a set of states on a given input symbol
% symbol must not be epsilon

next(FSA, State, Symbol, NextStates) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % Get epsilon-closure of starting state
    epsilon_closure_set(FSA, [State], ClosureOfState),
    % Find all states reachable via Symbol
    findall(ToState,
            (   member(FromState, ClosureOfState),
                member(trans(FromState, Symbol, ToState), Transitions)
            ),
            DirectStates),
    % Get epsilon-closure of result
    epsilon_closure_set(FSA, DirectStates, NextStates).

% deterministic
% checks if the FSA is deterministic meaning no state has two transitions for the same symbol
% violated if there exists a state with two transitions on the same symbol (excluding epsilon)

deterministic(fsa(States, _, Transitions, _, _)) :-
    % Rule 1: No epsilon transitions allowed
    \+ member(trans(_, epsilon, _), Transitions),
    % Rule 2: Each (state, symbol) pair has at most one transition
    forall(member(State, States),
           is_state_deterministic(State, Transitions)).

% Check if a single state satisfies determinism constraint
is_state_deterministic(State, Transitions) :-
    % Get all transitions from this state
    findall(Symbol-ToState,
            member(trans(State, Symbol, ToState), Transitions),
            Pairs),
    % Verify no symbol appears more than once
    check_unique_symbols(Pairs).

% Verify each symbol appears at most once in transitions
check_unique_symbols([]).
check_unique_symbols([Symbol-_|Rest]) :-
    \+ member(Symbol-_, Rest),
    check_unique_symbols(Rest).

% toDFA
% converts a non-deterministic FSA to an equivalent deterministic FSA
% uses a subset construction algorithm

toDFA(NFA, DFA) :-
    fsa(_, Alphabet, _, StartState, NFAAcceptStates) = NFA,
    % Compute initial DFA state (epsilon-closure of NFA start)
    epsilon_closure_set(NFA, [StartState], InitialDFAState),
    % Perform subset construction
    subset_construction(NFA, [InitialDFAState], [], Alphabet, 
                       DFAStates, DFATransitions),
    % Determine which DFA states are accepting
    % (those containing at least one NFA accepting state)
    findall(DFAState,
            (   member(DFAState, DFAStates),
                member(NFAState, DFAState),
                member(NFAState, NFAAcceptStates)
            ),
            DFAAcceptStatesDuplicates),
    list_to_set(DFAAcceptStatesDuplicates, DFAAcceptStates),
    % Construct the resulting DFA
    DFA = fsa(DFAStates, Alphabet, DFATransitions, InitialDFAState, DFAAcceptStates).

% Subset construction main algorithm
subset_construction(_, [], Visited, _, Visited, []).

subset_construction(NFA, [CurrentSet|Unprocessed], Visited, Alphabet, 
                    AllStates, AllTransitions) :-
    member(CurrentSet, Visited),
    !,
    subset_construction(NFA, Unprocessed, Visited, Alphabet, 
                       AllStates, AllTransitions).

subset_construction(NFA, [CurrentSet|Unprocessed], Visited, Alphabet, 
                    AllStates, AllTransitions) :-
    % For each symbol, compute the next DFA state
    findall(Symbol-NextSet,
            (   member(Symbol, Alphabet),
                compute_dfa_next_state(NFA, CurrentSet, Symbol, NextSet),
                NextSet \= []
            ),
            SymbolNextPairs),
    % Extract new states and transitions
    findall(NextSet, member(_-NextSet, SymbolNextPairs), NewSets),
    findall(trans(CurrentSet, Symbol, NextSet),
            member(Symbol-NextSet, SymbolNextPairs),
            NewTransitions),
    % Add new states to unprocessed queue
    append(NewSets, Unprocessed, UpdatedUnprocessed),
    % Accumulate transitions
    append(NewTransitions, RestTransitions, AllTransitions),
    % Recursively process remaining states
    subset_construction(NFA, UpdatedUnprocessed, [CurrentSet|Visited], 
                       Alphabet, AllStates, RestTransitions).

% Compute next DFA state from current DFA state on given symbol
compute_dfa_next_state(NFA, StateSet, Symbol, NextSet) :-
    % For each NFA state in the DFA state, compute next states
    findall(NextState,
            (   member(State, StateSet),
                next(NFA, State, Symbol, ReachableStates),
                member(NextState, ReachableStates)
            ),
            AllNextStates),
    list_to_set(AllNextStates, NextSet).