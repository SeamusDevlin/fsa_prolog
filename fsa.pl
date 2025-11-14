% addState
% adds a new state to the FSA
% IsStart and IsAccepts are booleans indicating if the new state is a start or accept state
% a new state is added to the list and if isStart is true it will then replace the current start
% same with the isAccept

addState(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
         NewState, IsStart, IsAccept,
         fsa(NewStates, Alphabet, Transitions, NewStartState, NewAcceptStates)) :-
    add_state_to_list(NewState, States, NewStates),
    update_start_state(IsStart, NewState, StartState, NewStartState),
    update_accept_states(IsAccept, NewState, AcceptStates, NewAcceptStates).

% helper predicate for adding state to list
add_state_to_list(State, States, States) :- 
    member(State, States), !.
add_state_to_list(State, States, [State|States]).

% helper predicate for updating start state
update_start_state(true, NewState, _, NewState) :- !.
update_start_state(_, _, OldStart, OldStart).

% helper predicate for updating accept states
update_accept_states(true, State, AcceptStates, AcceptStates) :- 
    member(State, AcceptStates), !.
update_accept_states(true, State, AcceptStates, [State|AcceptStates]) :- !.
update_accept_states(_, _, AcceptStates, AcceptStates).

% addTransition
% adds a new transition to the FSA

addTransition(fsa(States, Alphabet, Transitions, StartState, AcceptStates),
              FromState, Symbol, ToState,
              fsa(States, NewAlphabet, NewTransitions, StartState, AcceptStates)) :-
              % checks if FromState and ToState exist
              member(FromState, States),
              member(ToState, States),
              % adds transition to the transition list
              NewTransitions = [trans(FromState, Symbol, ToState) | Transitions],
              % updates the alphabet if necessary
              update_alphabet(Symbol, Alphabet, NewAlphabet).

% helper predicate for updating alphabet
update_alphabet(epsilon, Alphabet, Alphabet) :- !.
update_alphabet(Symbol, Alphabet, Alphabet) :- 
    member(Symbol, Alphabet), !.
update_alphabet(Symbol, Alphabet, [Symbol|Alphabet]).

% accepts
% checks if the FSA accepts the given string

accepts(FSA, String) :-
    fsa(_, _, _, StartState, AcceptStates) = FSA,
    % get initial states
    epsilon_closure(FSA, [StartState], InitialStates),
    % processes the input string 
    process_string(FSA, InitialStates, String, FinalStates),
    % checks if any of the final states is an accept state
    member(AcceptState, AcceptStates),
    member(AcceptState, FinalStates).

% process input string symbol by symbol
process_string(_, CurrentStates, [], CurrentStates).

process_string(FSA, CurrentStates, [Symbol | Rest], FinalStates) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % this is so we can find all next states from current states on Symbol
    collect_transitions(CurrentStates, Symbol, Transitions, NextStatesUnclosed),
    % this will get all the epsilon closures of the states which were already reached
    epsilon_closure_set(FSA, NextStatesUnclosed, NextStates),
    % will then continue processing rest of the symbols
    process_string(FSA, NextStates, Rest, FinalStates).

% helper to collect all transition targets
collect_transitions(FromStates, Symbol, Transitions, ToStates) :-
    findall(ToState,
        (   member(FromState, FromStates),
            member(trans(FromState, Symbol, ToState), Transitions)
        ),
        ToStates).

% essentially a wrapper for epsilon_closure_set for multiple states and handles starting states and then sends to epsilon_closure_set for actual processing
epsilon_closure(FSA, StateList, ClosureSet) :-
    epsilon_closure_set(FSA, StateList, ClosureSet).

% closure
% returns the epsilon closure of a set of states reachable via epsilon transitions
% using depth-first search

closure(FSA, State, ClosureSet) :-
    epsilon_closure_set(FSA, [State], ClosureSet).

% computation for epsilon-closure for a set of states
epsilon_closure_set(FSA, StateSet, ClosureSet) :-
    epsilon_closure_helper(FSA, StateSet, [], ClosureSet).

% this is the actual computation for epsilon-closure using depth-first search with visited tracking
epsilon_closure_helper(_, [], Visited, Visited).

epsilon_closure_helper(FSA, [State|Rest], Visited, ClosureSet) :-
    member(State, Visited),
    !,
    epsilon_closure_helper(FSA, Rest, Visited, ClosureSet).

epsilon_closure_helper(FSA, [State|Rest], Visited, ClosureSet) :-
    fsa(_, _, Transitions, _, _) = FSA,
    findall(ToState,
            member(trans(State, epsilon, ToState), Transitions),
            EpsilonReachable),
    append(EpsilonReachable, Rest, UpdatedRest),
    epsilon_closure_helper(FSA, UpdatedRest, [State|Visited], ClosureSet).

% next
% returns the set of states reachable from a set of states on a given input symbol
% symbol must not be epsilon

next(FSA, State, Symbol, NextStates) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % must get epsilon-closure of the starting state first
    epsilon_closure_set(FSA, [State], ClosureOfState),
    % then finds all states reachable via Symbol from any state in the closure
    findall(ToState,
            (   member(FromState, ClosureOfState),
                member(trans(FromState, Symbol, ToState), Transitions)
            ),
            DirectStates),
    % the result being the epsilon-closure of the directly reachable states
    epsilon_closure_set(FSA, DirectStates, NextStates).

% deterministic
% checks if the FSA is deterministic meaning no state has two transitions for the same symbol
% violated if there exists a state with two transitions on the same symbol (excluding epsilon)

deterministic(fsa(States, _, Transitions, _, _)) :-
    has_no_epsilon_transitions(Transitions),
    all_states_deterministic(States, Transitions).

% Helper to check for no epsilon transitions
has_no_epsilon_transitions(Transitions) :-
    \+ member(trans(_, epsilon, _), Transitions).

% Helper to verify all states are deterministic
all_states_deterministic([], _).
all_states_deterministic([State|Rest], Transitions) :-
    is_state_deterministic(State, Transitions),
    all_states_deterministic(Rest, Transitions).

% checks if a given state is deterministic
is_state_deterministic(State, Transitions) :-
    % then it needs to get all transitions from this state
    findall(Symbol-ToState,
            member(trans(State, Symbol, ToState), Transitions),
            Pairs),
    % verify no symbol appears more than once
    check_unique_symbols(Pairs).

% verify each symbol appears at most once in transitions
check_unique_symbols([]).
check_unique_symbols([Symbol-_|Rest]) :-
    \+ member(Symbol-_, Rest),
    check_unique_symbols(Rest).

% toDFA
% converts NFA to DFA using subset construction

toDFA(NFA, DFA) :-
    fsa(_, Alphabet, _, Start, NFAAccepts) = NFA,
    epsilon_closure_set(NFA, [Start], InitState),
    build_dfa(NFA, [InitState], [], Alphabet, States, Trans),
    findall(S, (member(S, States), member(N, S), member(N, NFAAccepts)), Accepts),
    DFA = fsa(States, Alphabet, Trans, InitState, Accepts).

% builds DFA states and transitions
build_dfa(_, [], Visited, _, Visited, []).

build_dfa(NFA, [S|Rest], Visited, Alphabet, States, Trans) :-
    member(S, Visited), !,
    build_dfa(NFA, Rest, Visited, Alphabet, States, Trans).

build_dfa(NFA, [S|Rest], Visited, Alphabet, States, [trans(S,Sym,Next)|RestTrans]) :-
    member(Sym, Alphabet),
    findall(NS, (member(N, S), next(NFA, N, Sym, R), member(NS, R)), NextStates),
    list_to_set(NextStates, Next),
    Next \= [], !,
    build_dfa(NFA, [S|Rest], Visited, Alphabet, States, RestTrans).

build_dfa(NFA, [S|Rest], Visited, Alphabet, States, Trans) :-
    findall(Next, 
            (member(Sym, Alphabet),
             findall(NS, (member(N, S), next(NFA, N, Sym, R), member(NS, R)), L),
             list_to_set(L, Next), 
             Next \= []), 
            NewStates),
    append(NewStates, Rest, Updated),
    build_dfa(NFA, Updated, [S|Visited], Alphabet, States, Trans).