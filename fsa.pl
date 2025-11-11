% addState
% adds a new state to the FSA
% IsStart and IsAccepts are booleans indicating if the new state is a start or accept state
% a new state is added to the list and if isStart is true it will then replace the current start
% same with the isAccept

addState(fsa(States, Alphabet, Transitions, start_state, accept_states),
         new_state, is_start, is_accept,
         fsa(new_states, Alphabet, Transitions, new_start_state, new_accept_states)) :-
    add_state_to_list(new_state, States, new_states),
    update_start_state(is_start, new_state, start_state, new_start_state),
    update_accept_states(is_accept, new_state, accept_states, new_accept_states).

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

addTransition(fsa(States, Alphabet, Transitions, start_state, accept_states),
              from_state, Symbol, to_state,
              fsa(States, new_alphabet, new_transitions, start_state, accept_states)) :-
              % checks if from_state and to_state exist
              member(from_state, States),
              member(to_state, States),
              % adds transition to the transition list
              new_transitions = [trans(from_state, Symbol, to_state) | Transitions],
              % updates the alphabet if necessary
              update_alphabet(Symbol, Alphabet, new_alphabet).

% helper predicate for updating alphabet
update_alphabet(epsilon, Alphabet, Alphabet) :- !.
update_alphabet(Symbol, Alphabet, Alphabet) :- 
    member(Symbol, Alphabet), !.
update_alphabet(Symbol, Alphabet, [Symbol|Alphabet]).

% accepts
% checks if the FSA accepts the given string

accepts(FSA, String) :-
    fsa(_, _, _, start_state, accept_states) = FSA,
    % get initial states
    epsilon_closure(FSA, [start_state], initial_states),
    % processes the input string 
    process_string(FSA, initial_states, String, final_states),
    % checks if any of the final states is an accept state
    member(accept_state, accept_states),
    member(accept_state, final_states).

% process input string symbol by symbol
process_string(_, current_states, [], current_states).

process_string(FSA, current_states, [Symbol | Rest], final_states) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % this is so we can find all next states from current states on Symbol
    collect_transitions(current_states, Symbol, Transitions, next_states_unclosed),
    % this will get all the epsilon closures of the states which were already reached
    epsilon_closure_set(FSA, next_states_unclosed, next_states),
    % will then continue processing rest of the symbols
    process_string(FSA, next_states, Rest, final_states).

% helper to collect all transition targets
collect_transitions(FromStates, Symbol, Transitions, ToStates) :-
    findall(to_state,
        (   member(from_state, FromStates),
            member(trans(from_state, Symbol, to_state), Transitions)
        ),
        ToStates).

% essentially a wrapper for epsilon_closure_set for multiple states and handles starting states and then sends to epsilon_closure_set for actual processing
epsilon_closure(FSA, state_list, closure_set) :-
    epsilon_closure_set(FSA, state_list, closure_set).

% closure
% returns the epsilon closure of a set of states reachable via epsilon transitions
% using depth-first search

closure(FSA, State, closure_set) :-
    epsilon_closure_set(FSA, [State], closure_set).

% computation for epsilon-closure for a set of states
epsilon_closure_set(FSA, state_set, closure_set) :-
    epsilon_closure_helper(FSA, state_set, [], closure_set).

% this is the actual computation for epsilon-closure using depth-first search with visited tracking
epsilon_closure_helper(_, [], Visited, Visited).

epsilon_closure_helper(FSA, [State|Rest], Visited, closure_set) :-
    member(State, Visited),
    !,
    epsilon_closure_helper(FSA, Rest, Visited, closure_set).

epsilon_closure_helper(FSA, [State|Rest], Visited, closure_set) :-
    fsa(_, _, Transitions, _, _) = FSA,
    % Find all states reachable via epsilon from State
    findall(to_state,
            member(trans(State, epsilon, to_state), Transitions),
            epsilon_reachable),
    % Add newly discovered states to the processing queue
    append(epsilon_reachable, Rest, updated_rest),
    % Continue with State added to visited set
    epsilon_closure_helper(FSA, updated_rest, [State|Visited], closure_set).

% next
% returns the set of states reachable from a set of states on a given input symbol
% symbol must not be epsilon

next(FSA, State, Symbol, next_states) :-
    Symbol \= epsilon,
    fsa(_, _, Transitions, _, _) = FSA,
    % must get epsilon-closure of the starting state first
    epsilon_closure_set(FSA, [State], closure_of_state),
    % then finds all states reachable via Symbol from any state in the closure
    findall(to_state,
            (   member(from_state, closure_of_state),
                member(trans(from_state, Symbol, to_state), Transitions)
            ),
            direct_states),
    % the result being the epsilon-closure of the directly reachable states
    epsilon_closure_set(FSA, direct_states, next_states).

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