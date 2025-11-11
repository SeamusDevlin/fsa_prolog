# FSA Prolog Implementation

A purely declarative Prolog implementation of Finite State Automata (FSA) with comprehensive operations for NFA and DFA manipulation.

## Features

This implementation provides the following operations on finite state automata:

- **addState**: Add new states to the FSA (with start/accept state flags)
- **addTransition**: Add transitions between states (including epsilon transitions)
- **accepts**: Check if the FSA accepts a given input string
- **closure**: Compute epsilon-closure of a state
- **next**: Compute reachable states on an input symbol
- **deterministic**: Check if the FSA is deterministic (DFA)
- **toDFA**: Convert NFA to equivalent DFA using subset construction algorithm

## Data Structure

An FSA is represented as:
```prolog
fsa(States, Alphabet, Transitions, StartState, AcceptStates)
```

Where:
- `States`: List of state identifiers
- `Alphabet`: List of input symbols (excluding epsilon)
- `Transitions`: List of `trans(FromState, Symbol, ToState)` where Symbol can be `epsilon`
- `StartState`: Single start state identifier
- `AcceptStates`: List of accepting state identifiers

## Running the Code

### Requirements
- SWI-Prolog (swipl)

### Usage

1. Load the main file:
```bash
swipl -l fsa.pl
```

2. Example queries:
```prolog
% Load example FSA (accepts strings matching (a|b)*abb)
?- example_fsa(NFA).

% Check if a string is accepted
?- example_fsa(NFA), accepts(NFA, [a,b,b]).
true.

% Compute epsilon-closure of state 0
?- example_fsa(NFA), closure(NFA, 0, ClosureSet).
ClosureSet = [0, 1, 2, 4, 7].

% Convert NFA to DFA
?- example_fsa(NFA), toDFA(NFA, DFA).

% Check if FSA is deterministic
?- example_fsa(NFA), deterministic(NFA).
false.
```

## Running Tests

The implementation includes comprehensive test suites:

```bash
# Test addState operation
swipl -g "['test_addstate.pl'], run_all_tests, halt."

# Test addTransition operation
swipl -g "['test_addtransition.pl'], run_all_tests, halt."

# Test accepts operation
swipl -g "['test_accepts.pl'], run_all_tests, halt."

# Test closure operation
swipl -g "['test_closure.pl'], run_all_tests, halt."

# Test next operation
swipl -g "['test_next.pl'], run_all_tests, halt."

# Test deterministic operation
swipl -g "['test_deterministic.pl'], run_all_tests, halt."

# Test toDFA operation
swipl -g "['test_todfa.pl'], run_all_tests, halt."
```

## Example FSA

The `example.pl` file contains an NFA that accepts the language defined by the regular expression `(a|b)*abb`:

- States: {0,1,2,3,4,5,6,7,8,9,10}
- Alphabet: {a,b}
- Start state: 0
- Accept states: {10}

## Implementation Details

- **Purely declarative**: No use of assert/retract, maintaining functional purity
- **Epsilon-closure**: Efficient DFS-based algorithm with cycle handling
- **Subset construction**: Standard powerset construction for NFA to DFA conversion
- **Comprehensive testing**: All operations tested with multiple test cases

## Files

- `fsa.pl`: Main implementation file
- `example.pl`: Example FSA and reference implementations
- `test_*.pl`: Comprehensive test suites for each operation

## License

This is an academic project for learning FSA theory and Prolog programming.

## Author

Seamus Devlin
