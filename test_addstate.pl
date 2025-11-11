% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Test 1: Add a regular state (not start, not accept)
test_add_regular_state :-
    example_fsa(FSA),
    addState(FSA, 99, false, false, NewFSA),
    NewFSA = fsa(States, _, _, _, _),
    member(99, States),
    write('Test 1 PASSED: Regular state added'), nl.

% Test 2: Add a new start state
test_add_start_state :-
    example_fsa(FSA),
    addState(FSA, 100, true, false, NewFSA),
    NewFSA = fsa(_, _, _, StartState, _),
    StartState = 100,
    write('Test 2 PASSED: New start state added'), nl.

% Test 3: Add a new accept state
test_add_accept_state :-
    example_fsa(FSA),
    addState(FSA, 101, false, true, NewFSA),
    NewFSA = fsa(_, _, _, _, AcceptStates),
    member(101, AcceptStates),
    write('Test 3 PASSED: Accept state added'), nl.

% Test 4: Add a state that is both start and accept
test_add_start_accept_state :-
    example_fsa(FSA),
    addState(FSA, 102, true, true, NewFSA),
    NewFSA = fsa(_, _, _, StartState, AcceptStates),
    StartState = 102,
    member(102, AcceptStates),
    write('Test 4 PASSED: Start+Accept state added'), nl.

% Test 5: Try to add an existing state (should not duplicate)
test_add_existing_state :-
    example_fsa(FSA),
    FSA = fsa(States, _, _, _, _),
    length(States, OriginalCount),
    addState(FSA, 1, false, false, NewFSA),
    NewFSA = fsa(NewStates, _, _, _, _),
    length(NewStates, NewCount),
    NewCount = OriginalCount,
    write('Test 5 PASSED: Existing state not duplicated'), nl.

% Run all tests
run_all_tests :-
    write('Running addState tests...'), nl, nl,
    test_add_regular_state,
    test_add_start_state,
    test_add_accept_state,
    test_add_start_accept_state,
    test_add_existing_state,
    nl, write('All tests completed!'), nl.
