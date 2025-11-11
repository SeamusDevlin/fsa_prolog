% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Test 1: Add a simple transition with existing symbol
test_add_simple_transition :-
    example_fsa(FSA),
    addTransition(FSA, 1, a, 5, NewFSA),
    NewFSA = fsa(_, _, Transitions, _, _),
    member(trans(1, a, 5), Transitions),
    write('Test 1 PASSED: Simple transition added'), nl.

% Test 2: Add a transition with new symbol (should update alphabet)
test_add_new_symbol :-
    example_fsa(FSA),
    FSA = fsa(_, Alphabet, _, _, _),
    \+ member(c, Alphabet),
    addTransition(FSA, 1, c, 2, NewFSA),
    NewFSA = fsa(_, NewAlphabet, Transitions, _, _),
    member(c, NewAlphabet),
    member(trans(1, c, 2), Transitions),
    write('Test 2 PASSED: New symbol added to alphabet'), nl.

% Test 3: Add epsilon transition (should not modify alphabet)
test_add_epsilon_transition :-
    example_fsa(FSA),
    FSA = fsa(_, Alphabet, _, _, _),
    length(Alphabet, OriginalSize),
    addTransition(FSA, 2, epsilon, 3, NewFSA),
    NewFSA = fsa(_, NewAlphabet, Transitions, _, _),
    length(NewAlphabet, NewSize),
    NewSize = OriginalSize,
    member(trans(2, epsilon, 3), Transitions),
    write('Test 3 PASSED: Epsilon transition added without changing alphabet'), nl.

% Test 4: Try to add transition with non-existent FromState (should fail)
test_invalid_from_state :-
    example_fsa(FSA),
    (   addTransition(FSA, 999, a, 1, _)
    ->  write('Test 4 FAILED: Should not allow non-existent FromState'), nl
    ;   write('Test 4 PASSED: Correctly rejected non-existent FromState'), nl
    ).

% Test 5: Try to add transition with non-existent ToState (should fail)
test_invalid_to_state :-
    example_fsa(FSA),
    (   addTransition(FSA, 1, a, 999, _)
    ->  write('Test 5 FAILED: Should not allow non-existent ToState'), nl
    ;   write('Test 5 PASSED: Correctly rejected non-existent ToState'), nl
    ).

% Test 6: Add duplicate transition (should it be allowed?)
test_duplicate_transition :-
    example_fsa(FSA),
    FSA = fsa(_, _, Transitions, _, _),
    member(trans(0, epsilon, 1), Transitions),  % This exists in example
    addTransition(FSA, 0, epsilon, 1, NewFSA),
    NewFSA = fsa(_, _, NewTransitions, _, _),
    length(Transitions, OriginalCount),
    length(NewTransitions, NewCount),
    NewCount is OriginalCount + 1,  % It will add duplicate
    write('Test 6 PASSED: Duplicate transition added (NFAs allow this)'), nl.

% Run all tests
run_all_tests :-
    write('Running addTransition tests...'), nl, nl,
    test_add_simple_transition,
    test_add_new_symbol,
    test_add_epsilon_transition,
    test_invalid_from_state,
    test_invalid_to_state,
    test_duplicate_transition,
    nl, write('All tests completed!'), nl.
