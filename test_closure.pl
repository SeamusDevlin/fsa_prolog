% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Helper to check if two sets are equal (order doesn't matter)
sets_equal(Set1, Set2) :-
    sort(Set1, Sorted1),
    sort(Set2, Sorted2),
    Sorted1 = Sorted2.

% Test 1: State 0 epsilon-closure
% From state 0: epsilon -> 1 and epsilon -> 7
% From state 1: epsilon -> 2 and epsilon -> 4
% So closure(0) should include: {0, 1, 2, 4, 7}
test_closure_state_0 :-
    example_fsa(FSA),
    closure(FSA, 0, ClosureSet),
    sets_equal(ClosureSet, [0, 1, 2, 4, 7]),
    write('Test 1 PASSED: closure(0) = {0, 1, 2, 4, 7}'), nl.

% Test 2: State 1 epsilon-closure
% From state 1: epsilon -> 2 and epsilon -> 4
% So closure(1) should include: {1, 2, 4}
test_closure_state_1 :-
    example_fsa(FSA),
    closure(FSA, 1, ClosureSet),
    sets_equal(ClosureSet, [1, 2, 4]),
    write('Test 2 PASSED: closure(1) = {1, 2, 4}'), nl.

% Test 3: State 2 epsilon-closure
% State 2 has no epsilon transitions
% So closure(2) should only include: {2}
test_closure_state_2 :-
    example_fsa(FSA),
    closure(FSA, 2, ClosureSet),
    sets_equal(ClosureSet, [2]),
    write('Test 3 PASSED: closure(2) = {2}'), nl.

% Test 4: State 3 epsilon-closure
% From state 3: epsilon -> 6
% From state 6: epsilon -> 1 and epsilon -> 7
% From state 1: epsilon -> 2 and epsilon -> 4
% So closure(3) should include: {3, 6, 1, 7, 2, 4}
test_closure_state_3 :-
    example_fsa(FSA),
    closure(FSA, 3, ClosureSet),
    sets_equal(ClosureSet, [3, 6, 1, 7, 2, 4]),
    write('Test 4 PASSED: closure(3) = {3, 6, 1, 7, 2, 4}'), nl.

% Test 5: State 4 epsilon-closure
% State 4 has no epsilon transitions
% So closure(4) should only include: {4}
test_closure_state_4 :-
    example_fsa(FSA),
    closure(FSA, 4, ClosureSet),
    sets_equal(ClosureSet, [4]),
    write('Test 5 PASSED: closure(4) = {4}'), nl.

% Test 6: State 5 epsilon-closure
% From state 5: epsilon -> 6
% From state 6: epsilon -> 1 and epsilon -> 7
% From state 1: epsilon -> 2 and epsilon -> 4
% So closure(5) should include: {5, 6, 1, 7, 2, 4}
test_closure_state_5 :-
    example_fsa(FSA),
    closure(FSA, 5, ClosureSet),
    sets_equal(ClosureSet, [5, 6, 1, 7, 2, 4]),
    write('Test 6 PASSED: closure(5) = {5, 6, 1, 7, 2, 4}'), nl.

% Test 7: State 6 epsilon-closure (has cycle!)
% From state 6: epsilon -> 1 and epsilon -> 7
% From state 1: epsilon -> 2 and epsilon -> 4
% Note: This creates a cycle (6->1, and later paths can go back to 6)
% So closure(6) should include: {6, 1, 7, 2, 4}
test_closure_state_6 :-
    example_fsa(FSA),
    closure(FSA, 6, ClosureSet),
    sets_equal(ClosureSet, [6, 1, 7, 2, 4]),
    write('Test 7 PASSED: closure(6) = {6, 1, 7, 2, 4}'), nl.

% Test 8: State 7 epsilon-closure
% State 7 has no epsilon transitions
% So closure(7) should only include: {7}
test_closure_state_7 :-
    example_fsa(FSA),
    closure(FSA, 7, ClosureSet),
    sets_equal(ClosureSet, [7]),
    write('Test 8 PASSED: closure(7) = {7}'), nl.

% Test 9: State 8 epsilon-closure
% State 8 has no epsilon transitions
% So closure(8) should only include: {8}
test_closure_state_8 :-
    example_fsa(FSA),
    closure(FSA, 8, ClosureSet),
    sets_equal(ClosureSet, [8]),
    write('Test 9 PASSED: closure(8) = {8}'), nl.

% Test 10: State 9 epsilon-closure
% State 9 has no epsilon transitions
% So closure(9) should only include: {9}
test_closure_state_9 :-
    example_fsa(FSA),
    closure(FSA, 9, ClosureSet),
    sets_equal(ClosureSet, [9]),
    write('Test 10 PASSED: closure(9) = {9}'), nl.

% Test 11: State 10 epsilon-closure (accept state)
% State 10 has no epsilon transitions
% So closure(10) should only include: {10}
test_closure_state_10 :-
    example_fsa(FSA),
    closure(FSA, 10, ClosureSet),
    sets_equal(ClosureSet, [10]),
    write('Test 11 PASSED: closure(10) = {10}'), nl.

% Test 12: Verify closure handles cycles properly
% Starting from state 3, we should reach state 6, which can reach back to 1
% and from there potentially back to 6 (through transitions)
% But the closure should not loop infinitely
test_closure_no_infinite_loop :-
    example_fsa(FSA),
    closure(FSA, 3, ClosureSet),
    length(ClosureSet, Length),
    Length =< 11,  % Should not exceed total number of states
    write('Test 12 PASSED: Closure handles cycles without infinite loop'), nl.

% Run all tests
run_all_tests :-
    write('Running closure (epsilon-closure) tests'), nl,
    write('Testing epsilon transitions in FSA: (a|b)*abb'), nl, nl,
    write('Epsilon transitions:'), nl,
    write('  0 -> {1, 7}'), nl,
    write('  1 -> {2, 4}'), nl,
    write('  3 -> {6}'), nl,
    write('  5 -> {6}'), nl,
    write('  6 -> {1, 7} (creates cycles!)'), nl, nl,
    test_closure_state_0,
    test_closure_state_1,
    test_closure_state_2,
    test_closure_state_3,
    test_closure_state_4,
    test_closure_state_5,
    test_closure_state_6,
    test_closure_state_7,
    test_closure_state_8,
    test_closure_state_9,
    test_closure_state_10,
    test_closure_no_infinite_loop,
    nl, write('All tests completed!'), nl.
