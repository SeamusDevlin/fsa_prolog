% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Helper to check if two sets are equal (order doesn't matter)
sets_equal(Set1, Set2) :-
    sort(Set1, Sorted1),
    sort(Set2, Sorted2),
    Sorted1 = Sorted2.

% Test 1: next(0, a) - From start state on symbol 'a'
% closure(0) = {0, 1, 2, 4, 7}
% From state 2: a -> 3
% From state 7: a -> 8
% Then closure({3, 8}): 3->6->1->{2,4,7}, so {3, 6, 1, 2, 4, 7, 8}
test_next_state_0_a :-
    example_fsa(FSA),
    next(FSA, 0, a, NextStates),
    sets_equal(NextStates, [3, 6, 1, 2, 4, 7, 8]),
    write('Test 1 PASSED: next(0, a) = {3, 6, 1, 2, 4, 7, 8}'), nl.

% Test 2: next(0, b) - From start state on symbol 'b'
% closure(0) = {0, 1, 2, 4, 7}
% From state 4: b -> 5
% Then closure({5}): 5->6->1->{2,4,7}, so {5, 6, 1, 2, 4, 7}
test_next_state_0_b :-
    example_fsa(FSA),
    next(FSA, 0, b, NextStates),
    sets_equal(NextStates, [5, 6, 1, 2, 4, 7]),
    write('Test 2 PASSED: next(0, b) = {5, 6, 1, 2, 4, 7}'), nl.

% Test 3: next(1, a) - From state 1 on symbol 'a'
% closure(1) = {1, 2, 4}
% From state 2: a -> 3
% Then closure({3}): {3, 6, 1, 2, 4, 7}
test_next_state_1_a :-
    example_fsa(FSA),
    next(FSA, 1, a, NextStates),
    sets_equal(NextStates, [3, 6, 1, 2, 4, 7]),
    write('Test 3 PASSED: next(1, a) = {3, 6, 1, 2, 4, 7}'), nl.

% Test 4: next(1, b) - From state 1 on symbol 'b'
% closure(1) = {1, 2, 4}
% From state 4: b -> 5
% Then closure({5}): {5, 6, 1, 2, 4, 7}
test_next_state_1_b :-
    example_fsa(FSA),
    next(FSA, 1, b, NextStates),
    sets_equal(NextStates, [5, 6, 1, 2, 4, 7]),
    write('Test 4 PASSED: next(1, b) = {5, 6, 1, 2, 4, 7}'), nl.

% Test 5: next(2, a) - State with only 'a' transition
% closure(2) = {2}
% From state 2: a -> 3
% Then closure({3}): {3, 6, 1, 2, 4, 7}
test_next_state_2_a :-
    example_fsa(FSA),
    next(FSA, 2, a, NextStates),
    sets_equal(NextStates, [3, 6, 1, 2, 4, 7]),
    write('Test 5 PASSED: next(2, a) = {3, 6, 1, 2, 4, 7}'), nl.

% Test 6: next(2, b) - No transition on 'b' from state 2
% closure(2) = {2}
% No 'b' transitions from state 2
% Should return empty set
test_next_state_2_b :-
    example_fsa(FSA),
    next(FSA, 2, b, NextStates),
    sets_equal(NextStates, []),
    write('Test 6 PASSED: next(2, b) = {} (no transition)'), nl.

% Test 7: next(4, b) - State with only 'b' transition
% closure(4) = {4}
% From state 4: b -> 5
% Then closure({5}): {5, 6, 1, 2, 4, 7}
test_next_state_4_b :-
    example_fsa(FSA),
    next(FSA, 4, b, NextStates),
    sets_equal(NextStates, [5, 6, 1, 2, 4, 7]),
    write('Test 7 PASSED: next(4, b) = {5, 6, 1, 2, 4, 7}'), nl.

% Test 8: next(4, a) - No transition on 'a' from state 4
% closure(4) = {4}
% No 'a' transitions from state 4
% Should return empty set
test_next_state_4_a :-
    example_fsa(FSA),
    next(FSA, 4, a, NextStates),
    sets_equal(NextStates, []),
    write('Test 8 PASSED: next(4, a) = {} (no transition)'), nl.

% Test 9: next(7, a) - Part of the "abb" sequence
% closure(7) = {7}
% From state 7: a -> 8
% Then closure({8}): {8}
test_next_state_7_a :-
    example_fsa(FSA),
    next(FSA, 7, a, NextStates),
    sets_equal(NextStates, [8]),
    write('Test 9 PASSED: next(7, a) = {8}'), nl.

% Test 10: next(8, b) - First 'b' in "abb"
% closure(8) = {8}
% From state 8: b -> 9
% Then closure({9}): {9}
test_next_state_8_b :-
    example_fsa(FSA),
    next(FSA, 8, b, NextStates),
    sets_equal(NextStates, [9]),
    write('Test 10 PASSED: next(8, b) = {9}'), nl.

% Test 11: next(9, b) - Second 'b' in "abb" (reaching accept state)
% closure(9) = {9}
% From state 9: b -> 10
% Then closure({10}): {10}
test_next_state_9_b :-
    example_fsa(FSA),
    next(FSA, 9, b, NextStates),
    sets_equal(NextStates, [10]),
    write('Test 11 PASSED: next(9, b) = {10} (accept state)'), nl.

% Test 12: next(10, a) - No transitions from accept state
% closure(10) = {10}
% No transitions from state 10
% Should return empty set
test_next_state_10_a :-
    example_fsa(FSA),
    next(FSA, 10, a, NextStates),
    sets_equal(NextStates, []),
    write('Test 12 PASSED: next(10, a) = {} (no transitions from accept)'), nl.

% Test 13: next(10, b) - No transitions from accept state
% closure(10) = {10}
% No transitions from state 10
% Should return empty set
test_next_state_10_b :-
    example_fsa(FSA),
    next(FSA, 10, b, NextStates),
    sets_equal(NextStates, []),
    write('Test 13 PASSED: next(10, b) = {} (no transitions from accept)'), nl.

% Test 14: Verify next rejects epsilon symbol
test_next_rejects_epsilon :-
    example_fsa(FSA),
    (   next(FSA, 0, epsilon, _)
    ->  write('Test 14 FAILED: Should reject epsilon as input symbol'), nl
    ;   write('Test 14 PASSED: Correctly rejects epsilon as input'), nl
    ).

% Run all tests
run_all_tests :-
    write('Running next (state transition) tests'), nl,
    write('Testing next function for FSA: (a|b)*abb'), nl, nl,
    write('The next function computes: closure(state) -> symbol -> closure(result)'), nl, nl,
    test_next_state_0_a,
    test_next_state_0_b,
    test_next_state_1_a,
    test_next_state_1_b,
    test_next_state_2_a,
    test_next_state_2_b,
    test_next_state_4_b,
    test_next_state_4_a,
    test_next_state_7_a,
    test_next_state_8_b,
    test_next_state_9_b,
    test_next_state_10_a,
    test_next_state_10_b,
    test_next_rejects_epsilon,
    nl, write('All tests completed!'), nl.
