% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Test 1: Example FSA is non-deterministic (has epsilon transitions)
test_example_nondeterministic :-
    example_fsa(FSA),
    (   deterministic(FSA)
    ->  write('Test 1 FAILED: Example FSA should be non-deterministic'), nl
    ;   write('Test 1 PASSED: Example FSA is non-deterministic (has epsilon)'), nl
    ).

% Test 2: Simple deterministic FSA - accepts "a"
% States: {0, 1}, Alphabet: {a}, Start: 0, Accept: {1}
% Transitions: 0 -a-> 1
test_simple_deterministic :-
    SimpleDFA = fsa([0, 1], [a], [trans(0, a, 1)], 0, [1]),
    (   deterministic(SimpleDFA)
    ->  write('Test 2 PASSED: Simple DFA is deterministic'), nl
    ;   write('Test 2 FAILED: Simple DFA should be deterministic'), nl
    ).

% Test 3: DFA accepting strings ending in 'a'
% States: {0, 1}, Alphabet: {a, b}
% Transitions: 0 -a-> 1, 0 -b-> 0, 1 -a-> 1, 1 -b-> 0
test_dfa_ending_a :-
    DFA = fsa([0, 1], [a, b],
              [trans(0, a, 1), trans(0, b, 0),
               trans(1, a, 1), trans(1, b, 0)],
              0, [1]),
    (   deterministic(DFA)
    ->  write('Test 3 PASSED: DFA ending in "a" is deterministic'), nl
    ;   write('Test 3 FAILED: DFA ending in "a" should be deterministic'), nl
    ).

% Test 4: NFA with epsilon transition
% States: {0, 1, 2}, Transitions include epsilon
test_nfa_with_epsilon :-
    NFA = fsa([0, 1, 2], [a],
              [trans(0, epsilon, 1), trans(1, a, 2)],
              0, [2]),
    (   deterministic(NFA)
    ->  write('Test 4 FAILED: NFA with epsilon should be non-deterministic'), nl
    ;   write('Test 4 PASSED: NFA with epsilon is non-deterministic'), nl
    ).

% Test 5: NFA with multiple transitions on same symbol
% States: {0, 1, 2}, State 0 has TWO transitions on 'a'
test_nfa_multiple_transitions :-
    NFA = fsa([0, 1, 2], [a],
              [trans(0, a, 1), trans(0, a, 2)],
              0, [1, 2]),
    (   deterministic(NFA)
    ->  write('Test 5 FAILED: NFA with multiple a-transitions should be non-deterministic'), nl
    ;   write('Test 5 PASSED: NFA with multiple a-transitions is non-deterministic'), nl
    ).

% Test 6: NFA with multiple transitions on 'b' from same state
test_nfa_multiple_b_transitions :-
    NFA = fsa([0, 1, 2], [a, b],
              [trans(0, a, 1), trans(0, b, 1), trans(0, b, 2)],
              0, [1, 2]),
    (   deterministic(NFA)
    ->  write('Test 6 FAILED: NFA with multiple b-transitions should be non-deterministic'), nl
    ;   write('Test 6 PASSED: NFA with multiple b-transitions is non-deterministic'), nl
    ).

% Test 7: DFA with complete state graph
% Every state has exactly one transition per symbol
test_complete_dfa :-
    DFA = fsa([0, 1, 2], [a, b],
              [trans(0, a, 1), trans(0, b, 2),
               trans(1, a, 0), trans(1, b, 2),
               trans(2, a, 1), trans(2, b, 0)],
              0, [0]),
    (   deterministic(DFA)
    ->  write('Test 7 PASSED: Complete DFA is deterministic'), nl
    ;   write('Test 7 FAILED: Complete DFA should be deterministic'), nl
    ).

% Test 8: Partial DFA (some states missing transitions)
% This is still deterministic - just incomplete
test_partial_dfa :-
    PartialDFA = fsa([0, 1], [a, b],
                     [trans(0, a, 1)],  % Only one transition
                     0, [1]),
    (   deterministic(PartialDFA)
    ->  write('Test 8 PASSED: Partial DFA is still deterministic'), nl
    ;   write('Test 8 FAILED: Partial DFA should be deterministic'), nl
    ).

% Test 9: DFA with self-loops
test_dfa_with_self_loops :-
    DFA = fsa([0, 1], [a, b],
              [trans(0, a, 0), trans(0, b, 1),
               trans(1, a, 1), trans(1, b, 0)],
              0, [0, 1]),
    (   deterministic(DFA)
    ->  write('Test 9 PASSED: DFA with self-loops is deterministic'), nl
    ;   write('Test 9 FAILED: DFA with self-loops should be deterministic'), nl
    ).

% Test 10: Empty FSA (no transitions)
test_empty_fsa :-
    EmptyFSA = fsa([0], [a], [], 0, [0]),
    (   deterministic(EmptyFSA)
    ->  write('Test 10 PASSED: Empty FSA (no transitions) is deterministic'), nl
    ;   write('Test 10 FAILED: Empty FSA should be deterministic'), nl
    ).

% Test 11: FSA with only epsilon transitions
test_only_epsilon :-
    EpsilonFSA = fsa([0, 1, 2], [],
                     [trans(0, epsilon, 1), trans(1, epsilon, 2)],
                     0, [2]),
    (   deterministic(EpsilonFSA)
    ->  write('Test 11 FAILED: FSA with only epsilon should be non-deterministic'), nl
    ;   write('Test 11 PASSED: FSA with only epsilon is non-deterministic'), nl
    ).

% Test 12: Mixed - one state deterministic, another non-deterministic
test_mixed_determinism :-
    MixedNFA = fsa([0, 1, 2, 3], [a, b],
                   [trans(0, a, 1), trans(0, b, 2),  % State 0: OK
                    trans(1, a, 2), trans(1, a, 3)], % State 1: TWO a-transitions!
                   0, [2, 3]),
    (   deterministic(MixedNFA)
    ->  write('Test 12 FAILED: Mixed FSA should be non-deterministic'), nl
    ;   write('Test 12 PASSED: Mixed FSA is non-deterministic (state 1 has two a-transitions)'), nl
    ).

% Test 13: DFA with single state
test_single_state_dfa :-
    SingleDFA = fsa([0], [a, b],
                    [trans(0, a, 0), trans(0, b, 0)],
                    0, [0]),
    (   deterministic(SingleDFA)
    ->  write('Test 13 PASSED: Single-state DFA is deterministic'), nl
    ;   write('Test 13 FAILED: Single-state DFA should be deterministic'), nl
    ).

% Test 14: Three-way non-determinism (three transitions on same symbol)
test_three_way_nondeterminism :-
    NFA = fsa([0, 1, 2, 3], [a],
              [trans(0, a, 1), trans(0, a, 2), trans(0, a, 3)],
              0, [1, 2, 3]),
    (   deterministic(NFA)
    ->  write('Test 14 FAILED: Three-way NFA should be non-deterministic'), nl
    ;   write('Test 14 PASSED: Three-way NFA is non-deterministic'), nl
    ).

% Run all tests
run_all_tests :-
    write('Running deterministic tests'), nl,
    write('Testing if FSAs are deterministic (no epsilon, at most one transition per (state, symbol))'), nl, nl,
    test_example_nondeterministic,
    test_simple_deterministic,
    test_dfa_ending_a,
    test_nfa_with_epsilon,
    test_nfa_multiple_transitions,
    test_nfa_multiple_b_transitions,
    test_complete_dfa,
    test_partial_dfa,
    test_dfa_with_self_loops,
    test_empty_fsa,
    test_only_epsilon,
    test_mixed_determinism,
    test_single_state_dfa,
    test_three_way_nondeterminism,
    nl, write('All tests completed!'), nl.
