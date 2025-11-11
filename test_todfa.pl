% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Test 1: Convert example NFA to DFA and verify it's deterministic
test_example_to_dfa :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   deterministic(DFA)
    ->  write('Test 1 PASSED: Converted NFA to DFA is deterministic'), nl
    ;   write('Test 1 FAILED: Converted DFA should be deterministic'), nl
    ).

% Test 2: DFA should accept the same strings as the original NFA
% Test with "abb"
test_accepts_abb :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   (accepts(NFA, [a,b,b]), accepts(DFA, [a,b,b]))
    ->  write('Test 2 PASSED: DFA accepts "abb" like NFA'), nl
    ;   write('Test 2 FAILED: DFA should accept "abb"'), nl
    ).

% Test 3: DFA should accept "aabb"
test_accepts_aabb :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   (accepts(NFA, [a,a,b,b]), accepts(DFA, [a,a,b,b]))
    ->  write('Test 3 PASSED: DFA accepts "aabb" like NFA'), nl
    ;   write('Test 3 FAILED: DFA should accept "aabb"'), nl
    ).

% Test 4: DFA should reject "ab" like NFA
test_rejects_ab :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   (\+ accepts(NFA, [a,b]), \+ accepts(DFA, [a,b]))
    ->  write('Test 4 PASSED: DFA rejects "ab" like NFA'), nl
    ;   write('Test 4 FAILED: DFA should reject "ab"'), nl
    ).

% Test 5: DFA should reject empty string like NFA
test_rejects_empty :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   (\+ accepts(NFA, []), \+ accepts(DFA, []))
    ->  write('Test 5 PASSED: DFA rejects empty string like NFA'), nl
    ;   write('Test 5 FAILED: DFA should reject empty string'), nl
    ).

% Test 6: Simple NFA with epsilon transitions
% NFA: 0 -epsilon-> 1 -a-> 2 (accepts "a")
test_simple_epsilon_nfa :-
    SimpleNFA = fsa([0, 1, 2], [a],
                    [trans(0, epsilon, 1), trans(1, a, 2)],
                    0, [2]),
    toDFA(SimpleNFA, DFA),
    (   (deterministic(DFA), accepts(DFA, [a]), \+ accepts(DFA, []))
    ->  write('Test 6 PASSED: Simple epsilon NFA converted correctly'), nl
    ;   write('Test 6 FAILED: Simple epsilon NFA conversion incorrect'), nl
    ).

% Test 7: NFA with non-deterministic branching
% State 0 can go to either 1 or 2 on 'a'
test_nondeterministic_branching :-
    BranchNFA = fsa([0, 1, 2, 3], [a, b],
                    [trans(0, a, 1), trans(0, a, 2),
                     trans(1, b, 3), trans(2, b, 3)],
                    0, [3]),
    toDFA(BranchNFA, DFA),
    (   (deterministic(DFA), accepts(DFA, [a,b]))
    ->  write('Test 7 PASSED: Non-deterministic branching NFA converted'), nl
    ;   write('Test 7 FAILED: Branching NFA conversion incorrect'), nl
    ).

% Test 8: Already deterministic FSA (no epsilon, no branching)
% Converting DFA to DFA should preserve properties
test_dfa_to_dfa :-
    AlreadyDFA = fsa([0, 1], [a],
                     [trans(0, a, 1)],
                     0, [1]),
    toDFA(AlreadyDFA, ResultDFA),
    (   (deterministic(ResultDFA), accepts(ResultDFA, [a]))
    ->  write('Test 8 PASSED: DFA to DFA conversion works'), nl
    ;   write('Test 8 FAILED: DFA to DFA should preserve acceptance'), nl
    ).

% Test 9: NFA with multiple accept states
test_multiple_accept_states :-
    MultiAcceptNFA = fsa([0, 1, 2], [a, b],
                         [trans(0, a, 1), trans(0, b, 2)],
                         0, [1, 2]),
    toDFA(MultiAcceptNFA, DFA),
    (   (deterministic(DFA), accepts(DFA, [a]), accepts(DFA, [b]))
    ->  write('Test 9 PASSED: Multiple accept states handled correctly'), nl
    ;   write('Test 9 FAILED: Multiple accept states not handled'), nl
    ).

% Test 10: Complex NFA - accepts strings ending with "aba"
% Fixed: This NFA has no transitions from state 3, so strings must END with "aba"
test_complex_aba_nfa :-
    ABANFA = fsa([0, 1, 2, 3], [a, b],
                 [trans(0, a, 0), trans(0, b, 0),
                  trans(0, a, 1),
                  trans(1, b, 2),
                  trans(2, a, 3)],
                 0, [3]),
    toDFA(ABANFA, DFA),
    (   (deterministic(DFA),
         accepts(DFA, [a,b,a]),
         accepts(DFA, [b,a,b,a]),
         \+ accepts(DFA, [a,b,a,b]),  % Can't continue after reaching accept
         \+ accepts(DFA, [a,b]))
    ->  write('Test 10 PASSED: Complex "aba" ending NFA converted'), nl
    ;   write('Test 10 FAILED: Complex NFA conversion incorrect'), nl
    ).

% Test 11: NFA with epsilon cycle
test_epsilon_cycle :-
    EpsilonCycleNFA = fsa([0, 1, 2], [a],
                          [trans(0, epsilon, 1),
                           trans(1, epsilon, 2),
                           trans(2, epsilon, 0),
                           trans(1, a, 1)],
                          0, [1]),
    toDFA(EpsilonCycleNFA, DFA),
    (   deterministic(DFA)
    ->  write('Test 11 PASSED: Epsilon cycle NFA converted to DFA'), nl
    ;   write('Test 11 FAILED: Epsilon cycle caused issues'), nl
    ).

% Test 12: Verify DFA has no epsilon transitions
test_no_epsilon_in_dfa :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    DFA = fsa(_, _, Transitions, _, _),
    (   \+ member(trans(_, epsilon, _), Transitions)
    ->  write('Test 12 PASSED: DFA has no epsilon transitions'), nl
    ;   write('Test 12 FAILED: DFA should not have epsilon transitions'), nl
    ).

% Test 13: DFA accepts "bbbbabb"
test_accepts_bbbbabb :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   accepts(DFA, [b,b,b,b,a,b,b])
    ->  write('Test 13 PASSED: DFA accepts "bbbbabb"'), nl
    ;   write('Test 13 FAILED: DFA should accept "bbbbabb"'), nl
    ).

% Test 14: DFA rejects "abba"
test_rejects_abba :-
    example_fsa(NFA),
    toDFA(NFA, DFA),
    (   \+ accepts(DFA, [a,b,b,a])
    ->  write('Test 14 PASSED: DFA rejects "abba"'), nl
    ;   write('Test 14 FAILED: DFA should reject "abba"'), nl
    ).

% Test 15: Single state NFA accepting empty string
test_single_state_empty :-
    SingleNFA = fsa([0], [], [], 0, [0]),
    toDFA(SingleNFA, DFA),
    (   (deterministic(DFA), accepts(DFA, []))
    ->  write('Test 15 PASSED: Single state NFA accepting empty string'), nl
    ;   write('Test 15 FAILED: Single state NFA conversion failed'), nl
    ).

% Run all tests
run_all_tests :-
    write('Running toDFA (NFA to DFA conversion) tests'), nl,
    write('Testing subset construction algorithm'), nl, nl,
    test_example_to_dfa,
    test_accepts_abb,
    test_accepts_aabb,
    test_rejects_ab,
    test_rejects_empty,
    test_simple_epsilon_nfa,
    test_nondeterministic_branching,
    test_dfa_to_dfa,
    test_multiple_accept_states,
    test_complex_aba_nfa,
    test_epsilon_cycle,
    test_no_epsilon_in_dfa,
    test_accepts_bbbbabb,
    test_rejects_abba,
    test_single_state_empty,
    nl, write('All tests completed!'), nl.
