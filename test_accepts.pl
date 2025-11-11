% Load the main FSA file
:- ['fsa.pl'].
:- ['example.pl'].

% Test 1: Accept "abb" (simplest valid string)
test_accept_abb :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b,b])
    ->  write('Test 1 PASSED: Accepts "abb"'), nl
    ;   write('Test 1 FAILED: Should accept "abb"'), nl
    ).

% Test 2: Accept "aabb" (with (a|b)* prefix)
test_accept_aabb :-
    example_fsa(FSA),
    (   accepts(FSA, [a,a,b,b])
    ->  write('Test 2 PASSED: Accepts "aabb"'), nl
    ;   write('Test 2 FAILED: Should accept "aabb"'), nl
    ).

% Test 3: Accept "babb" (with b prefix)
test_accept_babb :-
    example_fsa(FSA),
    (   accepts(FSA, [b,a,b,b])
    ->  write('Test 3 PASSED: Accepts "babb"'), nl
    ;   write('Test 3 FAILED: Should accept "babb"'), nl
    ).

% Test 4: Accept "ababb" (longer prefix)
test_accept_ababb :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b,a,b,b])
    ->  write('Test 4 PASSED: Accepts "ababb"'), nl
    ;   write('Test 4 FAILED: Should accept "ababb"'), nl
    ).

% Test 5: Accept "aaabbabbabb" (complex valid string)
test_accept_complex :-
    example_fsa(FSA),
    (   accepts(FSA, [a,a,a,b,b,a,b,b,a,b,b])
    ->  write('Test 5 PASSED: Accepts "aaabbabbabb"'), nl
    ;   write('Test 5 FAILED: Should accept "aaabbabbabb"'), nl
    ).

% Test 6: Reject "ab" (doesn't end with "abb")
test_reject_ab :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b])
    ->  write('Test 6 FAILED: Should reject "ab"'), nl
    ;   write('Test 6 PASSED: Correctly rejects "ab"'), nl
    ).

% Test 7: Reject "a" (single character)
test_reject_a :-
    example_fsa(FSA),
    (   accepts(FSA, [a])
    ->  write('Test 7 FAILED: Should reject "a"'), nl
    ;   write('Test 7 PASSED: Correctly rejects "a"'), nl
    ).

% Test 8: Reject "abba" (ends with "bba" not "abb")
test_reject_abba :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b,b,a])
    ->  write('Test 8 FAILED: Should reject "abba"'), nl
    ;   write('Test 8 PASSED: Correctly rejects "abba"'), nl
    ).

% Test 9: Reject empty string
test_reject_empty :-
    example_fsa(FSA),
    (   accepts(FSA, [])
    ->  write('Test 9 FAILED: Should reject empty string'), nl
    ;   write('Test 9 PASSED: Correctly rejects empty string'), nl
    ).

% Test 10: Reject "aba" (almost "abb")
test_reject_aba :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b,a])
    ->  write('Test 10 FAILED: Should reject "aba"'), nl
    ;   write('Test 10 PASSED: Correctly rejects "aba"'), nl
    ).

% Test 11: Accept "bbbbabb" (multiple b's in prefix)
test_accept_bbbbabb :-
    example_fsa(FSA),
    (   accepts(FSA, [b,b,b,b,a,b,b])
    ->  write('Test 11 PASSED: Accepts "bbbbabb"'), nl
    ;   write('Test 11 FAILED: Should accept "bbbbabb"'), nl
    ).

% Test 12: Reject "abbab" (doesn't end with "abb")
test_reject_abbab :-
    example_fsa(FSA),
    (   accepts(FSA, [a,b,b,a,b])
    ->  write('Test 12 FAILED: Should reject "abbab"'), nl
    ;   write('Test 12 PASSED: Correctly rejects "abbab"'), nl
    ).

% Run all tests
run_all_tests :-
    write('Running accepts tests for FSA: (a|b)*abb'), nl,
    write('This FSA accepts strings with any a/b sequence ending in "abb"'), nl, nl,
    test_accept_abb,
    test_accept_aabb,
    test_accept_babb,
    test_accept_ababb,
    test_accept_complex,
    test_reject_ab,
    test_reject_a,
    test_reject_abba,
    test_reject_empty,
    test_reject_aba,
    test_accept_bbbbabb,
    test_reject_abbab,
    nl, write('All tests completed!'), nl.
