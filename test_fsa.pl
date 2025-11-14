% Tests all FSA operations

:- ['fsa.pl'].

% Simple test counter
:- dynamic(passed/1).
:- dynamic(failed/1).

init_tests :-
    retractall(passed(_)),
    retractall(failed(_)),
    assert(passed(0)),
    assert(failed(0)).

% Test runner
test(Name, Goal) :-
    format('~w: ', [Name]),
    (   call(Goal)
    ->  format('PASS~n'),
        retract(passed(N)), N1 is N + 1, assert(passed(N1))
    ;   format('FAIL~n'),
        retract(failed(N)), N1 is N + 1, assert(failed(N1))
    ).

summary :-
    passed(P), failed(F), Total is P + F,
    format('~n=== Results: ~w/~w passed ===~n', [P, Total]),
    (F = 0 -> format('All tests passed! ✓~n') ; format('~w tests failed~n', [F])).

% Test addState
test_addstate :-
    format('~n--- Testing addState ---~n'),
    test('Add state', (
        addState(fsa([0], [a], [], 0, []), 1, false, false, fsa([1,0|_], _, _, _, _))
    )),
    test('Add start state', (
        addState(fsa([0], [a], [], 0, []), 1, true, false, fsa(_, _, _, 1, _))
    )).

% Test addTransition
test_addtransition :-
    format('~n--- Testing addTransition ---~n'),
    test('Add transition', (
        addTransition(fsa([0,1], [a], [], 0, [1]), 0, a, 1, fsa(_, _, [trans(0,a,1)], _, _))
    )),
    test('Add new symbol', (
        addTransition(fsa([0,1], [a], [], 0, [1]), 0, b, 1, fsa(_, Alph, _, _, _)),
        member(b, Alph)
    )).

% Test closure
test_closure :-
    format('~n--- Testing closure ---~n'),
    FSA = fsa([0,1,2], [a], [trans(0,epsilon,1), trans(1,epsilon,2)], 0, [2]),
    test('Simple closure', (
        closure(FSA, 0, C),
        member(0, C), member(1, C), member(2, C)
    )),
    test('No epsilon closure', (
        closure(fsa([0,1], [a], [trans(0,a,1)], 0, [1]), 0, [0])
    )).

% Test next
test_next :-
    format('~n--- Testing next ---~n'),
    FSA = fsa([0,1,2], [a], [trans(0,epsilon,1), trans(1,a,2)], 0, [2]),
    test('Next with epsilon', (
        next(FSA, 0, a, N),
        member(2, N)
    )),
    test('Simple next', (
        next(fsa([0,1], [a], [trans(0,a,1)], 0, [1]), 0, a, [1])
    )).

% Test accepts
test_accepts :-
    format('~n--- Testing accepts ---~n'),
    test('Accept simple string', (
        accepts(fsa([0,1], [a], [trans(0,a,1)], 0, [1]), [a])
    )),
    test('Reject string', (
        \+ accepts(fsa([0,1], [a], [trans(0,a,1)], 0, [1]), [b])
    )),
    test('Accept empty string', (
        accepts(fsa([0], [a], [], 0, [0]), [])
    )).

% Test deterministic
test_deterministic :-
    format('~n--- Testing deterministic ---~n'),
    test('DFA is deterministic', (
        deterministic(fsa([0,1], [a], [trans(0,a,1)], 0, [1]))
    )),
    test('NFA with epsilon is not', (
        \+ deterministic(fsa([0,1], [a], [trans(0,epsilon,1)], 0, [1]))
    )),
    test('NFA with multiple transitions is not', (
        \+ deterministic(fsa([0,1,2], [a], [trans(0,a,1), trans(0,a,2)], 0, [1]))
    )).

% Test toDFA
test_todfa :-
    format('~n--- Testing toDFA ---~n'),
    test('Convert NFA to DFA', (
        toDFA(fsa([0,1,2], [a], [trans(0,epsilon,1), trans(1,a,2)], 0, [2]), DFA),
        deterministic(DFA)
    )).

% Main test runner
run_tests :-
    init_tests,
    format('~n=== FSA.PL TEST SUITE ===~n'),
    test_addstate,
    test_addtransition,
    test_closure,
    test_next,
    test_accepts,
    test_deterministic,
    summary.

% Run tests on load
:- initialization(run_tests).
