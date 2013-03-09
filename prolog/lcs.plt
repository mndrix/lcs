:- use_module(lcs, [lcs/3, lcs/5]).

:- begin_tests(lcs_3).

test(numbers) :-
    lcs("1234", "1224533324", LCS),
    LCS = "1234".

test(diddle_dum) :-
    lcs("beginning-middle-ending","beginning-diddle-dum-ending", LCS),
	LCS = "beginning-iddle-ending".

test(thisisatest) :-
    lcs("thisisatest", "testing123testing", LCS),
	LCS = "tsitest".

test(atoms) :-
    lcs([alpha,beta,gamma,delta],[alpha,bet,games], LCS),
	LCS = [alpha].

:- end_tests(lcs_3).



:- begin_tests(lcs_5).

vowel_metric(A,B,Similarity) :-
    Vowels = "aeiou",
    ( memberchk(A, Vowels), memberchk(B, Vowels) ->
        Similarity = 0.90
    ; true ->
        Similarity = 0.0
	).

test(moose) :-
    lcs(vowel_metric, "moose", "maase", LCS, Length),
    LCS = [0'o-0'a, 0'o-0'a, 0'e-0'e],
    Length =:= 2.7.

:- end_tests(lcs_5).
