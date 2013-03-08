:- module(lcs, [ equality_metric/3
               , lcs/3
               , lcs/5
               ]).
:- use_module(library(quintus), [otherwise/0]).
/** <module> Longest common subsequence

Compute the longest common subsequence between two lists.
Elements can be compared by means of an arbitrary similarity metric.

*/

%%	lcs(+As:list, +Bs:list, -LCS:list) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	This is implemented in terms of lcs/5.
lcs(A, B, LCS) :-
    lcs(equality_metric, A, B, LCS_Pairs, _Length),
    maplist(fst, LCS_Pairs, LCS).

fst(X-_, X).


%%	lcs(+Cmp:callable,+As:list,+Bs:list,-LCS:list,-Length) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	Elements of As and Bs are compared by =|call(Cmp,A,B,Similarity)|=,
%	where =Similarity= should be between 0 and 1 inclusive.
%	Length is the sum of similarity scores for elements in the
%	subsequence.
%
%	This is currently implemented using a naive, exponential
%	algorithm with memoization.
%	Be careful with long lists or send a patch
%	to improve the algorithm :-)
lcs(Cmp,[A|As],[B|Bs],LCS,Length) :-
    !,
    call(Cmp, A, B, Similarity),
    lcs(Cmp,   As ,   Bs ,LCS_AB, Length_AB),
    lcs(Cmp,   As ,[B|Bs],LCS_A,  Length_A),
    lcs(Cmp,[A|As],   Bs ,LCS_B,  Length_B),
    ( Length_AB >= Length_A, Length_AB >= Length_B ->
        LCS = [A-B|LCS_AB],
        Length is Similarity + Length_AB
    ; Length_A >= Length_AB, Length_A >= Length_B ->
        LCS = LCS_A,
        Length is Length_A
    ; otherwise ->
        LCS = LCS_B,
        Length is Length_B
    ).
lcs(_,[],_,[],0) :- !.
lcs(_,_,[],[],0).


%%	equality_metric(+A, +B, -Similarity) is det.
%
%	Similarity is 1 if =|A == B|=, otherwise 0.  This predicate
%	is helpful as the first argument to lcs/5.
equality_metric(A,B,Similarity) :-
    ( A==B -> Similarity=1
    ; true -> Similarity=0
    ).
