:- module(lcs, [ equality_metric/3
               , lcs/3
               , lcs/5
               ]).
:- use_module(library(quintus), [otherwise/0]).
/** <module> Longest common subsequence

Compute a longest common subsequence between two lists.
Elements can be compared by means of an arbitrary similarity metric.

*/

%%	lcs(+As:list, +Bs:list, -LCS:list) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	Elements =A= and =B= are can be common if =|A==B|=.
%
%	Implemented in terms of lcs/5.
lcs(A, B, LCS) :-
    lcs(equality_metric, A, B, LCS_Pairs, _Length),
    maplist(fst, LCS_Pairs, LCS).

fst(X-_, X).

% Place to stored memoized lcs/5 results
:- dynamic lcs_cache/3.

%%	lcs(+Cmp:callable,+As:list,+Bs:list,-LCS:list,-Length) is det.
%
%	True if LCS is a longest common subsequence of As and Bs.
%	LCS is a list of pairs =|A-B|= since Cmp allows non-identical
%	elements to be considered common.
%
%	Elements of As and Bs are compared by =|call(Cmp,A,B,Similarity)|=,
%	where larger =Similarity= values indicate more similar elements.
%	Length is the sum of similarity scores for elements in the
%	subsequence.
%
%	Implemented with memoization on top of a naive, exponential
%	algorithm.  It performs fairly well, but patches to use a better
%	algorithm are welcome.
:- meta_predicate lcs(3,+,+,-,-).
lcs(Cmp, As, Bs, LCS, Length) :-
    retractall(lcs_cache(_,_,_)),
    lcs_(Cmp,As,Bs,LCS,Length),
    retractall(lcs_cache(_,_,_)).

:- meta_predicate lcs_(3,+,+,-,-).
lcs_(Cmp, As, Bs, LCS, Length) :-
    term_hash((Cmp,As,Bs), Hash),
    lcs_cache(Hash, LCS, Length),
    !.
lcs_(Cmp,[A|As],[B|Bs],LCS,Length) :-
    !,
    call(Cmp, A, B, Similarity),
    lcs_(Cmp,   As ,   Bs ,LCS_AB, Length_AB0),
    lcs_(Cmp,   As ,[B|Bs],LCS_A,  Length_A),
    lcs_(Cmp,[A|As],   Bs ,LCS_B,  Length_B),
    Length_AB is Similarity + Length_AB0,
    ( Length_A >= Length_AB, Length_A >= Length_B ->
        LCS = LCS_A,
        Length is Length_A
    ; Length_B >= Length_AB, Length_B >= Length_A ->
        LCS = LCS_B,
        Length is Length_B
    ; otherwise ->
        LCS = [A-B|LCS_AB],
        Length = Length_AB
    ),
    term_hash((Cmp,[A|As],[B|Bs]), Hash),
    assert(lcs_cache(Hash, LCS, Length)).
lcs_(_,[],_,[],0) :- !.
lcs_(_,_,[],[],0).


%%	equality_metric(+A, +B, -Similarity) is det.
%
%	Similarity is 1 if =|A == B|=, otherwise 0.  This predicate
%	is helpful as the first argument to lcs/5.
equality_metric(A,B,Similarity) :-
    ( A==B -> Similarity=1
    ; true -> Similarity=0
    ).
