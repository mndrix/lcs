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


% BELOW experimental optimizations to discard identical regions

:- use_module(library(edcg)).
:- use_module(library(rbtrees)).

edcg:acc_info(left,X,In,Out,In=[X|Out]).
edcg:acc_info(right,X,In,Out,In=[X|Out]).
edcg:acc_info(stats,X,In,Out,update_stats(X,In,Out)).

edcg:pred_info(parts,1,[left,right,stats]).
edcg:pred_info(take_common_line,1,[left,right]).
edcg:pred_info(take_common_bytes,1,[left,right]).
edcg:pred_info(clear_rest_of_line,0,[left,right]).
edcg:pred_info(sync_on_unique_line,2,[left,right,stats]).
edcg:pred_info(common_line,1,[stats]).
edcg:pred_info(left_line,1,[left]).
edcg:pred_info(left_saw_line,2,[stats]).
edcg:pred_info(swap_left_right,0,[left,right,stats]).
edcg:pred_info(untangle_swaps,0,[left,right,stats]).
edcg:pred_info(is_swapped,0,[stats]).
edcg:pred_info(unique_on_right,2,[stats]).

parts(Left,Right,Parts) :-
    empty_stats(Stats),
    parts(Parts,Left,[],Right,[],Stats,_).

parts(Parts) -->>
    take_common_line(Line),
    common_line(Line),
    parts(Parts).
parts(Parts) -->>
    take_common_bytes(_Bytes),
    LeftDeparture/left,
    RightDeparture/right,
    clear_rest_of_line,
    sync_on_unique_line(LeftJoin,RightJoin),
    todo(Parts,LeftDeparture,RightDeparture,LeftJoin,RightJoin).

empty_stats(stats(straight,Left,Right)) :-
    rb_empty(Left),
    rb_empty(Right).

% left and right saw this common line
common_line(LineCodes) -->>
    string_codes(Line,LineCodes),
    stats(Xd,Left0,Right0)/stats,
    stats/stats(Xd,Left,Right),
    saw_line(Line,_,Left0,Left),
    saw_line(Line,_,Right0,Right).

saw_line(LineCodes,Position,Tree0,Tree) :-
    string_codes(Line,LineCodes),
    ( rb_lookup(Line,Occur,Tree0) ->
        ( Occur=multiple -> Tree=Tree0
        ; Occur=once(_) -> rb_update(Tree0,Line,multiple,Tree)
        )
    ; otherwise -> % first occurrence
        rb_insert(Tree0,Line,once(Position),Tree)
    ).

% fails if left and right share no leading lines
take_common_line([C|Line]) -->>
    [C]:left,
    [C]:right,
    C \= 0'\n,
    !,
    take_common_line(Line).
take_common_line([]) -->>
    C = 0'\n,
    [C]:left,
    [C]:right,
    !.
take_common_line([]) -->>
    []/left,
    []/right.

% unifies argument with leading bytes shared by left and right
% or [] if left and right share no bytes.
take_common_bytes([B|Bytes]) -->>
    [B]:left,
    [B]:right,
    !,
    take_common_bytes(Bytes).
take_common_bytes([]) -->>
    true.


clear_rest_of_line -->>
    insert(Left0,Left):left,
    take_line(_,Left0,Left),
    insert(Right0,Right):right,
    take_line(_,Right0,Right).

take_line([],[],[]).
take_line(Line,[C|Cs0],Cs) :-
    ( C = 0'\n ->
        Line=[],
        Cs=Cs0
    ; otherwise ->
        Line=[C|Line1],
        take_line(Line1,Cs0,Cs)
    ).

sync_on_unique_line([],[]) -->>
    []/left,
    []/right,
    !.
sync_on_unique_line(LeftJoin,RightJoin) -->>
    LeftJoin/left,
    left_line(Line),
    unique_on_right(Line,RightJoin),
    !,
    right/RightJoin,  % rewind right
    left/LeftJoin,    % rewind left
    untangle_swaps.
sync_on_unique_line(LeftJoin,RightJoin) -->>
    LeftMark/left,
    left_line(Line),
    \+ unique_on_right(Line,_),
    !,
    left_saw_line(Line,LeftMark),
    swap_left_right,
    sync_on_unique_line(LeftJoin,RightJoin).
sync_on_unique_line(LeftJoin,RightJoin) -->>
    \+ left_line(_),
    swap_left_right,
    sync_on_unique_line(LeftJoin,RightJoin).


left_line(Line) -->>
    insert(Left0,Left):left,
    take_line(Line,Left0,Left).

left_saw_line(Line,Position) -->>
    insert(stats(Xd,Tree0,R),stats(Xd,Tree,R)):stats,
    saw_line(Line,Position,Tree0,Tree).

swap_left_right -->>
    insert(Left,Right):left,
    insert(Right,Left):right,
    insert(stats(Xd0,L,R),stats(Xd,R,L)):stats,
    toggle_xd(Xd0,Xd).

toggle_xd(straight,crossed).
toggle_xd(crossed,straight).

untangle_swaps -->>
    is_swapped,
    !,
    swap_left_right.
untangle_swaps -->>
    \+ is_swapped,
    true.  % no swapping needed

is_swapped -->>
    stats(swapped,_,_)/stats.

unique_on_right(LineCodes,Position) -->>
    stats(_,_,R)/stats,
    string_codes(Line,LineCodes),
    rb_lookup(Line,once(Position),R).
