%  File     	: Proj2.pl
%  Author   	: Liyu Zhang
%  Student Id	: 842191
%  Origin   	: Mon Oct 16 2017
%  Purpose  	: This program is my solution to the puzzle problem in COMP90048 project2.

% module for transpose function
:- ensure_loaded(library(clpfd)).

% This function can solve the puzzle, all other predicates are used for this
% one.
% It works in the following procedure:
% 1. It firstly check whether elements on the diagonal are the same
% 2. It then checks whether all elements in each row are disctinct frim each other
% 3. It then checks whenther the first element in the list is the sum or product of
%    all other element in the list.
% 4. Transpose the list
% 5. Do 2 and 3 again.
% It does not need to check diagonal again since transpose won't change their value.
puzzle_solution(Rows) :-
    Rows = [_RH|NewRows],
    check_diagonal(NewRows, 1, _DiaValue),
    maplist(all_distinct, NewRows),
    maplist(is_sum_or_product, NewRows),

    % transpose the puzzle
    transpose(Rows, Columns),

    Columns = [_CH|NewColumns],
    maplist(all_distinct, NewColumns),
    maplist(is_sum_or_product, NewColumns),
    maplist(label, Rows).


% Check if the elements except the top-left one in the diagonal line of the puzzle are
% the same
check_diagonal([], _, _).
check_diagonal([FstRow|RestRows], Index, DiaValue) :-
    between(1, 9, DiaValue),
    nth0(Index, FstRow, DiaValue),
    Index2 is Index+1,
    check_diagonal(RestRows, Index2, DiaValue).


% predicate that the first element in the row is the sum or product of
% other elements in the list
is_sum_or_product(Row) :-
    get_head(Row, ListHead),
    Row = [_RH|Rest],
    (is_sum(Rest, ListHead);
    is_product(Rest, ListHead)).


% Check if argument Pro is the product of all elements in the list
is_product([], 1).
is_product([H|T], Pro) :-
    H #> 0,
    H #< 10,
    is_product(T, Pro2),
    Pro #= Pro2 * H.


% Check if argument Sum is the sum of all elements in the list
is_sum([], 0).
is_sum([H|T], Sum) :-
    H #> 0,
    H #< 10,
    is_sum(T, Sum2),
    Sum #= Sum2 + H.


% Get a new list without the first element of original list
strip_first([], []).
strip_first([_|T], T).


% Get the head element of a list
get_head([H|_], H).
