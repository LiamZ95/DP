
% q1
correspond(E1, [E1|_], E2, [E2|_]).
correspond(E1, [Head1|Tail1], E2, [Head2|Tail2]) :-
  correspond(E1, Tail1, E2, Tail2).


% q2

interleave([], []).
interleave([X], X).
interleave([H|T], Y) :-
  is_equal([H|T]),
  length(H, L),
  merge_list([H|T], 1, L, Y).

is_equal([_]).
is_equal([X, Y|T]) :-
  length(X, Len),
  length(Y, Len),
  is_equal([Y|T]).

merge_list([H|T], Idx, Len, Y) :-
  (Len > 0 ->
  	Len1 is Len-1,
  	Idx1 is Idx+1,
  	get_all([H|T], Idx, Y1),
  	merge_list([H|T], Idx1, Len1, Y2),
  	append(Y1,Y2,Y)
  ; 
    Len=:=0,
    Y = []).

get_elem([], _, []).
get_elem([H|T], 1, [H]).
get_elem([H|T], Idx, [Y]) :-
  length([H|T], Len),
  Len >= Idx,
  Idx1 is Idx-1,
  get_elem(T, Idx1, [Y]).

get_all([], _, []).
get_all([H|T], Idx, Y) :-
  get_elem(H, Idx, Y1),
  get_all(T, Idx, Y2),
  append(Y1, Y2, Y).


% q3
partial_eval(E, Var, Val, R) :- number(E), R is E.
partial_eval(E, Var, Val, R) :- atom(E), (E \= Var -> R = E;R = Val).

partial_eval(X+Y, Var, Val, R) :-
 partial_eval(X, Var, Val, R1),
    partial_eval(Y, Var, Val, R2),
   ((number(R1), number(R2)) ->
   R is R1 + R2
  ; R = R1+R2).

partial_eval(X-Y, Var, Val, R) :-
 partial_eval(X, Var, Val, R1),
    partial_eval(Y, Var, Val, R2),
   ((number(R1), number(R2)) ->
   R is R1 - R2
  ; R = R1-R2).

partial_eval(X*Y, Var, Val, R) :-
 partial_eval(X, Var, Val, R1),
    partial_eval(Y, Var, Val, R2),
   ((number(R1), number(R2)) ->
   R is R1 * R2
  ; R = R1*R2).

partial_eval(X/Y, Var, Val, R) :-
 partial_eval(X, Var, Val, R1),
    partial_eval(Y, Var, Val, R2),
   ((number(R1), number(R2)) ->
   R is R1 / R2
  ; R = R1/R2).

partial_eval(X//Y, Var, Val, R) :-
 partial_eval(X, Var, Val, R1),
    partial_eval(Y, Var, Val, R2),
   ((number(R1), number(R2)) ->
   R is R1 // R2
  ; R = R1//R2). 
  