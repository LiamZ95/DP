% workshop 9

same_elements(L1, L2) :-
  every_element_in(L1, L2),
  every_element_in(L2, L1).

every_element_in([], _).
every_element_in([H|T], L2) :-
  member(H, L2),
  every_element_in(T, L2).


same_elements1(L1, L2) :-
  sort(L1, S),
  sort(L2, S).


times(W, X, Y, Z) :-
  (integer(W), integer(X), integer(Y) ->
    Z is W*X + Y
    ;
    integer(Z), integer(W) ->
      X is Z div W,
      Y is Z mod W
    ;
    integer(Z), integer(X) ->
      W is Z // X,
      Y is Z rem X
    ;
    throw(error(instantiation_error,
    context(times/4,_)))
  ).
