list_of(Elt, [Elt]).
list_of(Elt, [Elt|T]) :-
  list_of(Elt, T).

all_same([]).
all_same([_Head]).
all_same([H|T]) :-
  list_of(H, T).

adjacent(E1, E2, L) :-
  append(_, [E1, E2|_], L)

adjacent1(E1, E2, [E1, E2|_]).
adjacent1(E1, E2, [_|T]) :-
  adjacent1(E1, E2, T).


insert(K, empty, tree(empty, K, empty)).
insert(K, tree(L, V, R), tree(L1, V, R1)) :-
  (K < V -> insert(K, L, L1), R = R1
  	;
  	insert(K, R, R1), L = L1
  	).

list_tree([], empty)
list_tree([H|T], Tree) :-

list_tree_acc([], Tree, Tree).
list_tree_acc([H|T], Acc, Out) :-
  insert(H, Acc, Acc1),
  list_tree_acc(T, Acc1, Out)

