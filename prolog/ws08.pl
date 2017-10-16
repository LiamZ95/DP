sumlist([], 0).
sumlist([H|T], Sum) :-
  sumlist(T, TailSum),
  sum is H + TailSum.

sumlist_acc([], Sum, Sum).
sumlist_acc([H|T], Acc, Sum) :-
  Acc1 is H + Acc,
  sumlist_acc(T, Acc1, Sum).

tree(empty).
tree(node(Left, _, Right)) :-
  tree(Left),
  tree(Right).

tree_list_app(empty, []).
tree_list_app(node(L, V, R), List) :-
  tree_list_app(L, LList),
  tree_list_app(R, RList),
  append(LList, [V|RList], List).

tree_list(Tree, List) :-
  tree_list_acc(Tree, [], List).

tree_list_acc(empty, List, List).
tree_list_acc(node(L, V, R), Acc, List) :-
  tree_list_acc(R, Acc, Acc2),
  Acc1 = [V|Acc2],
  tree_list_acc(L, Acc1, List).
  