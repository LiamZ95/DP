puzzle_solution([[100,14,10,35], [14,_,_,_], [15,_,_,_],[18,_,1,_]])

puzzle_solution([[100,14,10,25], [14,7,2,1],[15,3,7,5],[28,4,1,7]]).

[[100,14,10,25], [14,7,2,1],[15,3,7,5],[28,4,1,7]]
[[14,7,2,1],[15,3,7,5],[28,4,1,7]]


same_diagonal_line([_Head|Tail]):-
  Tail = [Thead|Ttail],
  between(1,9,Diagoal_line),
  nth1(2,Thead,Diagoal_line),
  same(3,Ttail,Diagoal_line).
same(_,[],_).

%this predicate is for compare diagonal line elements and return true if they are the same
same(N,List,Diagoal_line):-
  List = [Head|Tail],
  nth1(N,Head,Diagoal_line),
  N1 is N + 1,
  same(N1,Tail,Diagoal_line).
