:- module(table, [
  print_table/2
]).

print_table(Headers, Rows) :-
  determine_spacing([Headers|Rows], Spacing),
  print_headers(Spacing, Headers),
  print_rows(Spacing, Rows),
  print_border(Spacing).

print_headers(Spacing, Headers) :-
  print_border(Spacing),
  print_row(Spacing, Headers),
  print_border(Spacing).

print_rows(Spacing, Rows) :-
  forall(member(Row, Rows), print_row(Spacing, Row)).

print_row(Spacing, Row) :-
  forall(
    nth1(Index, Row, Element),
    (
      nth1(Index, Spacing, Space),
      string_length(Element, Length),
      Delta is Space - Length,
      repeat(Delta, ' ', Spaces),
      format("| ~w~w ", [Element, Spaces])
    )
  ),
  format("|~n").

print_border(Spacing) :-
  forall(
    member(Space, Spacing),
    (
      repeat(Space, '-', Border),
      format("+-~w-", [Border])
    )
  ),
  format("+~n").

determine_spacing([Row|Rows], Spacing) :-
  length(Row, Length),
  determine_spacing(Length, [Row|Rows], ReverseSpacing),
  reverse(ReverseSpacing, Spacing).

determine_spacing(0, _, []).
determine_spacing(Index, Rows, [Space|Spacing]) :-
  determine_space(Index, Rows, 0, Space),
  NewIndex is Index - 1,
  determine_spacing(NewIndex, Rows, Spacing).

determine_space(_, [], Space, Space).
determine_space(Index, [Row|Rows], PreviousSpace, Space) :-
  nth1(Index, Row, Element),
  string_length(Element, PartialSpace),
  PartialSpace > PreviousSpace,
  determine_space(Index, Rows, PartialSpace, Space).
determine_space(Index, [_|Rows], PreviousSpace, Space) :-
  determine_space(Index, Rows, PreviousSpace, Space).

repeat(Count, Content, Result) :-
  repeat(Count, Content, '', Result).

repeat(0, _, Result, Result).
repeat(Count, Content, PreviousResult, Result) :-
  atom_concat(PreviousResult, Content, PartialResult),
  NewCount is Count - 1,
  repeat(NewCount, Content, PartialResult, Result).
