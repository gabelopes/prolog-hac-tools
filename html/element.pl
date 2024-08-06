:- module(element, [
  get_attribute/3,
  get_text/2
]).

get_attribute(element(_, [], _), _, _) :- fail.
get_attribute(element(_, [Name=Value|_], _), Name, Value).
get_attribute(element(_, [_|Attributes], _), Name, Value) :-
  get_attribute(element(_, Attributes, _), Name, Value).

get_text(Text, Text) :-
  atom(Text);
  string(Text).
get_text(Element, Text) :-
  get_text(Element, "", Text).

get_text([], Text, Text).
get_text([Child|Children], CurrentText, Text) :-
  get_text(Child, ChildText),
  join_text(CurrentText, ChildText, NextText),
  get_text(Children, NextText, Text).
get_text(element(_, _, Children), CurrentText, Text) :-
  get_text(Children, CurrentText, Text).

join_text("", B, Text) :-
  format(string(Text), B, []).
join_text(A, B, Text) :-
  format(string(Text), "~w ~w", [A, B]).
