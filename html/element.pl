:- module(element, [
  get_attribute/3
]).

get_attribute(element(_, [], _), _, _) :- fail.
get_attribute(element(_, [Name=Value|_], _), Name, Value).
get_attribute(element(_, [_|Attributes], _), Name, Value) :-
  get_attribute(element(_, Attributes, _), Name, Value).
