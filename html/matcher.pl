:- module(matcher, [
  match/3,
  matches/4
]).

:- use_module(library(dialect/ifprolog), [atom_split/3]).

matches(_, _, [], []).
matches(Siblings, Element, [selector(Type, SimpleSelectors, Combinator)|Selectors], [selector(Type, SimpleSelectors, Combinator)|Matches]) :-
  match(Siblings, Element, SimpleSelectors),
  matches(Siblings, Element, Selectors, Matches).
matches(Siblings, Element, [_|Selectors], Matches) :-
  matches(Siblings, Element, Selectors, Matches).

match(_, _, []).
match(Siblings, element(Tag, Attributes, _), [Selector|Selectors]) :-
  match_selector(Siblings, Tag, Attributes, Selector),
  match(Siblings, element(Tag, Attributes, _), Selectors).

match_selector(_, _, _, all).

match_selector(_, Tag, _, tag(Tag)).

match_selector(_, _, Attributes, id(Id)) :-
  get_attribute(Attributes, id, Id).

match_selector(_, _, Attributes, class(ClassName)) :-
  get_attribute(Attributes, class, Class),
  atom_split(Class, ' ', Classes),
  member(ClassName, Classes).

match_selector(_, _, Attributes, attribute(Name, Operator, ComparingValue, _)) :-
  get_attribute(Attributes, Name, Value),
  compare_attribute_value(Value, Operator, ComparingValue).

compare_attribute_value(Value, equals, Value).
compare_attribute_value(Value, contains, ComparingValue) :-
  sub_atom(Value, _, _, _, ComparingValue).
compare_attribute_value(Value, contains_word, ComparingValue) :-
  atom_split(Value, ' ', Words),
  member(ComparingValue, Words).
compare_attribute_value(Value, starts_with, ComparingValue) :-
  atom_concat(ComparingValue, _, Value).
compare_attribute_value(Value, ends_with, ComparingValue) :-
  atom_concat(_, ComparingValue, Value).
compare_attribute_value(Value, equals_before_hyphen, ComparingValue) :-
  atom_split(Value, '-', [ComparingValue|_]).

get_attribute([], _, _) :- fail.
get_attribute([Name=Value|_], Name, Value).
get_attribute([_|Attributes], Name, Value) :-
  get_attribute(Attributes, Name, Value).
