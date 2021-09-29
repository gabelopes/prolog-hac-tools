:- module(dict, [
  (':<<')/2,
  op(500, xfy, ':<<')
]).

:- use_module(library(http/json)).

':<<'(Selection, Dictionary) :-
  select_json(Selection, Dictionary).

select_json(Selection, JSONTerm) :-
  is_json_term(JSONTerm),
  atom_json_dict(JSONTerm, JSONDictionary, []), !,
  select_dictionary(Selection, JSONDictionary), !.
select_json(Selection, JSONDictionary) :-
  atom_json_dict(JSONTerm, JSONDictionary, []),
  select_json(Selection, JSONTerm), !.

select_dictionary([], []).
select_dictionary([Selector|Selectors], [Property|Properties]) :-
  select_dictionary(Selector, Property),
  select_dictionary(Selectors, Properties).
select_dictionary(Selection, Dictionary) :-
  is_dict(Dictionary),
  unify_selection(Selection, Dictionary).
select_dictionary([Property|Selectors], [Property|Properties]) :-
  select_dictionary(Selectors, Properties).
select_dictionary(Property, Property).
select_dictionary(_, _) :- !, fail.

unify_selection(Selection, Dictionary) :-
  dict_keys(Selection, Keys),
  unify_selection(Selection, Keys, Dictionary).

unify_selection(_, [], _).
unify_selection(Selection, [Key|Keys], Dictionary) :-
  get_dict(Key, Selection, Selector),
  get_dict(Key, Dictionary, Value),
  select_dictionary(Selector, Value),
  unify_selection(Selection, Keys, Dictionary).
unify_selection(_, _, _) :- !, fail.
