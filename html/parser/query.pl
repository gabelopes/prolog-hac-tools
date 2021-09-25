:- module(query, [
  parse_query/2
]).

:- use_module('../../utility/dcg').
:- use_module(identifier).
:- use_module(whitespace, [
  whitespaces//0,
  whitespaces//1,
  whitespace//0
]).

simple_selector_operator(id) --> "#".
simple_selector_operator(class) --> ".".

combinator_operator(child) -->
  whitespaces,
  ">".
combinator_operator(adjacent) -->
  whitespaces,
  "+".
combinator_operator(sibling) -->
  whitespaces,
  "~".
combinator_operator(descendant) -->
  whitespaces([required]).

attribute_operator(equals) --> "=".
attribute_operator(contains) --> "*=".
attribute_operator(contains_word) --> "~=".
attribute_operator(starts_with) --> "^=".
attribute_operator(ends_with) --> "$=".
attribute_operator(equals_before_hyphen) --> "|=".

attribute_option(case_insensitive) --> "i"; "I".
attribute_option(case_sensitive) --> "s"; "S".

left_bracket --> "[".
right_bracket --> "]".

pseudo_selector_operator --> ":".
left_parenthesis --> "(".
right_parenthesis --> ")".

all_selector_operator --> "*".

selector_separator --> ",".

selectors([Selector|Selectors]) -->
  selector(Selector),
  selector_separator,
  selectors(Selectors).
selectors([Selector]) -->
  selector(Selector).

selector(Selector) -->
  selector(descendant, Selector).

selector(Type, selector(Type, SimpleSelectors, Combinator)) -->
  whitespaces,
  simple_selectors(SimpleSelectors),
  combinator(Combinator),
  whitespaces.

simple_selectors([SimpleSelector|SimpleSelectors]) -->
  simple_selector(SimpleSelector),
  simple_selectors(SimpleSelectors).
simple_selectors([SimpleSelector]) -->
  simple_selector(SimpleSelector).

simple_selector(SimpleSelector) -->
  simple_selector_operator(Type),
  identifier(Identifier),
  { SimpleSelector =.. [Type, Identifier] }.
simple_selector(attribute(Name, Operator, Value, Options)) -->
  left_bracket,
  whitespaces,
  identifier(Name),
  whitespaces,
  attribute_operator(Operator),
  whitespaces,
  quoted_identifier(Value),
  whitespaces,
  attribute_options(Options),
  whitespaces,
  right_bracket.
simple_selector(pseudo_selector(Name, Parameter)) -->
  pseudo_selector_operator,
  identifier(Name),
  pseudo_selector_parameter(Parameter).
simple_selector(all) --> all_selector_operator.
simple_selector(tag(Name)) --> identifier(Name).

attribute_options([Option|Options]) -->
  attribute_option(Option),
  attribute_options(Options).
attribute_options([]) --> "", !.

pseudo_selector_parameter(Parameter) -->
  left_parenthesis,
  whitespaces,
  simple_selector(Parameter),
  whitespaces,
  right_parenthesis.
pseudo_selector_parameter(none) --> "", !.

combinator(Combinator) -->
  combinator_operator(Type),
  whitespaces,
  selector(Type, Combinator).
combinator(none) --> "", !.

parse_query(Query, Selectors) :-
  parse(Query, query:selectors(Selectors)).
