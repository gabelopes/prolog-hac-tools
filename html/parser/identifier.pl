:- module(identifier, [
  identifier//1,
  quoted_identifier//1
]).

:- use_module(library(pcre), [re_match/2]).

identifier(Identifier) -->
  alphanumerics(Alphanumerics),
  { atom_codes(Identifier, Alphanumerics) }, !.

alphanumerics([Alphanumeric|Rest]) -->
  alphanumeric(Alphanumeric),
  alphanumerics(Rest).
alphanumerics([Alphanumeric]) -->
  alphanumeric(Alphanumeric).

alphanumeric(Alphanumeric) -->
  [Alphanumeric],
  {
    char_code(Char, Alphanumeric),
    re_match("\\w|-", Char)
  }.

single_quote --> "'".
double_quote --> "\"".

quoted_identifier(Identifier) -->
  single_quote,
  identifier(Identifier),
  single_quote, !.
quoted_identifier(Identifier) -->
  double_quote,
  identifier(Identifier),
  double_quote, !.
