:- module(whitespace, [
  space//0,
  tabulation//0,
  new_line//0,
  whitespace//0,
  whitespace//1,
  whitespaces//0,
  whitespaces//1
]).

space --> " ".
space --> "\f".

tabulation --> "\t".
tabulation --> "\v".

new_line --> "\r\n".
new_line --> "\n".
new_line --> "\r".

%! whitespace(++Exceptions) is semidet
%
%  Parse a whitespace using the module's =space=, =tabulation= or =new_line= rule.
%  To disallow specific whitespace rules, pass in a list of =no_<rule>= atoms via
%  =Exceptions= argument. For example, to exclude a new line from being parsed,
%  pass in =[no_new_line]=.
%
%  Rules are, namely:
%  * space: blank, \f
%  * tabulation: \t, \v
%  * new_line: \r\n, \n, \r
%
%  Calling =whitespace([])= is identic to calling =whitespace=.
%
%  @arg Exceptions a list of rule disallowances.
whitespace(Exceptions) -->
  { \+ member(no_space, Exceptions) },
  space.
whitespace(Exceptions) -->
  { \+ member(no_tabulation, Exceptions) },
  tabulation.
whitespace(Exceptions) -->
  { \+ member(no_new_line, Exceptions) },
  new_line.

whitespace --> whitespace([]).

%! whitespaces(++Options) is semidet
%
%  Greedily consumes whitespaces using the module's =space=, =tabulation= and =new_line= rules.
%  By default, this rule allows for no spaces to be consumed and still succeed. Thus, to
%  require at least one whitespace to be consumed, pass in the =required= atom via Options
%  argument.
%
%  To disallow specific whitespace rules, pass in a list of =no_<rule>= atoms via
%  =Options= argument (@see whitespace//1).
%
%  Calling =whitespaces([])= is identic to calling =whitespaces=.
%
%  @arg Options a list of options and/or exceptions
whitespaces(Options) -->
  whitespace(Options),
  whitespaces.
whitespaces(Options) -->
  { member(required, Options) }, !,
  whitespace(Options).
whitespaces(_) -->
  "", !.

whitespaces --> whitespaces([]).
