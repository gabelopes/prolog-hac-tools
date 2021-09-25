:- module(dcg, [
  parse/2,
  parse/3
]).

parse(Text, Goal, Rest) :-
  get_stream(Text, Stream),
  get_stream(Rest, RestStream), !,
  phrase(Goal, Stream, RestStream).

parse(Text, Goal) :-
  parse(Text, Goal, "").

get_stream(String, Stream) :-
  string(String),
  string_codes(String, Stream).
get_stream(Atom, Stream) :-
  atom(Atom),
  atom_codes(Atom, Stream).
get_stream(Number, Stream) :-
  number(Number),
  number_codes(Number, Stream).
