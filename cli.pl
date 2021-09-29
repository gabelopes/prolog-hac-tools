:- module(cli, [
  register_specifications/1,
  get_arguments/1,
  get_option/2,
  get_options/2,
  require_option/2,
  extract_options/2,
  get_help/1,
  write_help/0,
  write_help/1
]).

:- dynamic specifications/1.
:- dynamic options/2.
:- dynamic dirty/0.

specifications([[
  opt(hac), type(atom), default(''), shortflags([h]), longflags([hac])
], [
  opt(username), type(atom), default(''), shortflags([u]), longflags([user, username])
], [
  opt(password), type(atom), default(''), shortflags([p]), longflags([password])
], [
  opt('Ignored.Prolog.Flag'), type(boolean), default(false), shortflags([q])
]]).

dirty.

register_specifications(AdditionalSpecifications) :-
  specifications(CurrentSpecifications),
  append(CurrentSpecifications, AdditionalSpecifications, Specifications),
  retractall(specifications(_)),
  register_specifications(Specifications).
register_specifications(Specifications) :-
  assertz(specifications(Specifications)).

parse_options :-
  dirty,
  current_prolog_flag(argv, [_|Arguments]),
  specifications(Specifications),
  opt_parse(Specifications, Arguments, Options, PositionalArguments),
  retractall(options(_, _)),
  assertz(options(Options, PositionalArguments)),
  retractall(dirty).
parse_options.

get_arguments(Arguments) :-
  get_options(_, Arguments).

get_option(Name, Value) :-
  get_options(Options, _), !,
  Option =.. [Name, Value],
  option(Option, Options).

get_options(Options, PositionalArguments) :-
  parse_options,
  options(Options, PositionalArguments).
get_options([], []).

require_option(Option, Value) :-
  get_option(Option, Value).
require_option(_, _) :-
  write_help,
  halt(1).

extract_options(Names, ExtractedOptions) :-
  get_options(Options, _), !,
  findall(
    Option,
    (
      member(Option, Options),
      Option =.. [Name|_],
      member(Name, Names)
    ),
    ExtractedOptions
  ).

write_help :-
  current_output(OutputStream),
  write_help(OutputStream).

write_help(Stream) :-
  get_help(Help),
  writeln(Stream, Help).

get_help(Help) :-
  specifications(Specifications),
  opt_help(Specifications, Help).
