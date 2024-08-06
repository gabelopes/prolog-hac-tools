:- use_module(cli).

start :-
  current_prolog_flag(argv, [Module|_]),
  get_module(Module, _, Initialize, Run),
  call((Initialize, !)),
  catch(
    call(Run),
    Exception,
    handle_exception(Exception)
  ),
  halt.
start :-
  write_help,
  halt(2).

handle_exception(error(ErrorMessage)) :-
  write(user_error, ErrorMessage),
  halt(1).
handle_exception(error) :-
  halt(1).
handle_exception(help(ErrorMessage)) :-
  write(user_error, ErrorMessage),
  write_help,
  halt(2).
handle_exception(help) :-
  write_help,
  halt(2).

include_modules :-
  directory_files(modules, [_, _|Modules]),
  maplist(include_module, Modules).

include_module(Name) :-
  get_module(Name, CLI, Initialize, Run),
  use_module(modules/Name/CLI, [
    initialize/0 as Initialize,
    run/0 as Run
  ]).
include_module(Name, _) :-
  format("Could not find module '~w'.", [Name]), nl.

get_module(Name, CLI, Initialize, Run) :-
  atom_concat(Name, '_cli', CLI),
  atom_concat(Name, '_initialize', Initialize),
  atom_concat(Name, '_run', Run).

:- include_modules.
