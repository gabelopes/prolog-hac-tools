:- module(config_cli, [
  initialize/0,
  run/0
]).

:- use_module('../../hac/hac_settings').
:- use_module('../../hac/hac').
:- use_module('../../cli').
:- use_module('../../utility/table').
:- use_module(config).

initialize :-
  register_path(configurations, "/platform/config"),
  register_path(validate_configuration, "/platform/config/valuechanged"),
  register_path(store_configuration, "/platform/configstore"),
  register_path(delete_configuration, "/platform/configdelete"),
  register_specifications([
    [opt(get), type(boolean), default(true), shortflags([g]), longflags([get])],
    [opt(set), type(boolean), default(false), shortflags([s]), longflags([set])],
    [opt(remove), type(boolean), default(false), shortflags([r]), longflags([remove])],
    [opt(list), type(boolean), default(false), shortflags([l]), longflags([list])],
    [opt(filter), type(boolean), default(false), shortflags([f]), longflags([filter])],
    [opt(format), type(boolean), default(false), shortflags(['F']), longflags([format])]
  ]).

run :-
  get_action(Action/Arity),
  get_arguments(Arguments),
  take(Arity, Arguments, TakenArguments),
  append(TakenArguments, [Result], Parameters),
  Predicate =.. [Action|Parameters], !,
  login, !,
  catch(call(Predicate), Exception, (output_exception(Exception), !, fail)),
  output_result(Action, Result),
  halt.

take(0, _, []).
take(Count, List, Elements) :-
  prefix(Elements, List),
  length(Elements, Count).

get_action(get_configurations/0) :-
  get_option(list, true).
get_action(filter_configurations/1) :-
  get_option(filter, true).
get_action(remove_configuration/1) :-
  get_option(remove, true).
get_action(set_configuration/2) :-
  get_option(set, true).
get_action(get_configuration/1) :-
  require_option(get, true).

output_result(get_configurations, Configurations) :-
  get_option(format, true),
  to_table_rows(Configurations, ConfigurationsRows),
  print_table(["Configuration", "Value"], ConfigurationsRows).
output_result(get_configurations, Configurations) :-
  maplist(write_configuration, Configurations).
output_result(filter_configurations, Configurations) :-
  get_option(format, true),
  to_table_rows(Configurations, ConfigurationsRows),
  print_table(["Configuration", "Value"], ConfigurationsRows).
output_result(filter_configurations, Configurations) :-
  maplist(write_configuration, Configurations).
output_result(get_configuration, Value) :-
  writeln(Value).
output_result(set_configuration, created) :-
  writeln("Configuration created.").
output_result(set_configuration, updated) :-
  writeln("Configuration updated.").
output_result(remove_configuration, success) :-
  writeln("Configuration removed.").

output_exception(Exception) :-
  writeln(user_error, Exception).

write_configuration(Key=Value) :-
  format("~w=~w", [Key, Value]), nl.

to_table_rows([], []).
to_table_rows([Key=Value|Pairs], [[Key, Value]|Rows]) :-
  to_table_rows(Pairs, Rows).
