:- module(fs_cli, [
  initialize/0,
  run/0
]).

:- use_module(library(unison)).
:- use_module('../../hac/hac_settings').
:- use_module('../../hac/hac').
:- use_module('../../utility/table').
:- use_module('../../cli').
:- use_module(fs).

initialize :-
  register_path(flexible_search, "/console/flexsearch/execute"),
  register_specifications([
    [opt(header), type(boolean), default(false), shortflags(['H']), longflags([header])],
    [opt(commit), type(boolean), default(false), shortflags([c]), longflags([commit])],
    [opt(verbose), type(boolean), default(false), shortflags([v]), longflags([verbose])],
    [opt(format), type(boolean), default(false), shortflags([f]), longflags([format])],
    [opt(max), type(integer), default(200), shortflags([m]), longflags([max])],
    [opt(locale), type(atom), default(en), shortflags([l]), longflags([locale])],
    [opt(as), type(atom), default(admin), longflags([as])],
    [opt(separator), type(atom), default(','), shortflags([s]), longflags([separator])]
  ]).

run :-
  get_arguments([Query|_]),
  login,
  execute_search(Query, Results),
  output_results(Results).

output_results(Results) :-
  _{ exception: Exception } :<< Results,
  Exception \= null,
  output_exception(Exception).
output_results(Results) :-
  get_option(header, true),
  output_statistics(Results),
  output_result_header(Results).
output_results(Results) :-
  output_statistics(Results),
  output_result_list(Results).

output_statistics(Results) :-
  get_option(verbose, true),
  _{
    query: SQLQuery,
    executionTime: Time,
    resultCount: Count,
    catalogVersionsAsString: CatalogVersions,
    parametersAsString: Parameters
  } :<< Results,
  format("SQL Query:~n~w~n~n", [SQLQuery]),
  format("Execution time: ~w ms~n~n", [Time]),
  format("Result count: ~w~n~n", [Count]),
  format("Used catalog versions: ~w~n~n", [CatalogVersions]),
  format("Replaced parameters: ~w~n~n", [Parameters]).
output_statistics(_).

output_result_header(Results) :-
  _{ headers: Headers } :<< Results,
  as_result_list(Headers, ResultList),
  output_result_list(_{
    headers: ["Headers"],
    resultList: ResultList
  }).

as_result_list([], []).
as_result_list([Element|List], [[Element]|ResultList]) :-
  as_result_list(List, ResultList).

output_result_list(Results) :-
  get_option(format, true),
  _{ headers: Headers, resultList: ResultList } :<< Results,
  print_table(Headers, ResultList).
output_result_list(Results) :-
  get_option(separator, Separator),
  atom_codes(Separator, [SeparatorCode|_]),
  _{ resultList: ResultList } :<< Results,
  current_output(OutputStream),
  forall(
    member(Result, ResultList),
    (
      Row =.. [row|Result],
      csv_write_stream(OutputStream, [Row], [separator(SeparatorCode)])
    )
  ).

output_exception(Exception) :-
  _{ message: ExceptionMessage } :<< Exception,
  format(user_error, "Exception: ~w.~n", [ExceptionMessage]),
  halt(4).
