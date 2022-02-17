:- module(impex_cli, [
  initialize/0,
  run/0
]).

:- use_module('../../hac/hac_settings').
:- use_module('../../hac/hac').
:- use_module('../../cli').
:- use_module(impex).

initialize :-
  register_path(validate_impex_import, "/console/impex/import/validate"),
  register_path(import_impex, "./console/impex/import"),
  register_path(import_impex_script, "/console/impex/import/upload"),
  % register_path(export_impex, "/console/impex/export"),
  % register_path(export_impex_script, "/console/impex/export"),
  register_specifications([
    [opt(import), type(boolean), default(true), shortflags([i]), longflags([import])],
    [opt(import_script), type(boolean), default(false), shortflags([s]), longflags(['import-script'])],
    [opt(validate), type(boolean), default(false), shortflags([v]), longflags([validate])],
    [opt(raw), type(boolean), default(true), shortflags([r]), longflags([raw])],
    [opt(file), type(boolean), default(false), shortflags([f]), longflags([file])],
    [opt(encoding), type(atom), default('UTF-8'), shortflags([e]), longflags([encoding])],
    [opt(maxThreads), type(integer), default(16), shortflags([t]), longflags(['max-threads'])],
    [opt(validationEnum), type(atom), default('IMPORT_STRICT'), shortflags(['V']), longflags(['validation-type'])],
    [opt('_legacyMode'), type(boolean), default(false), shortflags([l]), longflags(['legacy-mode'])],
    [opt('_enableCodeExecution'), type(boolean), default(false), shortflags([x]), longflags(['enable-code-execution'])],
    [opt('_distributedMode'), type(boolean), default(false), shortflags([d]), longflags(['distributed-mode'])],
    [opt('_sldEnabled'), type(boolean), default(false), shortflags(['P']), longflags(['direct-persistence'])]
  ]).

run :-
  get_action(Action),
  get_content(Content),
  extract_options([encoding, maxThreads, validationEnum, '_legacyMode', '_enableCodeExecution', '_distributedMode', '_sldEnabled'], Options),
  Predicate =.. [Action, Content, Options, Results],
  login, !,
  call(Predicate),
  output_results(Action, Results).

get_action(validate_impex_import) :-
  get_option(validate, true).
get_action(import_impex_script) :-
  get_option(import_script, true).
get_action(import_impex) :-
  require_option(import, true).

get_content(Content) :-
  get_option(file, true),
  get_arguments([Filename|_]),
  read_file_to_string(Filename, Content, []).
get_content(Content) :-
  require_option(raw, true),
  get_arguments([Content|_]).

output_results(import_impex, success) :-
  writeln("Import finished successfully").
output_results(import_impex_script, success) :-
  writeln("Import finished successfully").
output_results(validate_impex_import, success) :-
  writeln("Import script is valid").
output_results(_, errors(Errors)) :-
  maplist(writeln(user_error), Errors).
