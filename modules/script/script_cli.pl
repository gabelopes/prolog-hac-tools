:- module(script_cli, [
  initialize/0,
  run/0
]).

:- use_module(library(unison)).
:- use_module('../../hac/hac_settings').
:- use_module('../../hac/hac').
:- use_module('../../utility/table').
:- use_module('../../cli').
:- use_module(script).

initialize :-
  register_path(scripting, "/console/scripting"),
  register_path(execute_script, "/console/scripting/execute"),
  register_path(load_script, "/console/scripting/load"),
  register_path(save_script, "/console/scripting/save"),
  register_path(delete_script, "/console/scripting/delete"),
  register_specifications([
    [opt(execute), type(boolean), default(true), shortflags([e]), longflags([execute])],
    [opt(view), type(boolean), default(false), shortflags([v]), longflags([view, cat])], % DONE
    [opt(list), type(boolean), default(false), shortflags([l]), longflags([list])], % DONE
    [opt(save), type(boolean), default(false), shortflags([s]), longflags([save])], % DONE
    [opt(delete), type(boolean), default(false), shortflags([d]), longflags([delete])], % DONE
    [opt(commit), type(boolean), default(false), shortflags([c]), longflags([commit])],
    [opt(scriptType), type(atom), default('groovy'), shortflags([t]), longflags([type])], % DONE
    [opt(scriptName), type(atom), default(''), shortflags([n]), longflags([name])], % DONE
    [opt(file), type(boolean), default(false), shortflags([f]), longflags([file])], % DONE
    [opt(showResult), type(boolean), default(false), shortflags(['R']), longflags(['show-result'])],
    [opt(showOutput), type(boolean), default(false), shortflags(['O']), longflags(['show-output'])],
    [opt(showStacktrace), type(boolean), default(false), shortflags(['S']), longflags(['show-stacktrace'])],
    [opt(output), type(atom), default('r'), shortflags([o]), longflags([output])]
  ]).

run :-
  login,
  execute_command.

execute_command :-
  get_option(view, true),
  view_script.
execute_command :-
  get_option(list, true),
  list_scripts.
execute_command :-
  get_option(save, true),
  save_script.
execute_command :-
  get_option(delete, true),
  delete_script.
execute_command :-
  require_option(execute, true),
  execute_script.

execute_script :-
  load_script(ScriptType, Script),
  script:execute_script(ScriptType, Script, JSON),
  output_results(JSON).

load_script(ScriptType, Script) :-
  get_option(scriptName, ScriptName),
  ScriptName \= '', !,
  script:load_script(ScriptName, ScriptType, Script).
load_script(ScriptType, Script) :-
  require_option(scriptType, ScriptType),
  get_script(Script).

view_script :-
  get_script_name(ScriptName),
  script:load_script(ScriptName, _, Script),
  writeln(Script).

list_scripts :-
  require_option(scriptType, ScriptType),
  script:list_scripts(ScriptType, Scripts),
  output_scripts(ScriptType, Scripts).

save_script :-
  require_option(scriptName, ScriptName),
  require_option(scriptType, ScriptType),
  get_script(Script),
  script:save_script(ScriptName, ScriptType, Script, Result),
  writeln(Result).

delete_script :-
  get_script_name(ScriptName),
  script:delete_script(ScriptName, Result),
  writeln(Result).

get_script(Script) :-
  get_option(file, true),
  get_arguments([Filename|_]),
  read_file_to_string(Filename, Script, []).
get_script(Script) :-
  get_arguments([Script|_]).

get_script_name(ScriptName) :-
  get_option(name, ScriptName),
  ScriptName \= ''.
get_script_name(ScriptName) :-
  get_arguments([ScriptName|_]).
get_script_name(_) :-
  writeln("Script name cannot be empty."),
  halt(2).

output_scripts(ScriptType, Scripts) :-
  length(Scripts, ScriptsLength),
  format("Found ~w ~w scripts: ~n", [ScriptsLength, ScriptType]),
  output_scripts(Scripts).

output_scripts([Script]) :-
  _{ name: ScriptName } :< Script,
  format(" \u2514\u2500 ~w~n", [ScriptName]).
output_scripts([Script|Scripts]) :-
  _{ name: ScriptName } :< Script,
  format(" \u251C\u2500 ~w~n", [ScriptName]),
  output_scripts(Scripts).

output_results(JSON) :-
  get_option(showResult, true),
  output_result(JSON, executionResult).
output_results(JSON) :-
  get_option(showOutput, true),
  output_result(JSON, outputText).
output_results(JSON) :-
  get_option(showStacktrace, true),
  output_result(JSON, stacktraceText).
output_results(JSON) :-
  get_output_properties(Properties),
  maplist(output_result(JSON), Properties).

output_result(JSON, Property) :-
  get_dict(Property, JSON, Result),
  writeln(Result).

get_output_properties(Properties) :-
  get_option(output, OutputRaw),
  string_lower(OutputRaw, Output),
  string_chars(Output, Characters),
  get_output_properties(Characters, Properties).

get_output_properties([], []).
get_output_properties([Character|Characters], [Property|Properties]) :-
  output_property(Character, Property),
  get_output_properties(Characters, Properties).

output_property(r, executionResult).
output_property(o, outputText).
output_property(s, stacktraceText).
