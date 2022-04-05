:- module(script, [
  execute_script/3,
  load_script/3,
  list_scripts/2,
  save_script/4,
  delete_script/2
]).

:- use_module(library(pcre)).
:- use_module(library(unison)).
:- use_module(library(http/json)).
:- use_module(html/dom).
:- use_module('../../hac/hac_client').
:- use_module('../../cli').

execute_script(ScriptType, Script, JSON) :-
  get_option(commit, Commit),
  hac_post(execute_script, [
    scriptType=ScriptType,
    script=Script,
    commit=Commit
  ], JSON).

load_script(ScriptName, ScriptType, Script) :-
  hac_post(load_script, [code=ScriptName], JSON),
  _{
    content: _{
      content: Script,
      engineName: ScriptType
    }
  } :<< JSON.

list_scripts(ScriptType, Scripts) :-
  hac_get(scripting, DOM),
  extract_scripts(ScriptType, DOM, Scripts).

save_script(ScriptName, ScriptType, Script, Result) :-
  hac_post(save_script, [
    code=ScriptName,
    scriptType=ScriptType,
    script=Script
  ], DOM),
  extract_save_result(DOM, Result).

delete_script(ScriptName, Result) :-
  hac_post(delete_script, [code=ScriptName], ResponseJSON),
  extract_delete_result(ResponseJSON, Result).

parse_json(JSONContent, JSON) :-
  re_replace("([{,]+\s*)['""]?(.+?)['""]?(\s*:)"/gm, "$1""$2""$3", JSONContent, Content),
  atom_json_dict(Content, JSON, []).

filter_scripts(ScriptType, [], _) :-
  format("Could not find any ~w scripts.", [ScriptType]),
  halt(1).
filter_scripts(ScriptType, [ScriptsByType|_], Scripts) :-
  atom_string(ScriptType, ScriptTypeName),
  _{ name: ScriptTypeName, children: Scripts } :< ScriptsByType.
filter_scripts(ScriptType, [_|ScriptsByType], Scripts) :-
  filter_scripts(ScriptType, ScriptsByType, Scripts).

extract_scripts(ScriptType, DOM, Scripts) :-
  query(DOM, "#tabs-browse > script[language='javascript']", [element(_, _, [ScriptsContent|_])|_]),
  re_matchsub("(?<array>\\[.+?\\])(?:;|$)"/m, ScriptsContent, Matches, []),
  _{ array: ArrayContent } :< Matches,
  parse_json(ArrayContent, [ScriptsRepository|_]),
  _{ children: ScriptsByType } :< ScriptsRepository,
  filter_scripts(ScriptType, ScriptsByType, Scripts).

extract_save_result(DOM, Result) :-
  query(DOM, "#uploadResult", [element(_, Attributes, _)|_]),
  option('data-result'(Result), Attributes).

extract_delete_result(ResponseJSON, "Script deleted successfully.") :-
  _{ exception: null } :< ResponseJSON.
extract_delete_result(ResponseJSON, Result) :-
  _{ exception: Exception } :< ResponseJSON,
  format(string(Result), "Could not delete script: ~w", [Exception]).
