:- module(impex, [
  validate_impex_import/2,
  validate_impex_import/3,
  import_impex/2,
  import_impex/3,
  import_impex_script/2,
  import_impex_script/3
]).

:- use_module(html/dom).
:- use_module('../../hac/hac_client').

default_import_settings([
  encoding="UTF-8",
  maxThreads=16,
  validationEnum='IMPORT_STRICT',
  '_legacyMode'=false,
  '_enableCodeExecution'=false,
  '_distributedMode'=false,
  '_sldEnabled'=false
]).

validate_impex(Content, Results) :-
  validate_impex(Content, [], Results).


validate_impex_import(Content, Results) :-
  validate_impex_import(Content, [], Results).

validate_impex_import(Content, Options, Results) :-
  get_import_settings(Options, Settings),
  hac_post(validate_impex_import, [scriptContent(Content)|Settings], DOM),
  extract_results(DOM, Results).

import_impex(Content, Results) :-
  import_impex(Content, [], Results).

import_impex(Content, Options, Results) :-
  get_import_settings(Options, Settings),
  hac_post(import_impex, [scriptContent(Content)|Settings], DOM),
  extract_results(DOM, Results).

import_impex_script(ScriptPath, Results) :-
  import_impex_script(ScriptPath, [], Results).

import_impex_script(ScriptPath, Options, Results) :-
  absolute_file_name(ScriptPath, AbsoluteScriptPath),
  get_import_settings(Options, Settings),
  hac_post_multipart(import_impex_script, [file(file(AbsoluteScriptPath))|Settings], DOM),
  extract_results(DOM, Results).

get_import_settings(Options, Settings) :-
  default_import_settings(DefaultSettings),
  merge_options(Options, DefaultSettings, Settings).

extract_results(DOM, errors(Results)) :-
  query(DOM, ".impexResult > pre", [element(pre, _, Results)|_]).
extract_results(_, success).
