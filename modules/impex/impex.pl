:- module(impex, [
  validate_impex_import/2,
  validate_impex_import/3,
  import_impexes/2,
  import_impexes/3,
  import_impex_script/2,
  import_impex_script/3
  % export_impex/2,
  % export_impex/3,
  % export_impex_script/2,
  % export_impex_script/3
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

% default_export_settings([
%   encoding="UTF-8",
%   validationEnum='EXPORT_ONLY'
% ]).

% IMPORT

validate_impex_import(Content, Results) :-
  validate_impex_import(Content, [], Results).

validate_impex_import(Content, Options, Results) :-
  get_import_settings(Options, Settings),
  hac_post(validate_impex_import, [scriptContent(Content)|Settings], DOM),
  extract_results(DOM, Results).

import_impexes(Content, Results) :-
  import_impexes(Content, [], Results).

import_impexes([], _, []).
import_impexes([content(Impex, Content)|Contents], Options, [result(Impex, Result)|Results]) :-
  get_import_settings(Options, Settings),
  hac_post(import_impex, [scriptContent(Content)|Settings], DOM),
  extract_result(DOM, Result),
  import_impexes(Contents, Options, Results).

import_impex_script(ScriptPath, Result) :-
  import_impex_script(ScriptPath, [], Result).

import_impex_script(ScriptPath, Options, Result) :-
  absolute_file_name(ScriptPath, AbsoluteScriptPath),
  get_import_settings(Options, Settings),
  hac_post_multipart(import_impex_script, [file(file(AbsoluteScriptPath))|Settings], DOM),
  extract_result(DOM, Result).

get_import_settings(Options, Settings) :-
  default_import_settings(DefaultSettings),
  merge_options(Options, DefaultSettings, Settings).

% % EXPORT
% validate_impex_export(Content, Results) :-
%   validate_impex_export(Content, [], Results).

% validate_impex_export(Content, Options, Results) :-
%   get_export_settings(Options, Settings),
%   hac_post(validate_impex_export, [scriptContent(Content)|Settings], DOM),
%   extract_result(DOM, Results).

% export_impex(Content, Results) :-
%   export_impex(Content, [], Results).

% export_impex(Content, Options, Results) :-
%   get_export_settings(Options, Settings),
%   hac_post(export_impex, [scriptContent(Content)|Settings], DOM),
%   extract_result(DOM, Results).

% export_impex_script(ScriptPath, Results) :-
%   export_impex_script(ScriptPath, [], Results).

% export_impex_script(ScriptPath, Options, Results) :-
%   absolute_file_name(ScriptPath, AbsoluteScriptPath),
%   get_export_settings(Options, Settings),
%   hac_post_multipart(export_impex_script, [file(file(AbsoluteScriptPath))|Settings], DOM),
%   extract_result(DOM, Results).

% get_export_settings(Options, Settings) :-
%   default_export_settings(DefaultSettings),
%   merge_options(Options, DefaultSettings, Settings).

extract_result(DOM, errors(Errors)) :-
  query(DOM, ".impexResult > pre", [element(pre, _, Errors)|_]).
extract_result(_, success).
