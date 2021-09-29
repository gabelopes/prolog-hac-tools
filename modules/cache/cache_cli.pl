:- module(cache_cli, [
  initialize/0,
  run/0
]).

:- use_module('../../hac/hac_settings').
:- use_module('../../hac/hac').
:- use_module('../../cli').
:- use_module(cache).

initialize :-
  register_path(clear_cache, "/monitoring/cache/regionCache/clear"),
  register_specifications([
    [opt(clear), type(boolean), default(true), shortflags([c]), longflags([clear])]
  ]).

run :-
  login,
  clear_cache,
  writeln("Cache cleared.").
run :-
  writeln("Could not clear cache."),
  fail.
