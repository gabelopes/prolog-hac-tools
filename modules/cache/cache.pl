:- module(cache, [
  clear_cache/0
]).

:- use_module('../../hac/hac_client').

clear_cache :-
  hac_post(clear_cache).
