:- module(fs, [
  execute_search/2
]).

:- use_module('../../hac/hac_client').
:- use_module('../../cli').

execute_search(Query, Results) :-
  get_option(max, MaxCount),
  get_option(as, As),
  get_option(locale, Locale),
  get_option(commit, Commit),
  hac_post(flexible_search, [
    flexibleSearchQuery=Query,
    sqlQuery='',
    maxCount=MaxCount,
    user=As,
    locale=Locale,
    commit=Commit
  ], Results).
