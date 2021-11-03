:- module(config, [
  get_configurations/1,
  filter_configurations/2,
  get_configuration/2,
  set_configuration/3,
  remove_configuration/2
]).

:- use_module(library(unison)).
:- use_module('../../hac/hac_client').
:- use_module('../../html/dom').

get_configurations(Configurations) :-
  hac_get(configurations, DOM),
  query(DOM, "#props input.configValue", Inputs),
  extract_configurations(Inputs, Configurations), !.
get_configurations(_) :-
  throw("Could not retrieve configurations.").

filter_configurations(Keyword, FilteredConfigurations) :-
  get_configurations(Configurations),
  findall(
    Key=Value,
    (
      member(Key=Value, Configurations),
      sub_string(Key, _, _, _, Keyword)
    ),
    FilteredConfigurations
  ).

get_configuration(Key, Value) :-
  get_configurations(Configurations),
  member(Key=Value, Configurations).
get_configuration(Key, _) :-
  format(string(Exception), "Could not find configuration for key '~w'.", [Key]),
  throw(Exception).

set_configuration(Key, Value, Status) :-
  validate_configuration(Key, Value, Status),
  hac_post(store_configuration, [key(Key), val(Value)]).
set_configuration(Key, Value, _) :-
  format(string(Exception), "Invalid configuration '~w' => '~w'.", [Key, Value]),
  throw(Exception).

validate_configuration(Key, Value, Status) :-
  hac_post(validate_configuration, [key(Key), val(Value)], JSON),
  _{ validationError: false } :<< JSON, !,
  get_validation_status(JSON, Status).

get_validation_status(JSON, created) :-
  _{ isNew: true } :<< JSON.
get_validation_status(JSON, updated) :-
  _{ changed: true } :<< JSON.

remove_configuration(Key, success) :-
  hac_post(delete_configuration, [key(Key)]).
remove_configuration(Key, _) :-
  format(string(Exception), "Could not remove configuration '~w'.", [Key]),
  throw(Exception).

extract_configurations([], []).
extract_configurations([element(_, Attributes, _)|Inputs], [Key=Value|Configurations]) :-
  option(name(Key), Attributes),
  option(value(Value), Attributes),
  extract_configurations(Inputs, Configurations).
