:- module(hac_settings, [
  get_url/2,
  get_settings/1,
  get_credentials/2
]).

:- use_module(library(http/json)).
:- use_module(utility/dict).
:- use_module(cli).

default_settings(_{
  host: "https://localhost:9002",
  hac: _{
    path: "/hac",
    username: "admin",
    password: "nimda"
  }
}).

path(login, "/j_spring_security_check").
path(validate_impex_import, "/console/impex/import/validate").
path(import_impex, "./console/impex/import").
path(import_impex_script, "/console/impex/import/upload").
path(configurations, "/platform/config").
path(validate_configuration, "/platform/config/valuechanged").
path(store_configuration, "/platform/configstore").
path(delete_configuration, "/platform/configdelete").
path(clear_cache, "/monitoring/cache/regionCache/clear").

get_url(hac, URL) :-
  get_hac_url(URL).
get_url(Name, URL) :-
  path(Name, Path),
  get_hac_url(HAC),
  resolve_url([HAC, Path], URL).

resolve_url(Parts, URL) :-
  normalize_parts(Parts, NormalParts),
  atomics_to_string(NormalParts, "/", UnresolvedURL),
  uri_resolve('', UnresolvedURL, URL).

normalize_parts([], []).
normalize_parts([Part|Parts], NormalParts) :-
  atom_length(Part, 0),
  normalize_parts(Parts, NormalParts).
normalize_parts([Part|Parts], [NormalPart|NormalParts]) :-
  re_replace('(^\\/|\\/$)'/g, "", Part, NormalPart),
  normalize_parts(Parts, NormalParts).

get_settings(SettingsPattern) :-
  catch(read_file_to_string('settings.json', JSON, []), _, fail),
  atom_json_dict(JSON, Settings, [as(string)]),
  SettingsPattern :<< Settings.
get_settings(SettingsPattern) :-
  default_settings(Settings),
  SettingsPattern :<< Settings.

get_credentials(Username, Password) :-
  get_username(Username),
  get_password(Password).

get_username(Username) :-
  get_option(username, Username),
  Username \= ''.
get_username(Username) :-
  get_settings(_{ hac: _{ username: Username }}).

get_password(Password) :-
  get_option(password, Password),
  Password \= ''.
get_password(Password) :-
  get_settings(_{ hac: _{ password: Password }}).

get_hac_url(URL) :-
  get_option(hac, URL),
  URL \= ''.
get_hac_url(URL) :-
  get_settings(_{
    host: Host,
    hac: _{
      path: HACPath
    }
  }),
  resolve_url([Host, HACPath], URL).
