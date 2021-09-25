:- module(hac, [
  login/0,
  login/2
]).

:- use_module(hac_settings).
:- use_module(hac_client).

login :-
  get_credentials(Username, Password),
  login(Username, Password).

login(Username, Password) :-
  create_session,
  hac_post(login, [
    j_username=Username,
    j_password=Password
  ]).
login(_, _) :-
  throw("Failed to login to HAC.").

create_session :-
  hac_get(hac, _).
