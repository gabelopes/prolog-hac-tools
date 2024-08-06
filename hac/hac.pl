:- module(hac, [
  login/0,
  login/2
]).

:- use_module(hac_settings).
:- use_module(hac_client).
:- use_module('../html/dom').
:- use_module('../html/element').

login :-
  get_credentials(Username, Password), !,
  login(Username, Password).

login(Username, Password) :-
  create_session, !,
  hac_post(login, [
    j_username=Username,
    j_password=Password
  ], DOM), !,
  handle_response(DOM).
login(_, _) :-
  throw(error("Failed to login to HAC.")).

handle_response(DOM) :-
  query(DOM, "#loginErrors", [LoginErrorsDiv]),
  get_text(LoginErrorsDiv, LoginErrorsText), !,
  throw(error(LoginErrorsText)).
handle_response(_).

create_session :-
  hac_get(hac, _).
