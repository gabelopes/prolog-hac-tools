:- module(hac_client, [
  hac_get/2,
  hac_post/2,
  hac_post/3,
  hac_post_multipart/2,
  hac_post_multipart/3
]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_cookie)).
:- use_module(library(http/json)).
:- use_module(library(ssl)).
:- use_module('../html/dom').
:- use_module(hac_settings).

:- dynamic csrf_token/1.

hac_get(Path, FormattedResponse) :-
  get_url(Path, URL),
  catch(
    http_get(URL, Response, [cert_verify_hook(ssl:cert_accept_any)]),
    Exception,
    handle_exception(Exception)
  ),
  format_response(Response, FormattedResponse), !.

hac_post(Path, Form) :-
  hac_post(Path, Form, _).

hac_post(Path, Form, Response) :-
  hac_post(Path, form, Form, Response).

hac_post_multipart(Path, Form) :-
  hac_post_multipart(Path, Form, _).

hac_post_multipart(Path, Form, Response) :-
  hac_post(Path, form_data, Form, Response).

hac_post(Path, FormType, Form, FormattedResponse) :-
  get_url(Path, URL),
  csrf_token(CSRFToken),
  format_data(Form, FormattedData),
  Data =.. [FormType, ['_csrf'=CSRFToken|FormattedData]],
  catch(
    http_post(URL, Data, Response, [cert_verify_hook(ssl:cert_accept_any)]),
    Exception,
    handle_exception(Exception)
  ),
  format_response(Response, FormattedResponse), !.

format_data([], []).
format_data([Name=Value|Options], [Name=Value|NormalOptions]) :-
  format_data(Options, NormalOptions).
format_data([Option|Options], [Name=Value|NormalOptions]) :-
  Option =.. [Name, Value],
  format_data(Options, NormalOptions).

format_response(Response, JSON) :-
  catch(atom_json_dict(Response, JSON, []), _, fail).
format_response(Response, DOM) :-
  catch(parse_html(Response, DOM), _, fail),
  save_csrf_token(DOM).
format_response(Response, Response).

handle_exception(_) :-
  writeln(user_error, "Could not connect to HAC, is Hybris server running?"),
  halt(3).

save_csrf_token(DOM) :-
  extract_csrf_token(DOM, CSRFToken),
  retractall(csrf_token(_)),
  assertz(csrf_token(CSRFToken)).
save_csrf_token(_).

extract_csrf_token(DOM, CSRFToken) :-
  query(DOM, "meta[name='_csrf']", [element(meta, Attributes, _)|_]),
  option(content(CSRFToken), Attributes).
