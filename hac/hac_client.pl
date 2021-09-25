:- module(hac_client, [
  hac_get/2,
  hac_post/2,
  hac_post/3,
  hac_post_multipart/2,
  hac_post_multipart/3
]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_cookie)).
:- use_module(library(ssl)).
:- use_module('../html/dom').
:- use_module(hac_settings).

:- dynamic csrf_token/1.

hac_get(Path, ReponseHTML) :-
  get_url(Path, URL),
  catch(
    http_get(URL, ReponseHTML, [cert_verify_hook(ssl:cert_accept_any)]),
    Exception,
    handle_http_client_exception(Exception)
  ),
  save_csrf_token(ReponseHTML), !.

hac_post(Path, Form) :-
  hac_post(Path, Form, _).

hac_post(Path, Form, ResponseHTML) :-
  hac_post(Path, form, Form, ResponseHTML).

hac_post_multipart(Path, Form) :-
  hac_post_multipart(Path, Form, _).

hac_post_multipart(Path, Form, ResponseHTML) :-
  hac_post(Path, form_data, Form, ResponseHTML).

hac_post(Path, FormType, Form, ResponseHTML) :-
  get_url(Path, URL),
  csrf_token(CSRFToken),
  normalize_data(Form, NormalForm),
  Data =.. [FormType, ['_csrf'=CSRFToken|NormalForm]],
  catch(
    http_post(URL, Data, ResponseHTML, [cert_verify_hook(ssl:cert_accept_any)]),
    Exception,
    handle_http_client_exception(Exception)
  ),
  save_csrf_token(ResponseHTML), !.

normalize_data([], []).
normalize_data([Name=Value|Options], [Name=Value|NormalOptions]) :-
  normalize_data(Options, NormalOptions).
normalize_data([Option|Options], [Name=Value|NormalOptions]) :-
  Option =.. [Name, Value],
  normalize_data(Options, NormalOptions).

handle_http_client_exception(Exception) :-
  writeln(Exception),
  writeln(user_error, "Could not connect to HAC, is Hybris server running?"),
  halt(3).

save_csrf_token(HTML) :-
  extract_csrf_token(HTML, CSRFToken),
  abolish(csrf_token/1),
  assertz(csrf_token(CSRFToken)).

extract_csrf_token(HTML, CSRFToken) :-
  parse_html(HTML, DOM),
  query(DOM, "meta[name='_csrf']", [element(meta, Attributes, _)|_]),
  option(content(CSRFToken), Attributes).
extract_csrf_token(_, _) :-
  throw("Could not extract CSRF Token from HAC.").

