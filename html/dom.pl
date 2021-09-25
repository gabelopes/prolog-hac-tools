:- module(html, [
  parse_html/2,
  query/3
]).

:- use_module(parser/query).
:- use_module(selector).

parse_html(Html, DOM) :-
  new_memory_file(File),
  open_memory_file(File, write, WriteStream),
  write(WriteStream, Html),
  close(WriteStream),
  open_memory_file(File, read, ReadStream),
  load_html(stream(ReadStream), DOM, []),
  close(ReadStream),
  free_memory_file(File).

query(DOM, Query, Results) :-
  parse_query(Query, Selectors), !,
  select(DOM, Selectors, Results).
