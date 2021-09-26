start :-
  current_prolog_flag(argv, [ModuleName|_]),
  load_module(ModuleName, Module),
  Module:initialize, !,
  Module:run,
  halt.
start :-
  halt(2).

load_module(ModuleName, Module) :-
  atom_concat(ModuleName, '_cli', Module),
  use_module(modules/ModuleName/Module).
load_module(ModuleName, _) :-
  format("Could not find module '~w'.", [ModuleName]), nl.
