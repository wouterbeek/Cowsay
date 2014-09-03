% Standalone startup for the cowsay application.

% Avoid errors when using gtrace/0 in threads.
:- initialization guitracer.

:- use_module(library(optparse)).

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- set_prolog_flag(verbose, silent).

:- initialization(cowsay).

cowsay:-
  opt_arguments(
    [[default(false),longflags([debug]),opt(debug),type(boolean)]],
    _,
    Atoms
  ),
  atomics_to_string(Atoms, String),
  cowsay(String),
  halt.

