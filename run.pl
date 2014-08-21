% Standalone startup for the cowsay application.

:- use_module(library(optparse)).

:- if(current_prolog_flag(argv, ['--debug'])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- use_module(cowsay(cowsay)).

:- set_prolog_flag(verbose, silent).

:- initialization(cowsay).

cowsay:-
  opt_arguments([], _, Message),
  cowsay(Message),
  halt.

