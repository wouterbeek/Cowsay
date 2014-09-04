#!/usr/bin/env swipl
% Standalone startup for the cowsay application.

% Avoid errors when using gtrace/0 in threads.
:- initialization guitracer.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- use_module(library(aggregate)).
:- use_module(library(optparse)).

:- use_module(pl(pl_log)).

:- set_prolog_flag(verbose, silent).

:- initialization(cowsay).

cowsay:-
  % Specify command-line arguments.
  aggregate_all(
    set(Mode),
    cow_mode(Mode),
    Modes
  ),
  Wraps = [line,none,word],
  OptionSpec = [
      [default(false),longflags([debug]),opt(debug),shortflags([d]),type(boolean)],
      [default(false),longflags([help]),opt(help),shortflags([h]),type(boolean)],
      [default(50),longflags([max_width]),opt(max_width),type(integer)],
      [longflags([message]),opt(message),shortflags([m]),type(string)],
      [longflags([mode]),opt(mode),type(oneof(Modes))],
      [default(true),longflags([speech]),opt(speech),type(boolean)],
      [default(word),longflags([wrap_mode]),opt(wrap),type(oneof(Wraps))]],

  % Process command-line arguments.
  run_print_messages(
    opt_arguments(OptionSpec, Options1, PositionalArgs),
    Status
  ),

  % We do not want to get stuck inside Prolog in case an exception is thrown.
  (   Status == true
  ->  true
  ;   halt
  ),

  % Process the help flag.
  select_option(help(Help), Options1, Options2),
  (   Help == true
  ->  opt_help(OptionSpec, Atom),
      Atoms = [Atom]
  ;   Atoms = PositionalArgs
  ),
  
  % Combine a message potentially specified via a flag
  % with positional arguments.
  select_option(message(Message), Options2, Options3),
  (   var(Message)
  ->  Atomics = Atoms
  ;   Atomics = [Message|Atoms]
  ),

  % Process the positional arguments.
  atomics_to_string(Atomics, String),

  % Run the Cowsay program.
  cowsay(String, Options3),

  % Automatically exit Prolog.
  halt.

