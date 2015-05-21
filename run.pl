% Standalone startup for the cowsay application.


:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- use_module(library(aggregate)).
:- use_module(library(optparse)).

:- use_module(plc(prolog/pl_log)).

:- set_prolog_flag(verbose, silent).

:- initialization(cowsay).

cowsay:-
  % Specify command-line arguments.
  OptionSpec = [
    % debug
    [default(false),longflags([debug]),opt(debug),shortflags([d]),type(boolean)],
    % help
    [default(false),longflags([help]),opt(help),shortflags([h]),type(boolean)],
    % max_width
    [default(50),longflags([max_width]),opt(max_width),type(integer)],
    % message
    [default(''),longflags([message]),opt(message),shortflags([m]),type(atom)],
    % mode
    [default(default),longflags([mode]),opt(mode),type(atom)],
    % speech
    [default(false),longflags([speech]),opt(speech),type(boolean)],
    % word_wrap
    [default(soft),longflags([word_wrap]),shortflags([w]),opt(word_wrap),type(atom)]
  ],

  % Process command-line arguments.
  run_collect_messages(
    opt_arguments(OptionSpec, Options1, PositionalAtoms),
    Status,
    _
  ),

  % Combine a message potentially specified via a flag
  % with the one(s) specified in positional arguments.
  select_option(message(Message), Options1, Options2),
  (   Message \== ''
  ->  AllAtoms0 = [Message|PositionalAtoms]
  ;   AllAtoms0 = PositionalAtoms
  ),

  (   Status == true
  ->  true
  ;   halt
  ),

  % (A) Process the help flag.
  % (B) Nothing to do, maybe the user does not know what the options are.
  select_option(help(Help), Options2, Options3),
  (   (   Help == true
      ;   AllAtoms0 == []
      )
  ->  opt_help(OptionSpec, HelpAtom),
      AllAtoms = ['Usage: cowsay [FLAGS] [TEXT]\n\n',HelpAtom]
  ;   AllAtoms = AllAtoms0
  ),

  % Process the positional arguments.
  atomic_list_concat(AllAtoms, ' ', FullMessage),

  % Typecheck the `mode` command-line argument.
  (   option(mode(Mode), Options3)
  ->  aggregate_all(
        set(Mode),
        cow_mode(Mode),
        Modes
      ),
      must_be(oneof(Modes), Mode)
  ;   true
  ),

  % Typecheck the `word_wrap` command-line argument.
  (   option(word_wrap(WordWrap), Options3)
  ->  must_be(oneof([hard,none,soft]), WordWrap)
  ;   true
  ),

  % Run the Cowsay program.
  cowsay(FullMessage, Options3),

  % Automatically exit Prolog.
  (   option(debug(true), Options3)
  ->  true
  ;   halt
  ).

