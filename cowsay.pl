#! /usr/bin/env swipl

:- use_module(library(aggregate)).
:- use_module(library(optparse)).
:- use_module(library(string_ext)).

:- use_module(cowsay_lib).

:- set_prolog_flag(verbose, silent).

:- initialization(cowsay).

cowsay:-
  aggregate_all(set(Mode), cow_mode(Mode), ModeVals),
  WordWrapVals = [hard,none,soft],
  
  % Specify command-line arguments.
  OptSpec = [
    [ % Debug
      default(false),
      longflags([debug]),
      opt(debug),
      shortflags([d]),
      type(boolean)
    ],
    [ % Help
      default(false),
      longflags([help]),
      opt(help),
      shortflags([h]),
      type(boolean)
    ],
    [ % Maximum width
      default(50),
      longflags([max_width]),
      opt(max_width),
      type(integer)
    ],
    [ % Message
      default(""),
      longflags([message]),
      opt(message),
      shortflags([m]),
      type(string)
    ],
    [ % Mode
      default("default"),
      longflags([mode]),
      opt(mode),
      type(oneof(ModeVals))
    ],
    [ % Speech
      default(false),
      longflags([speech]),
      opt(speech),
      type(boolean)
    ],
    [ % Word wrap
      default(soft),
      longflags([word_wrap]),
      shortflags([w]),
      opt(word_wrap),
      type(oneof(WordWrapVals))
    ]
  ],

  % Process command-line arguments.
  gtrace,
  opt_arguments(OptSpec, Opts1, PosOpts),

  % Combine a message potentially specified via a flag
  % with the one(s) specified in positional arguments.
  select_option(message(Message), Opts1, Opts2),
  (   Message \== ""
  ->  All0 = [Message|PosOpts]
  ;   All0 = PosOpts
  ),

  % (A) Process the help flag.
  % (B) Nothing to do, maybe the user does not know what the options are.
  select_option(help(Help), Opts2, Opts3),
  (   (Help == true ; All == [])
  ->  opt_help(OptSpec, HelpString),
      All = ["Usage: cowsay [FLAGS] [TEXT]\n\n",HelpString]
  ;   All = All0
  ),

  % Process the positional arguments.
  string_list_concat(All, " ", Full),

  % Run the Cowsay program.
  cowsay(Full, Opts3),

  % Automatically exit Prolog.
  (option(debug(true), Opts3) -> true ; halt).
