% Debug file for the cowsay application.

:- [load].


% Run unit tests.

:- use_module(library(plunit)).

:- use_module(cowsay(cowsay_test)).

:- run_tests.

