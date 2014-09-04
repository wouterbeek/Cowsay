:- module(cowsay_test, []).

/** <module> Cowsay test

Automated tests for the Cowsay application.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(plunit)).

:- use_module(cowsay(cowsay)).



:- begin_tests(cowsay).

cow_sentence("test").
cow_sentence("Interpret the next argument as a character code and add it to \c
              the output. This argument must be a valid Unicode character \c
              code. Note that the actually emitted bytes are defined by the \c
              character encoding of the output stream and an exception may \c
              be raised if the output stream is not capable of representing \c
              the requested Unicode character. See section 2.18.1 \c
              for details.\c
             ").
cow_sentence("%! cowsay(+Options, +Content:or([term,list(terms)])) is det.").
cow_sentence(' \c
^__^\n \c
 (oo)\\_______\n \c
 (__)\\       )\\/\\\n     \c
     ||----w |\n     \c
     ||     ||\c
').

cow_sentence(String, Mode):-
  cow_sentence(String),
  cow_mode(Mode).

test(cowsay, [forall(cow_sentence(String,Mode)),true]):-
  cowsay(String, [mode(Mode)]).

:- end_tests(cowsay).

