:- module(cowsay_test, []).

/** <module> Cowsay test

Automated tests for the Cowsay application.

@author Wouter Beek
@version 2014/08, 2014/11
*/

:- use_module(library(lists)).
:- use_module(library(plunit)).

:- use_module(cowsay(cowsay)).



:- begin_tests(cowsay).

cow_sentence('test').
cow_sentence('Interpret the next argument as a character code and add it to \c
              the output. This argument must be a valid Unicode character \c
              code. Note that the actually emitted bytes are defined by the \c
              character encoding of the output stream and an exception may \c
              be raised if the output stream is not capable of representing \c
              the requested Unicode character. See section 2.18.1 \c
              for details.\c
             ').
cow_sentence('%! cowsay(+Options, +Content:or([term,list(terms)])) is det.').
cow_sentence(' \c
^__^\n \c
 (oo)\\_______\n \c
 (__)\\       )\\/\\\n     \c
     ||----w |\n     \c
     ||     ||\c
').

cow_sentence(Atom, MaxWidth, Mode, WordWrap):-
  cow_sentence(Atom),
  %between(10, 80, MaxWidth),
  MaxWidth = 50,
  cow_mode(Mode),
  member(WordWrap, [hard,soft]).

test(cowsay, [forall(cow_sentence(Atom,MaxWidth,Mode,WordWrap)),true]):-
  cowsay(
    Atom,
    [max_width(MaxWidth),mode(Mode),speech(false),word_wrap(WordWrap)]
  ).

:- end_tests(cowsay).

