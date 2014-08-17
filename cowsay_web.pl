:- module(
  cowsay_web,
  [
    cowsay_web//1, % +Content
    cowsay_web//2 % +Content
                  % +Options:list(nvpair)
  ]
).

/** <module> Cowsay Web

Version of Cowsay that can be used for generating HTML pages
with SWI-Prolog's `library(http/html_write))`.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(http/html_write)).

:- use_module(cowsay(cowsay)).



%! cowsay_web(+Content)// is det.

cowsay_web(Content) -->
  cowsay_web(Content, []).


%! cowsay_web(+Content, +Options:list(nvpair))// is det.

cowsay_web(Content, Options) -->
  with_output_to(atom(Code), cowsay(Content, Options)),
  html(code(Code)).

