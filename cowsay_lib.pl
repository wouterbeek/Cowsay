:- module(
  cowsay_lib,
  [
    cow_mode/1, % ?Mode
    cowsay/1,   % +Message
    cowsay/2    % +Message, +Options
  ]
).

/** <module> Cowsay library

Predicates that support the Cowsay application.

*/

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- use_module(library(abnf)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(string_ext)).
:- use_module(library(thread_ext)).



%! cowsay(+Message:or([string,list(string)])) is det.
%! cowsay(+Message:or([string,list(string)]), +Options:options) is det.
%
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% The cow, as it reflects upon its own cowly life:
%
% ~~~text
% /--------------------------------------\
% |     ^__^                             |
% |     (oo)\_______                     |
% |     (__)\       )\/\                 |
% |         ||----w |                    |
% |         ||     ||                    |
% \--------------------------------------/
%         \   ^__^
%          \  (oo)\_______
%             (__)\       )\/\
%                 ||----w |
%                 ||     ||
% ~~~
%
% The following options are supported:
%
%   * eyes(+string)
%
%     The two characters that are the eyes of the cow.  Default:
%     `"oo"`.  If the `eyes` option is absent but the `mode` option is
%     present, the eyes may be set based on the mode.
%
%   * max_width(+between(5,inf)
%
%     The maximum width the speech bubble is allowed to have.  If the
%     maximum width is exceeded by any content line, content wrapping
%     is used (see option `wrap_mode` for the wrap mode that is used).
%     Default: `40`.  The minimum maximum width is 5, since the
%     vertical margins of the speech bubble take up 4 characters.
%
%   * mode(+atom)
%
%     The original **Cowsay** came with a number of modes in which the
%     cow could appear. These modes can be set with this option.  The
%     following values are supported:
%
%       1. `'Borg'`
%       2. `dead`
%       3. `default`
%       4. `greedy`,
%       5. `paranoia`
%       6. `stoned`
%       7. `tired`
%       8. `wired`
%       9. `youth`
%
%   * output(+Output)
%
%     The same output alternatives that apply to with_output_to/2.
%     Default: `user_output`.
%
%   * speech(+boolean)
%
%     Whether **Cowsay** should use a speech synthesizer for reading
%     the message out loud.  Default: `true`.
%
%   * tongue(+char)
%
%     The tongue of the cow.  Default: `' '`.  If the `tongue` option
%     is absent but the `mode` option is present, the tongue may be
%     set based on the mode.
%
%   * wait(+boolean)
%
%     Whether consecutive executions of **Cowsay** should wait for
%     each other's speech synthesizer to complete.  Default: `true`.

cowsay(Message) :-
  cowsay(Message, options{}).


cowsay(Message, Options) :-
  options{max_width: MaxWidth} :< Options,
  % Some characters are needed in order to display the vertical
  % margins of the speech bubble.
  MaxEffectiveWidth #= MaxWidth - 4,
  message_lines(Message, MaxEffectiveWidth, Lines),
  % Establish the width of the speech bubble.  This may be a little
  % bit less than MaxEffectiveWidth, depending on the content in
  % Lines.
  max_string_length(Lines, LineWidth),
  % The cow grammar writes to the given output stream.
  dict_get(output, Options, user_output, Out),
  once(dcg_with_output_to(Out, cow(LineWidth, Lines, Options))),
  % It can talk!
  (   options{speech: true} :< Options
  ->  (   options{wait: false} :< Options
      ->  create_detached_thread(text_to_speech(Message))
      ;   text_to_speech(Message)
      )
  ;   true
  ).

text_to_speech(Message) :-
  run_process(espeak, ['--',Message], [detached(false),program(espeak)]).



%! cow(+LineWidth:between(5,inf),
%!     +Lines:list(string)),
%!     +Options:options)// is det.

cow(LineWidth, Lines, Options) -->
  "\n",
  speech_bubble(LineWidth, Lines),
  cow(Options),
  "\n".

cow(Options) -->
  {options{length: CowLength} :< Options},
  {Indent1 = 4},
  {Indent2 #= Indent1 + 2},

  % First line.
  indent(Indent1),
  "\\   ^__^\n",

  % Second line.
  indent(Indent1),
  " \\  (",
  cow_eyes(Options),
  ")\\___",
  {gtrace},
  #(CowLength, "_"),
  "\n",

  % Third line.
  indent(Indent2),
  "(__)\\   ",
  #(CowLength, " "),
  ")",
  cow_tail,
  "\n",

  % Fourth line.
  indent(Indent2),
  " ", cow_tongue(Options),
  " ||",
  #(CowLength, "-"),
  "w |\n",

  % Fifth line.
  indent(Indent2),
  "   ||",
  #(CowLength, " "),
  " ||\n".



%! cow_eyes(+Options:options)// is det.

cow_eyes(Options) -->
  {options{eyes: Eyes} :< Options}, !,
  string(Eyes).
cow_eyes(Options) -->
  {options{mode: Mode} :< Options},
  cow_eyes_mode_(Mode).

cow_eyes_mode_('Borg') --> "==".
cow_eyes_mode_(dead) --> "XX".
cow_eyes_mode_(default) --> "oo".
cow_eyes_mode_(greedy) --> "$$".
cow_eyes_mode_(paranoia) --> "@@".
cow_eyes_mode_(stoned) --> "**".
cow_eyes_mode_(tired) --> "--".
cow_eyes_mode_(wired) --> "OO".
cow_eyes_mode_(youth) --> "..".



%! cow_mode(+Mode:atom) is semidet.
%! cow_mode(-Mode:atom) is multi.

cow_mode(Mode) :-
  cow_eyes_mode_(Mode, _, _).



%! cow_tail// is det.

cow_tail --> "\\/\\".



%! cow_tongue(+Options:options)// is det.

cow_tongue(Options) -->
  {options{tongue: Tongue} :< Options}, !,
  char(Tongue).
cow_tongue(Options) -->
  {option(mode(Mode), Options, "default")},
  cow_tongue_mode_(Mode).

cow_tongue_mode_(dead) --> "U".
cow_tongue_mode_(stoned) --> "U".
cow_tongue_mode_(_) --> " ".



%! speech_bubble(+LineWidth:between(5,inf),
%!               +Lines:list(list(string)))// is det.
%
% Draws a speech bubble with the given content, and whose content
% lines have the given length.

speech_bubble(LineWidth, Lines) -->
  speech_bubble_top(LineWidth),
  "\n",
  speech_bubble_lines(LineWidth, Lines),
  speech_bubble_bottom(LineWidth),
  "\n".



%! speech_bubble_bottom(+LineWidth:between(5,inf))// is det.

speech_bubble_bottom(LineWidth) -->
  "\\-",
  #(LineWidth, "-"),
  "-/".



%! speech_bubble_line(+LineWidth:between(5,inf), +Line:list(string))// is det.

speech_bubble_line(LineWidth, Line) -->
  "| ",
  Line,
  {
    string_length(Line, ContentLength),
    NumberOfSpaces #= LineWidth - ContentLength
  },
  #(NumberOfSpaces, " "),
  " |\n".



%! speech_bubble_lines(+LineWidth:between(5,inf),
%!                     +Lines:list(list(string)))// is det.

speech_bubble_lines(_, []) --> !, "".
speech_bubble_lines(LineWidth, [H|T]) -->
  speech_bubble_line(LineWidth, H),
  speech_bubble_lines(LineWidth, T).



%! speech_bubble_top(+LineWidth:between(5,inf))// is det.

speech_bubble_top(LineWidth) -->
  "/-",
  #(LineWidth, "-"),
  "-\\".
