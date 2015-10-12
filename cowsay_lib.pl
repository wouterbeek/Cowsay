:- module(
  cowsay_lib,
  [
    cow_mode/1, % ?Mode:string
    cowsay/1, % +Message:string
    cowsay/2 % +Message:string
             % :Options:list(compound)
  ]
).

/** <module> Cowsay

A funny cow for communicating with the user.

Based on `cowsay` by Tony Monroe,
 using the open source speech synthesizer `eSpeak`.

@author Wouter Beek
@see [Pointers to cowsay resources](http://en.wikipedia.org/wiki/Cowsay)
@see [Homepage of eSpeak](http://espeak.sourceforge.net/)
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(dcg/dcg_word_wrap)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process_ext)).

:- meta_predicate(cowsay(+,:)).

is_meta(eyes).
is_meta(tongue).

:- predicate_options(cow//1, 1, [
     pass_to(cow_eyes//1, 1),
     pass_to(cow_tongue//1, 1)
   ]).
:- predicate_options(cow//3, 3, [
     pass_to(cow//1, 1)
   ]).
:- predicate_options(cow_eyes//1, 1, [
     eyes(+callable),
     mode(+atom)
   ]).
:- predicate_options(cow_tongue//1, 1, [
     mode(+string),
     tongue(+callable)
   ]).
:- predicate_options(cowsay/2, 2, [
     max_width(+nonneg),
     speech(+boolean),
     wait(+boolean),
     pass_to(cow//3, 3),
     pass_to(dcg_word_wrap//1, 1)
   ]).





%! cowsay(+Message:string) is det.
% @see Like cowsay/2, using all the default options.

cowsay(Message):-
  cowsay(Message, []).


%! cowsay(+Message:string, :Options:list(compound)) is det.
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% The cow, as it reflects upon its own cowly life:
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
%   1. `eyes(+callable)`
%      A DCG that produces exactly 2 characters
%       and that replaces the default cow eyes.
%      Default: `"oo"`.
%      If the `eyes` option is absent but the `mode` option is present,
%      the eyes may be set by the indicated mode.
%
%   2. `max_width(+between(5,inf)`
%      The maximum width the speech bubble is allowed to have.
%      If the maximum width is exceeded by any content line,
%       content wrapping is used (see option `wrap_mode`
%       for the wrap mode that is used).
%      Default: `40`.
%      The minimum maximum width is 5, since the vertical margins of
%       the speech bubble take up 4 characters.
%
%   3. `mode(+atom)`
%      The original **Cowsay** came with a number of modes in which the cow
%      could appear. These modes can be set with this option.
%      The following values are supported:
%
%        1. `Borg`
%        2. `dead`
%        3. `default`
%        4. `greedy`,
%        5. `paranoia`
%        6. `stoned`
%        7. `tired`
%        8. `wired`
%        9. `youth`
%
%   4. `output(+Output)`
%      The same output alternatives that apply to with_output_to/2.
%      Default: `user_output`.
%
%   5. `speech(+boolean)`
%      Whether **Cowsay** should use a speech synthesizer for reading
%       the message out loud.
%      Default: `true`.
%
%   6. `tongue(+callable)`
%      A DCG that emits exactly one character,
%      replacing the tongue of the cow.
%      Default: `" "`.
%      If the `tongue` option is absent but the `mode` option is present,
%      the tongue may be set by the indicated mode.
%
%   7. `wait(+boolean)`
%      Whether consecutive executions of **Cowsay** should wait
%       for each other's speech synthesizer to complete.
%      Default: `true`.
%
%   8. Other options are passed to dcg_word_wrap//1.

cowsay(Message, Opts1):-
  meta_options(is_meta, Opts1, Opts2),

  % Determine the maximum width of the speech bubble.
  select_option(max_width(MaxWidth), Opts2, Opts3, 40),

  % Some characters are needed to display the vertical margins of
  % the speech bubble.
  MaxEffectiveWidth is MaxWidth - 4,

  % Here we assemble the options that are used for wrapping the input message
  % into display lines.
  merge_options(
    [padding(true),separator(line_feed),wrap_margin(MaxEffectiveWidth)],
    Opts3,
    Opts4
  ),

  findall(
    Line,
    (
      % Split the message atom into lines.
      split_string(Message, "\n", "", Lines1),

      % Now we are taling about individual lines proper.
      member(Line1, Lines1),

      % Some lines may exceed the maximum allowed width,
      %  so they are split into lines further.
      % The way in which this is done depends on
      %  the type of word wrapping that is used.
      string_phrase(dcg_word_wrap(Opts4), Line1, Line2),

      % We need a list for each line in order to determine
      % the speech bubble width.
      split_string(Line2, "\n", "", Lines2),
      member(Line, Lines2)
    ),
    Lines
  ),

  % Establish the width of the speech bubble.
  maplist(string_length, Lines, LineLengths),
  max_list(LineLengths, LineWidth),

  % The cow DCG writes to the given output stream.
  select_option(output(Output), Opts4, Opts5, user_output),
  once(dcg_with_output_to(Output, cow(LineWidth, Lines, Opts5))),

  % It can talk!
  (   option(speech(true), Opts5, true)
  ->  (   option(wait(true), Opts5, true)
      ->  text_to_speech(Message)
      ;   thread_create(text_to_speech(Message), _, [detached(true)])
      )
  ;   true
  ).

text_to_speech(Message):-
  run_process(espeak, ['--',Message], [detached(false),program(espeak)]).



%! cow(
%!   +LineWidth:between(5,inf),
%!   +Lines:list(string)),
%!   +Options:list(compound)
%! )// is det.

cow(LineWidth, Lines, Opts) -->
  "\n",
  speech_bubble(LineWidth, Lines),
  cow(Opts),
  "\n".



%! cow(+Options:list(compound))// is det.
% Emits the actual cow.

cow(Opts) -->
  {
    Indent = 4,
    AddToIndent = 2,
    AddedIndent is Indent + AddToIndent,
    CowLength = 4
  },

  % First line.
  indent(Indent),
  "\\   ^__^\n",

  % Second line.
  indent(Indent),
  " \\  ",
  bracketed(round, cow_eyes(Opts)),
  "\\___",
  #(CowLength, underscore, []),
  "\n",

  % Third line.
  indent(AddedIndent),
  "(__)\\   ",
  #(CowLength, space, []),
  ")",
  cow_tail,
  "\n",

  % Fourth line.
  indent(AddedIndent),
  " ", cow_tongue(Opts),
  " ||",
  #(CowLength, hyphen, []),
  "w |\n",

  % Fifth line.
  indent(AddedIndent),
  "   ||",
  #(CowLength, space, []),
  " ||\n".



%! cow_eyes(+Options:list(compound))// is det.

cow_eyes(Opts) -->
  {option(eyes(Dcg_0), Opts)}, !,
  Dcg_0.
cow_eyes(Opts) -->
  {option(mode(Mode), Opts, default)},
  (   {Mode == "Borg"}
  ->  "=="
  ;   {Mode == "dead"}
  ->  "XX"
  ;   {Mode == "greedy"}
  ->  "$$"
  ;   {Mode == "paranoia"}
  ->  "@@"
  ;   {Mode == "stoned"}
  ->  "**"
  ;   {Mode == "tired"}
  ->  "--"
  ;   {Mode == "wired"}
  ->  "OO"
  ;   {Mode == "youth"}
  ->  ".."
  ;   "oo"
  ).



%! cow_mode(+Mode:string) is semidet.
%! cow_mode(-Mode:string) is multi.

cow_mode("Borg").
cow_mode("dead").
cow_mode("default").
cow_mode("greedy").
cow_mode("paranoia").
cow_mode("stoned").
cow_mode("tired").
cow_mode("wired").
cow_mode("youth").



%! cow_tail// is det.

cow_tail -->
  "\\/\\".



%! cow_tongue(+Options:list(compound))// is det.

cow_tongue(Opts) -->
  {option(tongue(Dcg_0), Opts)}, !,
  Dcg_0.
cow_tongue(Opts) -->
  {option(mode(Mode), Opts, "default")},
  (   {Mode == "dead" ; Mode == "stoned"}
  ->  "U"
  ;   " "
  ).



%! speech_bubble(
%!   +LineWidth:between(5,inf),
%!   +Lines:list(list(string))
%! )// is det.
% Draws a speech bubble with the given content,
% and whose content lines have the given length.

speech_bubble(LineWidth, Lines) -->
  speech_bubble_top(LineWidth),
  "\n",
  speech_bubble_lines(LineWidth, Lines),
  speech_bubble_bottom(LineWidth),
  "\n".



%! speech_bubble_bottom(+LineWidth:between(5,inf))// is det.

speech_bubble_bottom(LineWidth) -->
  "\\-",
  '#'(LineWidth, hyphen, []),
  "-/".



%! speech_bubble_line(+LineWidth:between(5,inf), +Line:list(string))// is det.

speech_bubble_line(LineWidth, Line) -->
  "| ",
  Line,
  {
    string_length(Line, ContentLength),
    NumberOfSpaces is LineWidth - ContentLength
  },
  '#'(NumberOfSpaces, " ", []),
  " |\n".



%! speech_bubble_lines(
%!   +LineWidth:between(5,inf),
%!   +Lines:list(list(string))
%! )// is det.

speech_bubble_lines(_, []) --> !, [].
speech_bubble_lines(LineWidth, [H|T]) -->
  speech_bubble_line(LineWidth, H),
  speech_bubble_lines(LineWidth, T).



%! speech_bubble_top(+LineWidth:between(5,inf))// is det.

speech_bubble_top(LineWidth) -->
  "/-",
  '#'(LineWidth, hyphen, []),
  "-\\".
