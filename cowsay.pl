:- module(
  cowsay,
  [
    cow_mode/1, % ?Mode:atom
    cowsay/1, % +Message:atom
    cowsay/2 % +Message:atom
             % :Options:list(nvpair)
  ]
).

/** <module> Cowsay

A funny cow for communicating with the user.

Based on `cowsay` by Tony Monroe,
 using the open source speech synthesizer `eSpeak`.

@author Wouter Beek
@see [Pointers to cowsay resources](http://en.wikipedia.org/wiki/Cowsay)
@see [Homepage of eSpeak](http://espeak.sourceforge.net/)
@tbd When tabs are used in cowsay/2 the width of the speech balloon
     cannot be reliable ascertained right now.
@version 2012-2015
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(option)).

:- use_module(plc(dcg/dcg_abnf)).
:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_code)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_meta)).
:- use_module(plc(dcg/dcg_word_wrap)).
:- use_module(plc(os/tts_ext)).

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
  mode(+atom),
  tongue(+callable)
]).
:- predicate_options(cowsay/2, 2, [
  max_width(+nonneg),
  speech(+boolean),
  wait(+boolean),
  pass_to(cow//3, 3),
  pass_to(dcg_word_wrap//1, 1)
]).





%! cowsay(+Message:atom) is det.
% @see Like cowsay/2, using all the default options.

cowsay(Message):-
  cowsay(Message, []).


%! cowsay(+Message:atom, :Options:list(nvpair)) is det.
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

cowsay(Message, Options1):-
  meta_options(is_meta, Options1, Options2),

  % Determine the maximum width of the speech bubble.
  select_option(max_width(MaximumWidth), Options2, Options3, 40),

  % Some characters are needed to display the vertical margins of
  % the speech bubble.
  MaximumEffectiveWidth is MaximumWidth - 4,

  % Here we assemble the options that are used for wrapping the input message
  % into display lines.
  merge_options(
    [padding(true),separator(line_feed),wrap_margin(MaximumEffectiveWidth)],
    Options3,
    Options4
  ),

  findall(
    CodeLine3,
    (
      % Split the message atom into lines.
      atomic_list_concat(Lines1, '\n', Message),

      % Now we are taling about individual lines proper.
      member(Line1, Lines1),

      % Some lines may exceed the maximum allowed width,
      %  so they are split into lines further.
      % The way in which this is done depends on
      %  the type of word wrapping that is used.
      atom_codes(Line1, CodeLine1),
      phrase(dcg_word_wrap(Options4), CodeLine1, CodeLine2),

      % We need a list for each line in order to determine
      % the speech bubble width.
      phrase(dcg_separated_list(line_feed, CodeLines1), CodeLine2),
      member(CodeLine3, CodeLines1)
    ),
    CodeLines2
  ),

  % Establish the width of the speech bubble.
  maplist(length, CodeLines2, LineLengths),
  max_list(LineLengths, LineWidth),

  % The cow DCG writes to the given output stream.
  select_option(output(Output), Options4, Options5, user_output),
  dcg_with_output_to(Output, phrase(cow(LineWidth, CodeLines2, Options5))),

  % It can talk!
  (   option(speech(true), Options5, true)
  ->  (   option(wait(true), Options5, true)
      ->  text_to_speech(Message)
      ;   thread_create(text_to_speech(Message), _, [detached(true)])
      )
  ;   true
  ).



%! cow(
%!   +LineWidth:between(5,inf),
%!   +CodeLines:list(list(code)),
%!   +Options:list(nvpair)
%! )// is det.

cow(LineWidth, CodeLines, Options) -->
  line_feed,
  speech_bubble(LineWidth, CodeLines),
  cow(Options),
  line_feed.



%! cow(+Options:list(nvpair))// is det.
% Emits the actual cow.

cow(Options) -->
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
  bracketed(round, cow_eyes(Options)),
  "\\___",
  '#'(CowLength, underscore, []),
  "\n",

  % Third line.
  indent(AddedIndent),
  "(__)\\   ",
  '#'(CowLength, space, []),
  ")",
  cow_tail,
  "\n",

  % Fourth line.
  indent(AddedIndent),
  " ", cow_tongue(Options),
  " ||",
  '#'(CowLength, hyphen, []),
  "w |\n",

  % Fifth line.
  indent(AddedIndent),
  "   ||",
  '#'(CowLength, space, []),
  " ||\n".



%! cow_eyes(+Options:list(nvpair))// is det.

cow_eyes(Options) -->
  {option(eyes(Dcg), Options)}, !,
  dcg_call(Dcg).
cow_eyes(Options) -->
  {option(mode(Mode), Options, default)},
  (   {Mode == 'Borg'}
  ->  "=="
  ;   {Mode == dead}
  ->  "XX"
  ;   {Mode == greedy}
  ->  "$$"
  ;   {Mode == paranoia}
  ->  "@@"
  ;   {Mode == stoned}
  ->  "**"
  ;   {Mode == tired}
  ->  "--"
  ;   {Mode == wired}
  ->  "OO"
  ;   {Mode == youth}
  ->  ".."
  ;   "oo"
  ).



%! cow_mode(+Mode:atom) is semidet.
%! cow_mode(-Mode:atom) is multi.

cow_mode('Borg').
cow_mode(dead).
cow_mode(default).
cow_mode(greedy).
cow_mode(paranoia).
cow_mode(stoned).
cow_mode(tired).
cow_mode(wired).
cow_mode(youth).



%! cow_tail// is det.

cow_tail -->
  "\\/\\".



%! cow_tongue(+Options:list(nvpair))// is det.

cow_tongue(Options) -->
  {option(tongue(Dcg), Options)}, !,
  dcg_call(Dcg).
cow_tongue(Options) -->
  {option(mode(Mode), Options, default)},
  (   {Mode == dead ; Mode == stoned}
  ->  "U"
  ;   " "
  ).



%! speech_bubble(
%!   +LineWidth:between(5,inf),
%!   +CodeLines:list(list(code))
%! )// is det.
% Draws a speech bubble with the given content,
% and whose content lines have the given length.

speech_bubble(LineWidth, CodeLines) -->
  speech_bubble_top(LineWidth), line_feed,
  speech_bubble_lines(LineWidth, CodeLines),
  speech_bubble_bottom(LineWidth), line_feed.



%! speech_bubble_bottom(+LineWidth:between(5,inf))// is det.

speech_bubble_bottom(LineWidth) -->
  "\\-",
  '#'(LineWidth, hyphen, []),
  "-/".



%! speech_bubble_line(
%!   +LineWidth:between(5,inf),
%!   +CodeLine:list(code)
%! )// is det.

speech_bubble_line(LineWidth, CodeLine) -->
  "| ",
  '*'(code, CodeLine, []),
  {
    length(CodeLine, ContentLength),
    NumberOfSpaces is LineWidth - ContentLength
  },
  '#'(NumberOfSpaces, " ", []),
  " |\n".



%! speech_bubble_lines(
%!   +LineWidth:between(5,inf),
%!   +CodeLines:list(list(code))
%! )// is det.

speech_bubble_lines(_LineWidth, []) --> !, [].
speech_bubble_lines(LineWidth, [CodeLine|CodeLines]) -->
  speech_bubble_line(LineWidth, CodeLine),
  speech_bubble_lines(LineWidth, CodeLines).



%! speech_bubble_top(+LineWidth:between(5,inf))// is det.

speech_bubble_top(LineWidth) -->
  "/-",
  '#'(LineWidth, hyphen, []),
  "-\\".

