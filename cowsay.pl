:- module(
  cowsay,
  [
    cowsay/1, % +Content
    cowsay/2 % +Content
             % +Options:list(nvpair)
  ]
).

/** <module> Cowspeak

A funny cow for communicating with the user.

Based on `cowsay` by Tony Monroe,
 using the open source speech synthesizer `eSpeak`.

@author Wouter Beek
@see http://en.wikipedia.org/wiki/Cowsay pointers to cowsay resources.
@see http://espeak.sourceforge.net/ home of eSpeak.
@version 2012/09-2012/10, 2013/05-2013/09, 2014/01, 2014/08
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(settings)).

:- use_module(generics(codes_ext)).
:- use_module(generics(option_ext)).
:- use_module(os(tts_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_generic)).
:- use_module(plDcg(dcg_wrap)).

% The automated finding of meta-predicates seems to be over-eager.
:- meta_predicate(dcg_speech_bubble_line(+,+,?,?)).

:- setting(
  default_max_width,
  integer,
  40,
  'The default width of the speech bubble in the number characters.'
).



%! cow_atom(+In:or([atom,pair(atom,list),term]), -Out:atom) is det.

cow_atom(Format-Arguments, Atom):- !,
  format(atom(Atom), Format, Arguments).
cow_atom(Atom, Atom):-
  atom(Atom), !.
cow_atom(Term, Atom):-
  term_to_atom(Term, Atom).


%! cowsay(+Content:or([term,list(terms)])) is det.
%! cowsay(+Content:or([term,list(terms)]), +Options) is det.
% Turns the given text into a cowified message, displaying the given
% text in the cow's speech bubble.
%
% The cow, as it reflects upon its own cow life:
% ~~~{.txt}
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
% `Content` can be any term or list of terms.
% Terms of the form =|Format-ListOfArguments|= are treated specially
%  and are passed on to format/3.
%
% The following options are supported:
%   * =|eyes(+Eyes:or([atom,list(code)])|=
%     Either a list of character codes or an atom, the first 2 characters
%      of which will replace the eyes of the cow.
%   * =|maximum_width(?MaximumWidth:integer)|=
%     The maximum number of characters the speech bubble is allowed to have.
%     If the maximum width is exceeded by any content line, then the
%      wrap option -- if set -- is used.
%   * =|mode(+Mode:oneof(['Borg',dead,greedy,paranoia,stoned,tired,wired,youth]))|=
%     The following process_modes are supported: `Borg`, `dead`, `greedy`,
%      `paranoia`, `stoned`, `tired`, `wired`, `youth`.
%   * =|output(+Output)|=
%     The same output alternatives that apply to with_output_to/2.
%     The default value is =|stream(user_output)|=.
%   * =|speech(+OnOrOff:boolean)|=
%   * =|wrap_mode(+WrapMode:oneof([line,none,word]))|=
%     Whether `line` wrapping or =word= wrapping (default)
%     is applied, or neither of those (=none=, e.g. for ASCII art).
%
% @arg Options A list of name-value pairs.
% @arg Contents Either a term or a list of terms.
%        Processes terms of the form =|Format-ListOfArguments|= specially.
%
% @tbd Split lines by words (in whitespace). Add this to module ATOM_EXT.
% @tbd When tabs are used in cowsay/2 the width of the speech balloon
%      cannot be reliable ascertained right now.

cowsay(Content):-
  cowsay(Content, []).

cowsay(Contents, Options):-
  is_list(Contents), !,
  maplist(cow_atom, Contents, Atoms),
  cowsa_atoms(Atoms, Options).
% Since we work with lists, we create a singleton list for single terms.
cowsay(Content, Options):-
  cowsay_atoms([Content], Options).

% All content is atomic by now.
cowsay_atoms(Atoms, Options1):-
  % The default wrap mode is wrapping words.
  add_default_option(Options1, wrap_mode, word, Options2),

  % Determine the maximum width of the speech bubble.
  setting(default_max_width, DefaultMaxWidth),
  option(maximum_width(MaximumWidth), Options2, DefaultMaxWidth),

  % Some characters are needed to display the speech bubble itself.
  MaximumEffectiveWidth is MaximumWidth - 4,
  merge_options(
    [padding(true),separator(line_feed),wrap_margin(MaximumEffectiveWidth)],
    Options2,
    Options3
  ),
  findall(
    CodeLine3,
    (
      member(Atom, Atoms),
      
      % A single atom may contain multiple lines.
      atomic_list_concat(Lines1, '\n', Atom), % split
      
      % Now we are taling about individual lines proper.
      member(Line1, Lines1),
      
      % Some lines may exceed the maximum allowed width,
      %  so they are  split up further.
      % The way in which this is done depends on
      %  the type of wrapping that is used.
      atom_codes(Line1, CodeLine1),
      phrase(dcg_wrap(O3), CodeLine1, CodeLine2),
      
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

  % It can talk!
  select_option(speech(Speech), Options3, Options4, true),
  (
    Speech == true
  ->
    text_to_speech(Atoms)
  ;
    true
  ),
 
  % The cow DCG writes to the given output stream.
  select_option(output(Output), Options4, Options5, user_output),
  dcg_with_output_to(Output, phrase(cow(LineWidth, CodeLines2, Options5))).



%! cow(
%!   +LineWidth:nonneg,
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

cow(Options1) -->
  {
    process_modes(Options1, Options2),
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
  dcg_bracketed(round, dcg_cow_eyes(Options2)),
  "\\___",
  '#'(CowLength, underscore),
  "\n",

  % Third line.
  indent(AddedIndent),
  "(__)\\   ",
  '#'(CowLength, space), ")",
  cow_tail,
  "\n",

  % Fourth line.
  indent(AddedIndent),
  " ", cow_tongue(Options2),
  " ||",
  '#'(CowLength, hyphen),
  "w |\n",

  % Fifth line.
  indent(AddedIndent),
  "    ||",
  '#'(CowLength, space),
  " ||\n".


cow_eyes(Options) -->
  {
    option(eyes(Eyes1), Options, `oo`),
    % Ensure that the eyes are codes
    % (i.e., apply atom2code conversion if needed).
    once(atomic_codes(Eyes1, Eyes2)),
    Eyes2 = [X,Y|_], !
  },
  [X,Y].


cow_tail -->
  "\\/\\".


cow_tongue(Options) -->
  {
    option(tongue(Tongue1), Options, `  `),
    % Ensure that the eyes are codes
    % (i.e., apply atom2code conversion if needed).
    once(atomic_codes(Tongue1, Tongue2)),
    Tongue2 = [X,Y|_], !
  },
  [X,Y].


%! speech_bubble(+LineWidth:integer, +CodeLines:list(list(code)))//
% Draws a speech bubble with the given content,
% and whose content lines have the given length.

speech_bubble(LineWidth, CodeLines) -->
  speech_bubble_top(LineWidth), line_feed,
  speech_bubble_lines(LineWidth, CodeLines),
  speech_bubble_bottom(LineWidth), line_feed.


speech_bubble_bottom(LineWidth) -->
  "\\-",
  '#'(LineWidth, hyphen),
  "-/".


dcg_speech_bubble_line(LineWidth, CodeLine) -->
  "| ",
  CodeLine,
  {
    length(CodeLine, ContentLength),
    NumberOfSpaces is LineWidth - ContentLength
  },
  '#'(NumberOfSpaces, " "),
  " |\n".


speech_bubble_lines(_LineWidth, []) --> !, [].
speech_bubble_lines(LineWidth, [CodeLine|CodeLines]) -->
  speech_bubble_line(LineWidth, CodeLine),
  speech_bubble_lines(LineWidth, CodeLines).


speech_bubble_top(LineWidth) -->
  "/-",
  '#'(LineWidth, hyphen),
  "-\\".



mode('Borg', [eyes(`==`)]).
mode(dead, [eyes(`XX`),tongue(`U`)]).
mode(greedy, [eyes(`$$`)]).
mode(paranoia, [eyes(`@@`)]).
mode(stoned, [eyes(`**`),tongue(`U`)]).
mode(tired, [eyes(`--`)]).
mode(wired, [eyes(`OO`)]).
mode(youth, [eyes(`..`)]).


process_modes(Options1, Options2):-
  option(mode(Mode), Options1),
  mode(Mode, ModeOptions), !,
  merge_options(ModeOptions, Options1, Options2).
process_modes(Options, Options).
