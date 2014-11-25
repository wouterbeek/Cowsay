# Cowsay

## Simple

### Simple installation

Cowsay can be installed as a library from within
 [SWI-Prolog](http://www.swi-prolog.org/):
~~~shell
$ swipl
?- pack_install(cowsay).
~~~

---

### Simple use

Example usage:
~~~shell
$ cd Cowsay
$ ./run "I'm a funny cow that communicates messages to the user. I am based on the old cowsay by Tony Monroe, in combination with the open source speech synthesizer eSpeak."
~~~

This results in the following output:
~~~
/----------------------------------------------\
| I'm a funny cow that communicates messages   |
| to the user.                                 |
| I am based on the old cowsay by Tony Monroe, |
| in combination with the open source speech   |
| synthesizer eSpeak.                          |
\----------------------------------------------/
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
~~~

---

### Simple options

When used as a standalone application, **Cowsay** supports the following
options that can be given as command-line arguments:

| **Long flag** | **Short flag** | **Value type** | **Default value** |
|:-------------:|:--------------:|:---------------|:-----------------:|
| `--debug`     | `-d`           | boolean        | `false`           |
| `--help`      | `-h`           | boolean        | `false`           |
| `--max_width` | none           | integer        | `50`              |
| `--message`   | `-m`           | atom           | `''`              |
| `--mode`      | none           | One of: `Borg`, `dead`, `default`, `greedy`, `paranoia`, `stoned`, `tired`, `wired`, `youth` | `default` |
| `--speech`    | none           | boolean        | `true`            |
| `--word_wrap` | `-w`           | One of: `hard`, `none`, `soft` | `soft`    |

---

## Advanced use

Here is an example in which **Cowsay** is used as a library:
~~~shell
?- use_module(library(cowsay)).
?- cowsay("Example sentence spoken by the cow.", [mode(paranoia)]).
/------------------------------------------------\
| Example sentence spoken by the cow.            |
\------------------------------------------------/
        \   ^__^
         \  (@@)\_______
            (__)\       )\/\
               ||----w |
               ||     ||
~~~

### Advanced options

~~~prolog
cowsay(+Message:atom, +Options:list(nvpair)) is det.
~~~

*`Message`* is a Prolog atom.
*`Options`* is a list of name-value pairs that represent
 the options described below.
A name-value pair is a unary predicate,
 i.e. a Prolog term of the form `NAME(VALUE)`.
The following options are supported:

  1. `eyes(+callable)`
     A DCG rule that produces exactly 2 characters
      and that replaces the default cow eyes.
     Default: `"oo"`.
     If the `eyes` option is absent but the `mode` option is present,
     the eyes may be set by the indicated mode.

  2. `max_width(+between(5,inf)`
     The maximum width the speech bubble is allowed to have.
     If the maximum width is exceeded by any content line,
      content wrapping is used (see option `wrap_mode`
      for the wrap mode that is used).
     Default: `40`.
     The minimum maximum width is 5, since the vertical margins of
      the speech bubble take up 4 characters.

  3. `mode(+atom)`
     The original **Cowsay** came with a number of modes in which the cow
     could appear. These modes can be set with this option.
     The following values are supported:
       1. `Borg`
       2. `dead`
       3. `default`
       4. `greedy`,
       5. `paranoia`
       6. `stoned`
       7. `tired`
       8. `wired`
       9. `youth`

  4. `output(+Output)`
     The same output alternatives that apply to
      [`with_output_to/2`](http://www.swi-prolog.org/pldoc/doc_for?object=with_output_to/2).
     Default: `user_output`.

  5. `speech(+boolean)`
     Whether **Cowsay** should use a speech synthesizer for reading
      the message out loud.
     Default: `true`.

  6. `tongue(+callable)`
     A DCG rule that emits exactly one character, replacing the tongue of the cow.
     Default: `" "`.
     If the `tongue` option is absent but the `mode` option is present,
      the tongue may be set by the indicated mode.

  7. `wait(+boolean)`
     Whether consecutive executions of **Cowsay** should wait
      for each other's speech synthesizer to complete.
     Default: `true`.

  8. Other options are passed to `dcg_word_wrap//1`.

### Alternative installation: not using SWI-Prolog pack install

  1.  Compile the latest version of **SWI-Prolog**:
      ~~~shell
      $ git clone https://github.com/SWI-Prolog/swipl-devel.git
      $ cd swipl-devel
      $ ./build
      ~~~

  2.  Download the latest version of **Cowsay**:
      ~~~shell
      $ git clone https://github.com/wouterbeek/Cowsay.git
      ~~~

---

## Further reading

This project is written in [**SWI-Prolog 7**](http://www.swi-prolog.org/)
 and uses the following techniques and features:

  1. **Descriptive Clause Grammars (DCGs)** are used to draw
      the cow and the speech bubble, and are used to perform word-wrapping
      on the input.
     Anne Ogborn wrote
      [an introduction to Definite Clause Grammars (DCGs) in (SWI-)Prolog](http://www.pathwayslms.com/swipltuts/dcg/).
  2. This project imports several often reused DCG rules from
      [library **plDcg**](https://github.com/wouterbeek/plDcg.git),
      e.g., the DCG rule that wraps the given input string w.r.t.
      the maximum speech bubble width.
  3. Documentation throughout this project uses the
      [**plDoc 2**](http://www.swi-prolog.org/pldoc/package/pldoc.html)
      format by Jan Wielemaker.
  3. Options for predicates are explicitly denoted by using the
      [**predicate options library**](http://www.swi-prolog.org/pldoc/man?section=predicate_options).
  4. Options are processed by predicates from the
      [**options library**](http://www.swi-prolog.org/pldoc/man?section=option).
  5. This project imports several often occurring predicates from the
      [**Prolog-Library-Collection**](https://github.com/wouterbeek/Prolog-Library-Collection.git).

---

## The cow, as it reflects upon its own cow life

**Cowsay** also supports the display of ASCII art,
 by setting `wrap_mode` to `none`. Example:

~~~
/--------------------------------------\
|     ^__^                             |
|     (oo)\_______                     |
|     (__)\       )\/\                 |
|         ||----w |                    |
|         ||     ||                    |
\--------------------------------------/
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
~~~

---

Developed in 2012-2014 by [Wouter Beek](http://www.wouterbeek.com).
