# Cowsay

## Simple

### Simple installation

  1.  Compile the latest version of **SWI-Prolog**:
      ~~~
      git clone https://github.com/SWI-Prolog/swipl-devel.git
      cd swipl-devel
      ./build
      ~~~

  2.  Download the latest version of **Cowsay**:
      ~~~
      git clone https://github.com/wouterbeek/Cowsay.git
      ~~~

---

### Simple use

Example usage:
~~~
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

<table>
  <tr>
    <th>Long flag</th>
    <th>Short flag</th>
    <th>Value type</th>
    <th>Default value</th>
  </tr>
  <tr>
    <td><code>--debug</code></td>
    <td><code>-d</code></td>
    <td>boolean</td>
    <td><code>false</code></td>
  </tr>
  <tr>
    <td><code>--help</code></td>
    <td><code>-h</code></td>
    <td>booleah</td>
    <td><code>false</code></td>
  </tr>
  <tr>
    <td><code>--max_width</code></td>
    <td></td>
    <td>integer</td>
    <td><code>50</code></td>
  </tr>
  <tr>
    <td><code>--message</code></td>
    <td><code>-m</code></td>
    <td>string</td>
    <td></td>
  </tr>
  <tr>
    <td><code>--mode</code></td>
    <td></td>
    <td>
      <code>Borg</code>,
      <code>dead</code>,
      <code>default</code>,
      <code>greedy</code>,
      <code>paranoia</code>,
      <code>stoned</code>,
      <code>tired</code>,
      <code>wired</code>,
      <code>youth</code>
    </td>
    <td><code>default</code></td>
  </tr>
  <tr>
    <td><code>speech</code></td>
    <td></td>
    <td>boolean</td>
    <td><code>true</code></td>
  </tr>
  <tr>
    <td><code>--wrap_mode</code></td>
    <td></td>
    <td><code>line</code>, <code>none</code>, <code>word</code></td>
    <td><code>word</code></td>
  </tr>
</table>

---

## Advanced

### Advanced installation

Cowsay can be installed as a library from within SWI-Prolog:
~~~
$ swipl
?- pack_install(cowsay).
~~~

### Advanced use

Here is an example in which **Cowsay** is used as a library:
~~~
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

~~~{.pl}
cowsay(+Message:string, +Options:list(nvpair)) is det.
~~~

***`Message`*** is a
 [SWI7 string](http://www.swi-prolog.org/pldoc/man?section=strings).
In SWI7 atoms and code lists can be easily converted to strings
 using predicates `atom_string/2` and `string_codes/2`.

***`Options`*** is a list of name-value pairs that represent
 the options described below.
A name-value pair is a unary predicate,
 i.e. a Prolog term of the form `Name(Value)`.
The following options are supported:

  1. `eyes(+callable)`
     A DCG that produces exactly 2 characters
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

  3. `mode(+oneof(['Borg',dead,default,greedy,paranoia,stoned,tired,wired,youth]))`
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
     A DCG that emits exactly one character, replacing the tongue of the cow.
     Default: `" "`.
     If the `tongue` option is absent but the `mode` option is present,
      the tongue may be set by the indicated mode.

  7. `wait(+boolean)`
     Whether consecutive executions of **Cowsay** should wait
      for each other's speech synthesizer to complete.
     Default: `true`.

  8. Other options are passed to `dcg_wrap//1`.

---

## Further reading

This project is written in [**SWI-Prolog 7**](http://www.swi-prolog.org/)
 and uses the following techniques and features:

  1.  **Descriptive Clause Grammars (DCGs)** are used to draw
       the cow and the speech bubble, and are used to perform word-wrapping
       on the input.
      Anne Ogborn wrote
       [an introduction to Definite Clause Grammars (DCGs) in (SWI-)Prolog](http://www.pathwayslms.com/swipltuts/dcg/).
      This project imports several often reused DCG rules from
       [library **plDcg**](https://github.com/wouterbeek/plDcg.git),
       e.g., the DCG rule that wraps the given input string w.r.t.
       the maximum speech bubble width.
  2.  Documentation throughout this project uses
       the [**plDoc 2** format](http://www.swi-prolog.org/pldoc/package/pldoc.html)
       by Jan Wielemaker.
  3.  Options for predicates are explicitly denoted by using the
       [*predicate options library*](http://www.swi-prolog.org/pldoc/man?section=predicate_options).
      Options are processed by predicates from the
       [*options library*](http://www.swi-prolog.org/pldoc/man?section=option).
  4.  [**Strings**](http://www.swi-prolog.org/pldoc/man?section=strings)
      are used as the input to the predicate that draws the cow.
      Strings were introduced in SWI-Prolog 7 by Jan Wielemaker.
  5.  This project imports several often occuring predicates from the
      [**Prolog-Library-Collection**](https://github.com/wouterbeek/Prolog-Library-Collection.git).
      For example `string_list_concat/3` replicates the behavior of
      the SWI-Prolog built-in `atomic_list_concat/3`.

--

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

