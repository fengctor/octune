# Octune
<img src="logo.png" alt="Octune logo" width="400"/>

Logo made by [@Weelam](https://github.com/Weelam) 🙏

Octune is a domain-specific language (DSL) for creating 8-bit music,
compiling to WAV.
The name is a pun on "oct-" for the 8-bit music produced and
"tune" for, well, tune 🙂.

- [Usage](#usage)
- [Language Description](#language-description)
  * [Syntax](#syntax)
  * [Semantics](#semantics)
    + [Line Expressions](#line-expressions)
    + [Songs](#songs)
- [Octune Music Examples](#octune-music-examples)
- [Documentation TODO](#documentation-todo)

# Usage
Octune is used simply by feeding the executable the Octune song files (`.otn`)
used for constructing the song.
Using the module name, say `M`, containing `main`,
it will produce `M.wav` in the current directory.
For example, if `mainModule.otn` declares `module Main` containing `main`
and possibly uses variables from `other1.otn ... otherk.otn`,
`path/to/octune mainModule.otn other1.otn ... otherk.otn`
produces `Main.wav` if the files are successfully compiled.

# Language Description
## Syntax
The syntax for an Octune file is given by the following grammar in EBNF:
```
<file> ::= "module" <module_path> {<declaration>}+

<module_path> ::= <module_component>
               |  <module_component> , ".", <module_path>

<module_component> ::= capital letter followed by 0 or more letters


<declaration> ::= <variable> "=" <song>
               |  <variable> "=" <line_expression>

<variable> ::= lowercase letter followed by 0 or more alphanumeric characters


<song> ::= "{" <bpm> ":" <line_expression> "}"

<bpm> ::= positive integer


<line_expression> ::= <note>
                   |  <variable_usage>
                   |  <sequence>
                   |  <merge>
                   |  <repeat>
                   |  <volume_modify>


<note> ::= {<note_modifier>}, <beats>, <sound>

<note_modifier> ::= "'"
                 |  "''"

<beats> ::= <non_negative_decimal>
         |  <musical_length> {"."}

<non_negative_decimal> ::= non-negative decimal number

<musical_length> ::= "t" | "s" | "e" | "q" | "h"

<sound> ::= "_"
         |  <pitch>
         |  <percussion>

<pitch> ::= <letter>, [<accidental>], <octave>

<letter> ::= "A" | "B" | ... | "G"

<accidental> ::= "b" | "#"

<octave> ::= "0" | "1" | ... | "8"

<percussion> ::= "%" | "%%"


<variable_usage> ::= <variable>
                  |  <module_component>, ".", <variable_usage>


<sequence> ::= "[" {<line_expression>}+ "]"


<merge> ::= "[+" {<line_expression>}+ "+]"


<repeat> ::= "[*" <non_negative_int> ":" {<line_expression>}+ "*]"

<non_negative_int> ::= non-negative integer


<volume_modify> ::= "[!" <non_negative_decimal> ":" {<line_expression>}+ "!]"
```
Furthermore, line comments are preceded by `--` and
block comments are surrounded by `{-` and `-}`.
## Semantics
### Line Expressions
Octune songs are fundamentally created by composing (😉) entities called
"line expressions",
and then declaring the desired bpm for the line expression to be played at.
The basic building block of a line expression is a "note",
which can be combined with other line expressions
to create line expressions of their own.

#### Notes
A note is primarily denoted by its length (in terms of beats)
and its sound with no space between them.

The length may be specified by
a rational number in decimal form (like `1`, `2`, `3.5`, ...) or by
musical note length (`t` = thirty-second note, `s` = sixteenth note,
`e` = eigth note, `q` = quarter note, `h` = half note).
Musical note lengths may further by followed by dots (`.`)
to denote dotted notes, so that
`q.` is 1.5 beats, `q..` is 1.75 beats, `h.` is 3 beats, and etc.

The sound can be one of the following:
- a rest (`_`) resulting in silence
- a pitch given in scientific pitch notation (like `A4`, `G#2`, `Db6`)
- a light percussion sound (`%`)
- a heavier percussion sound (`%%`)

Notes may also be prefixed with 0 or more modifiers,
which slightly change how the note is played:
- `'` denotes the "detached" modifier,
making the last 20% of the note's duration silent
- `''` denotes the "staccato" modifier,
making the last 75% of the note's duration silent

Examples:
- `h._`
- `'qC4`
- `sBb3`
- `''6.5F#7`
- `s%`
- `e..%%`

#### Sequences
Sequences are denoted by a whitespace-separated list of line expressions
surrounded by `[` and `]`.
They represent playing those line expressions one at a time.

Examples:
- `[ 'eC4 eC4 'eG4 eG4 'eA4 eA4 qG4 ]`
- `[ [ q.C4 eD4 ] [ qE4 ] qF4 hG4 ]`

#### Merges
Merges are denoted by a whitespace-separated list of line expressions
surrounded by `[+` and `+]`.
They represent playing those line expressions simultaneously.
Note that the length of a merge is the length of its longest argument.

Examples:
- `[+ [hC3 qF3 qC3] [ 'eC4 eC4 'eG4 eG4 'eA4 eA4 qG4 ] +]`
- `[+ hF2 [ eF4 eA4 [+ eC5 eF5 +] ] [ eF5 eC5 eA4 ] +]`

#### Repeats
Sequences are denoted by a non-negative integer `n`, followed by `:`,
followed by a whitespace-separated list of line expressions,
together surrounded by `[*` and `*]`.
They represent the sequence of the given line expressions (played one at a time),
but with the sequence repeated `n` times.

Examples:
- `[* 2 : [+ 'sD2 'sD3 +] eF#2 e_ [* 4 : s%% *] *]`

#### Variables
Variables can hold line expressions, and when they do they may be used by their names
as line expressions themselves.
Valid variable names are those that start with a lowercase letter,
followed by any alphanumeric character.
They represent the line expression that they were declared with.

Variables are declared at the top level by `variableName = <some line expression>`,
and only variables that have been declared can be used.
Order of declarations is not important, but variables may not cyclically reference each other.

Examples:
- `soprano`
- `bass1Second`
- `sOm3Th1n6`

### Songs
A song is the entrypoint for music generation, separate from line expressions.
It is denoted by a non-negative integer `bpm` (the song's speed in beats per minute),
followed by `:`, followed by a single line expression,
together surrounded by `{` and `}`.
They represent the given line expression being played at `bpm` beats per minute.

Music generation starts at the `main` variable, which must exist and be declared
with a song.

Examples:
- `main = { 136 : [+ melody bass +] }`
- `main = { 60 : [* 2 : twinkleTwinkle *] }`

# Octune Music Examples
The `samples/` directory in this repository contains Octune code
for a few well-known songs.
Try compiling them and giving them a listen!

# Documentation TODO
- Simple module system
- Beats assertions
- Volume modifier block
- Getting Octune
