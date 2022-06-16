# Command interpretations

## \let

```tex
\let\cs=<token>
```

Gives `\cs` the token’s current meaning.

If the token is another control sequence, \cs will acquire the same significance as that control
sequence.

For example, if you say ‘\let\a=\def’, you could then say ‘\a\b...{...}’ to define
a macro \b, because \a would behave like Tex’s primitive \def command.

If you say,

```tex
\let\a=\b
\let\b=\c
\let\c=\a
```

you have interchanged the former meanings of `\b` and `\c`.

If you say,

```tex
\outer\def\a#1.{#1:}
\let\b=\a
```

the effect is exactly the same as,

```tex
\outer\def\b#1.{#1:}
\let\a=\b
```

If the token is a single character i.e., a (character code, category code) pair, then the control sequence will behave to a certain extent like that character; but there are some differences. For example, after `\let\zero=0` you can’t use `\zero` in a numerical constant, because Tex requires the tokens in a numerical constant to be digits, after macro expansion; \zero is not a macro, so it doesn’t expand.

## \advance

```tex
\count0=12
{
  \count0=4
  \global\advance\count0 by 2
  \showthe \count0

  \advance\count0 by 4
  \showthe \count0
}
\showthe \count0
```

Output:

```plaintext
6
6
```

This tells us that 'global', when applied to variable modifications like '\advance', applies to the write, not the read. i.e. we look up the variable value to modify, in local scope. Then we apply our modification to get our new value, then we apply the new value to the global scope, like a normal assignment.

## Conditionals

Quote from Texbook:

> Conditionals. When an \if... is expanded, Tex reads ahead as far as nec- essary to determine whether the condition is true or false; and if false, it skips ahead (keeping track of \if...\fi nesting) until finding the \else, \or, or \fi that ends the skipped text. Similarly, when \else, \or, or \fi is expanded, Tex reads to the end of any text that ought to be skipped. The “expansion” of a conditional is empty. (Conditionals always reduce the number of tokens that are seen by later stages of the digestive process, while macros usually increase the number of tokens.)

## \string

From p211:

```tex
\string⟨lex-token⟩
```

If ⟨lex-token⟩ is a control-symbol, its expansion is the control symbol name, including \escapechar as an escape character unless the control symbol is  an active character.

Otherwise the ⟨token⟩ is a character token, and its character code is retained as the expanded result.

From p39:

For example, `\string\TeX` produces four tokens:

(\\, Other), (T, Other), (e, Other), (X,Other)

The command creates char-code sequences that are then made into made-lex-tokens. See section 'A'. The backslash that \string inserts to represent an escape character, is made into a lex token by the same rules.

## A: Made tokens (\the \number, \romannumeral, \string, \meaning, \jobname, and \fontname)

From p213:

In all of the cases listed so far, `\the` produces a result that is a sequence of ASCII character tokens. Category code “other” is assigned to each token, except that character code 32 gets category “space”.

## \the

```tex
\the⟨internal quantity⟩
```

The expansion is a list of tokens representing the current value of one of Tex’s variables.

For example, ‘\the\skip5’ might expand into ‘5.0pt plus 2.0fil’.

\the has many subcases, so we shall discuss them one at a time.

- \the⟨parameter⟩, where ⟨parameter⟩ is a non-token-list parameter.
- \the⟨register⟩, where ⟨register⟩ is is a non-token-list register location.
- \the⟨codename⟩⟨8-bit number⟩: For example, \the\mathcode‘/ produces the current (integer) math code value for a slash
- \the⟨special register⟩
- \the\fontdimen⟨parameter number⟩⟨font⟩. This produces a dimension; for example, parameter 6 of a font is its “em” value, so ‘\the\fontdimen6\tenrm’ yields ‘10.0pt’ (six tokens).
- \the\hyphenchar⟨font⟩, \the\skewchar⟨font⟩. These produce the corresponding integer values defined for the specified font.
- \the\lastpenalty, \the\lastkern, \the\lastskip. These yield the amount of penalty, kerning, glue, or muglue in the final item on the current list, provided that the item is a penalty, kern, or glue, respectively; otherwise they yield ‘0’ or ‘0.0pt’.
- \the⟨defined character⟩, where ⟨defined character⟩ is a control sequence that has been given an integer value with \chardef or \mathchardef; the result is that integer value, in decimal notation.

These all create char-code sequences that are made into made-lex-tokens. See section 'A'.

There also are cases in which `\the` produces non-character tokens, either a font identifier like \tenrm, or an arbitrary token list:

- \the⟨font⟩ produces a font identifier that selects the specified font. For example, ‘\the\font’ is a control sequence corresponding to the current font.
- \the⟨token variable⟩ produces a copy of the token list that is the current value of the variable. For example, you can expand ‘\the\everypar’ and ‘\the\toks5’.

## Expansion inhibition

Expansion is suppressed at the following times:

- Deleting tokens during error recovery
- Skipping ignored conditional text.
- Reading macro arguments
- Reading the name of a control sequence defined by:
  - \let
  - \futurelet
  - \def, \gdef, \edef, \xdef
  - \chardef, \mathchardef, \countdef, \dimendef, \skipdef, \muskipdef, \toksdef
  - \read
  - \font
- Reading arguments for:
  - \expandafter
  - \noexpand
  - \string
  - \meaning
  - \let
  - \futurelet
  - \ifx
  - \show
  - \afterassignment
  - \aftergroup
- Reading macro definition parameter text of a \def, \gdef, \edef, or \xdef
- Reading the replacement text of a \def or \gdef or \read; or the text of a token variable like \everypar or \toks0; or the token list for \uppercase or \lowercase or \write. (The token list for \write will be expanded later, when it is actually output to a file.)
- Reading the preamble of an alignment, except after a token for the primitive command \span or when reading the ⟨glue⟩ after \tabskip.
- Just after a $_3 token that begins math mode, to see if another $_3 follows
- Just after a ‘_12 token that begins an alphabetic constant

## Set-box with explicit box

A command like:

```tex
\setbox⟨number⟩=\hbox to⟨dimen⟩{⟨horizontal mode material⟩}
```

causes Tex to evaluate the ⟨number⟩ and the ⟨dimen⟩, and to put those values on a “stack” for safe keeping.

Then Tex reads the ‘{’ (an explicit or implicit begin-group character), and this initiates a new level of grouping.

Then Tex enters 'restricted-horizontal' mode and executes commands in that mode.

An arbitrarily complex box can now be constructed; the fact that this box is eventually destined for a \setbox command does not affect Tex’s behavior while the box is being built

When the matching ‘}’ appears, Tex:

- Restores values that were changed by assignments in the group just ended
- Packages the hbox, using the size that was saved on the stack
- Completes the \setbox command
- Returns to the mode it was in at the time of the \setbox

## Box natural dimensions

Here’s the way TEX goes about setting the glue when an hbox is being wrapped up: The natural width, x, of the box contents is determined by adding up the widths of the boxes and kerns inside, together with the natural widths of all the glue inside.

The vertical analog of \hbox is \vbox, and TEX will obey the commands ‘\vbox to⟨dimen⟩’ and ‘\vbox spread⟨dimen⟩’ in about the way you would expect, by analogy with the horizontal case. However, there’s a slight complication because boxes have both height and depth in the vertical direction, while they have only width in the horizontal direction. The dimension in a \vbox command refers to the final height of the vbox, so that, for example, ‘\vbox to 50pt{...}’ produces a box that is 50 pt high; this is appropriate because everything that can stretch or shrink inside a vbox appears in the part that contributes to the height, while the depth is unaffected
by glue setting.
􏰟 The depth of a constructed \vbox is best thought of as the depth of the bottom box inside. Thus, a vbox is conceptually built by taking a bunch of boxes and arranging them so that their reference points are lined up vertically; then the reference point of the lowest box is taken as the reference point of the whole, and the glue is set
so that the final height has some desired value.
􏰟 However, this description of vboxes glosses over some technicalities that come up when you consider unusual cases. For example, TEX allows you to shift boxes in a vertical list to the right or to the left by saying ‘\moveright⟨dimen⟩⟨box⟩’ or ‘\moveleft⟨dimen⟩⟨box⟩’; this is like the ability to \raise or \lower boxes in a horizontal list, and it implies that the reference points inside a vbox need not always lie in a vertical line. Furthermore, it is necessary to guard against boxes that have too much depth, lest they extend too far into the bottom margin of a page; and later chapters will point out that vertical lists can contain other things like penalties and
marks, in addition to boxes and glue.
􏰟􏰟 Therefore, the actual rules for the depth of a constructed vbox are somewhat TEXnical. Here they are: Given a vertical list that is being wrapped up via \vbox, the problem is to determine its natural depth. (1) If the vertical list contains no boxes, the depth is zero. (2) If there’s at least one box, but if the final box is followed by kerning or glue, possibly with intervening penalties or other things, the depth is zero. (3) If there’s at least one box, and if the final box is not followed by kerning or glue, the depth is the depth of that box. (4) How- ever, if the depth computed by rules (1), (2), or (3) exceeds \boxmaxdepth, the depth will be the current value of \boxmaxdepth. (Plain TEX sets \boxmaxdepth to the largest possible dimension; therefore rule (4) won’t apply unless you specify a smaller value. When rule (4) does decrease the depth, TEX adds the excess depth to the box’s natural height, essentially
moving the reference point down until the depth has been reduced to the stated maximum.)

## Box dimension specifications

the command ‘\hbox spread⟨dimen⟩{⟨contents of box⟩}’ creates a box whose width w is a given amount more than the natural width of the contents.

## rules (\hrule, \vrule)

If you specify a dimension twice, the second specification overrules the first.

If you leave a dimension unspecified, you get the following by default:

hrule:
width: *
height: 0.4pt
depth: 0pt

vrule:
width: 0.4pt
height: *
depth: 0

‘*’ means that the actual dimension depends on the context: the rule will extend to the boundary of the smallest box or alignment that encloses it.
