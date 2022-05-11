# Command interpretations

## \let

```tex
\let\cs=<token>
```

Gives `\cs` the token’s current meaning.

If the token is another control sequence, \cs will acquire the same significance as that control
sequence.

For example, if you say ‘\let\a=\def’, you could then say ‘\a\b...{...}’ to define
a macro \b, because \a would behave like TEX’s primitive \def command.

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

If the token is a single character i.e., a (character code, category code) pair, then the control sequence will behave to a certain extent like that character; but there are some differences. For example, after `\let\zero=0` you can’t use `\zero` in a numerical constant, because TEX requires the tokens in a numerical constant to be digits, after macro expansion; \zero is not a macro, so it doesn’t expand.

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

> Conditionals. When an \if... is expanded, TEX reads ahead as far as nec- essary to determine whether the condition is true or false; and if false, it skips ahead (keeping track of \if...\fi nesting) until finding the \else, \or, or \fi that ends the skipped text. Similarly, when \else, \or, or \fi is expanded, TEX reads to the end of any text that ought to be skipped. The “expansion” of a conditional is empty. (Conditionals always reduce the number of tokens that are seen by later stages of the digestive process, while macros usually increase the number of tokens.)
