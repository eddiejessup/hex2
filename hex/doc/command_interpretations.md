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
