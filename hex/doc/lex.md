# Lexing

## Handling input as lines

The input to TeX is a sequence of “lines.” Whenever TeX is reading a line of text from a file, or a line of text that you entered directly on your terminal, the computer’s reading apparatus is in one of three so-called states:

- Beginning a new line (N)
- Middle of a line (M)
- Skipping blanks (S)

At the beginning of every line it’s in state N; but most of the time it’s in state M, and after a control word or a space it’s in state S.

TeX deletes any ⟨space⟩ characters (number 32) that occur at the right end of an input line. Then it inserts a `\endlinechar` character (default value 13) at the right end of the line. Note that `\endlinechar` is considered to be an actual character that is part of the line; you can obtain special effects by changing its catcode. If the value of \endlinechar is negative or greater than 255, no character is appended, and the effect is as if every line ends with a comment character.

If TeX has nothing more to read on the current line, it goes to the next line and enters state N. However, if \endinput has been specified for a file being \input, or if an \input file has ended, TeX returns to whatever it was reading when the \input command was originally given.

### Example

```tex
\endlinechar=65 This sentence does not end with 'A'.
\endlinechar=13 But this sentence does end with 'A'
\endlinechar=13 \end
```

In the above example, we don't get an 'A' at the end of the first line. This shows that the appending of `\endlinechar` happens when the line is first read, not once the line-end is reached. (Because if the latter behaviour held, we would append an 'A' at the end of the first line, after the assignment.)

## Escape character

If TeX sees an escape character (category 0) in any state, it scans the entire control sequence name as follows. (a) If there are no more characters in the line, the name is empty (like \csname\endcsname). Otherwise (b) if the next character is not of category 11 (letter), the name consists of that single symbol. Otherwise (c) the name consists of all letters beginning with the current one and ending just before the first nonletter, or at the end of the line. This name becomes a control sequence token. TeX goes into state S in case (c), or in
case (b) with respect to a character of category 10 (space); otherwise TeX goes into state M.

## Superscript cat-code (trioing)

If TeX sees a superscript character (category 7) in any state, and if that character is followed by another identical character, and if those two equal characters are followed by a character of code c < 128, then they are deleted and 64 is added to or subtracted from the code c. (Thus, ^^A is replaced by a single character whose code is 1, etc., as explained earlier.) However, if the two superscript characters are immediately followed by two of the lowercase hexadecimal digits 0123456789abcdef, the four-character sequence is replaced by a single character having the specified hexadecimal code. The replacement is carried out also if such a trio or quartet of characters is encountered during steps (b) or (c) of the control- sequence-name scanning procedure described above. After the replacement is made, TeX begins again as if the new character had been present all the time. If a superscript character is not the first of such a trio or quartet, it is handled by the following rule.

## End-of-line cat-code

If TeX sees an end-of-line character (category 5), it throws away any other information on the current line. Then if TeX is in state N (new line), the end-of-line character is converted to the control sequence token ‘par’ (end of paragraph); if TeX is in state M (mid-line), the end-of-line character is converted to a token for character 32 (‘␣’) of category 10 (space); and if TeX is in state S (skipping blanks), the end-of-line character is simply dropped.

## Ignored cat-code

If TeX sees a character to be ignored (category 9), it simply bypasses that character as if it weren’t there, and remains in the same state.

## Space cat-code

If TeX sees a character of category 10 (space), the action depends on the current state. If TeX is in state N or S, the character is simply passed by, and TeX remains in the same state. Otherwise TeX is in state M; the character is converted to a token of category 10 whose character code is 32, and TeX enters state S. The character code in a space token is always 32.

## Comment cat-code

If TeX sees a comment character (category 14), it throws away that character and any other information that might remain on the current line.

## Invalid cat-code

If TeX sees an invalid character (category 15), it bypasses that character, prints an error message, and remains in the same state.

## Other cat-codes

If TeX sees a character of categories 1, 2, 3, 4, 6, 8, 11, 12, or 13, or a character of category 7 that is not the first of a special sequence as just described, it converts the character to a token by attaching the category code, and goes into state M. This is the normal case; almost every nonblank character is handled by this rule.
