# Notes

## Glossary

- Lexing takes char-cats, lexes them into 'lex-tokens'
  - We only lex once, so we require a 'char-source' to store lex-tokens that we've pushed back into the input.
  - A 'CharSource' is like a single file: a source of individual char-codes.
    - It has its own lex-state.
- Resolution takes lex-tokens, resolves them into 'resolved tokens'
- Expansion takes 'resolved tokens', and produces 'primitive tokens'

## What order does stuff happen in?

- *Categorise*: Generate Categorised tokens
  - Interface: function `extractCharCat`
  - Implementation:
    - Uses:
      - MonadHexState
- *Lex*: Generate Lexed tokens
  - Interface: function `extractToken`
  - Implementation:
    - Uses:
      - MonadHexState
- *Resolve*: Generate resolved tokens
  - Interface: MonadResolvedTokenSource
  - Implementation:
    - Uses:
      - MonadState st m,
      - HasType H.Par.ChrSrc.CharSource st,
      - HSt.MonadHexState m
- *Expand*: Generate PrimTokens
  - Interface: MonadPrimTokenSource
  - Implementation:
    - Uses MonadResolvedTokenSource
- *Parse*: Generate ASTTokens like 'Command's
  - Interface: MonadParse
  - Implementation:
    - Uses MonadPrimTokenSource
- *Evaluate*: Evaluate ASTTokens into EvaluatedTokens, like evaluated 'Command's
  - Interface: MonadEvaluated
  - Implementation: (None)
- *Build*: Interpret evaluated 'Command's
  - Interface: functions `buildMainVList`, `buildParaList`, `handleModeIndependentCommand`

## Overarching concerns

- Handle internal state (parameters, fonts, symbol tables)
  - Interface: MonadHexState
  - Implementation:
    - MonadIO m
    - MonadState st m
    - HasType HexState st
