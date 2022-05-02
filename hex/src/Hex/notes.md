# Notes

## Scratch

- wondering how to handle end-of-input.
  - currently interfaces return 'maybe x', where nothing means end-of-input.
  - I think this interface might be fine, and I can provide a helper function using the interface to merge it into an error.
  - For testing it's nice to just finish
  - but in the code it's easier to treat is as an error then not have to handle it.
  - How easy is it to catch the error in testing code? I think not v hard.
  - Decision: I think I will treat it as an error.

## Glossary

- Categorise has an explicit implementation, and also a monad interface and implementation, MonadCharCatSource.
  - The class only has one method, it's just for consistency with the other stages.
  - One difference is that it expects just a 'bytestring' in its state, rather than a 'CharSource'. Because we don't use the lex-state or care about lex-tokens at this stage.
- Lexing takes char-cats, lexes them into 'lex-tokens'
  - We only lex once, so we require a 'char-source' to store lex-tokens that we've pushed back into the input.
  - A 'CharSource' is like a single file: a source of individual char-codes.
    - It has its own lex-state.
- Resolution takes lex-tokens, resolves them into 'resolved tokens'
  - There is some statefulness to this operation, we can be 'resolving' or 'not-resolving'.
    - If we are resolving, we pass through all lex-tokens as 'unresolved-tokens' containing the lex-tokens. No lookup of any symbols
    - If we are not-resolving, we look up control-symbols in the internal-state. Non-control-symbols like normal letters and stuff, are treated the same as when resolving.
- Expansion takes 'resolved tokens', and produces 'primitive tokens'
  - Expansion in fact does some parsing, so it might turn out that this gets quite intermingled with the 'Parsing' stage. That isn't worked out quite yet as I have the code now.
- Parsing takes 'primitive tokens' and produces 'commands'.

## What order does stuff happen in?

- *Categorise*: Generate Categorised tokens
  - Interface: MonadCharCatSource
  - Implementation:
    - Main function: `extractCharCat`
    - Uses:
      - MonadHexState
      - ByteString state
- *Lex*: Generate Lexed tokens
  - Interface: MonadLexTokenSource
    - This interface also lets us access and modify the char-source that contains the input.
  - Implementation:
    - Main function: `extractToken`
    - Uses:
      - MonadHexState
      - CharSource state
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
  - Interface: MonadCommandSource
  - Implementation:
    - Uses MonadPrimTokenSource
- *Evaluate*: Evaluate ASTTokens into EvaluatedTokens, like evaluated 'Command's
  - Interface: MonadEvaluate
    - This interface isn't actually a stream interface, the interface is about evaluating tokens. Paired with a source of unevaluated tokens, eg MonadCommandSource, we can build an evaluated-token-source.
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
