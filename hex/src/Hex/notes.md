# Notes

## Scratch

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

- _Categorise_: Generate Categorised tokens
  - Interface: MonadCharCatSource
  - Implementation:
    - Main function: `extractCharCat`
    - Uses:
      - MonadHexState
      - ByteString state
- _Lex_: Generate Lexed tokens
  - Interface: MonadLexTokenSource
    - This interface also lets us access and modify the char-source that contains the input.
  - Implementation:
    - Main function: `extractToken`
    - Uses:
      - MonadHexState
      - CharSource state
- _Resolve_: Generate resolved tokens
  - Interface: MonadResolve
    - This interface isn't actually a stream interface, the interface is about resolving tokens.
      Paired with a source of unresolved tokens, ie MonadLexTokenSource, we can build a resolved-token-source.
  - Implementation:
    - Uses:
      - HSt.MonadHexState m
- _Expand_: Generate PrimTokens
  - Interface: MonadPrimTokenSource
  - Implementation:
    - Uses MonadResolve
- _Parse_: Generate ASTTokens like 'Command's
  - Interface: MonadCommandSource
  - Implementation:
    - Uses MonadPrimTokenSource
- _Evaluate_: Evaluate ASTTokens into EvaluatedTokens, like evaluated 'Command's
  - Interface: MonadEvaluate
    - This interface isn't actually a stream interface, the interface is about evaluating tokens. Paired with a source of unevaluated tokens, eg MonadCommandSource, we can build an evaluated-token-source.
  - Implementation: (None)
- _Interpret_: Interpret evaluated 'Command's
  - Interface: functions `buildMainVList`, `buildParaList`, `handleModeIndependentCommand`
  - I call this 'Interpret' because we do more than 'build', we also modify the internal state, with things like assignments.

## Overarching concerns

- Handle internal state (parameters, fonts, symbol tables)
  - Interface: MonadHexState
  - Implementation:
    - MonadIO m
    - MonadState st m
    - HasType HexState st
