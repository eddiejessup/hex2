# Notes

## What order does stuff happen in?

### _Read_

### _Expand_

### _Parse_

### _Evaluate_: Evaluate Parsed-Tokens into Evaluated-Tokens

Interface: HexEvaluate

This interface isn't actually a stream interface, the interface is about evaluating tokens. Paired with a source of unevaluated tokens, eg CommandSource, we can build an evaluated-token-source.

Implementation:

- EHexState

### _Interpret_ (_Execute_ and _Build_): Interpret evaluated 'Command's

Interface and implementation: functions `extractMainVList`, `extractParagraphList`, `handleModeIndependentCommand`

I call this stage 'Interpret' because we do more than 'build', we also modify the internal state, with things like assignments.

## _Render_

Emit DVI

## Overarching concerns

- Handle internal state (parameters, fonts, symbol tables)
  - Interface: EHexState
