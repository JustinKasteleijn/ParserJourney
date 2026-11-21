# ParserJourney

Just me developing my own parser library as preparation for Advent of Code :)

# Combinators 

# Parser Combinators Roadmap

## BasicCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `item` | [x] | [x] |
| `satisfy` | [x] | [x] |
| `eof` | [ ] | [ ] |
| `choice` | [ ] | [ ] |
| `optional` | [ ] | [ ] |
| `unexpected` | [ ] | [ ] |
| `<?>` | [x] | [ ] |

## CharacterCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `anyChar` | [ ] | [ ] |
| `char` | [x] | [x] |
| `token` | [x] | [x] |
| `oneOf` | [ ] | [ ] |
| `noneOf` | [ ] | [ ] |
| `digit` | [x] | [x] |
| `digit0` | [x] | [x] |
| `digit1` | [x] | [x] |
| `letter` | [x] | [x] |
| `letter0` | [x] | [x] |
| `letter1` | [x] | [x] |
| `alphaNum` | [ ] | [ ] |
| `alphaNum0` | [ ] | [ ] |
| `alphaNum1` | [ ] | [ ] |
| `upper` | [ ] | [ ] |
| `upper0` | [ ] | [ ] |
| `upper1` | [ ] | [ ] |
| `lower` | [ ] | [ ] |
| `lower0` | [ ] | [ ] |
| `lower1` | [ ] | [ ] |
| `space` | [ ] | [ ] |
| `spaces` | [ ] | [ ] |
| `newline` | [x] | [x] |
| `tab` | [ ] | [ ] |
| `horizontalSpace` | [ ] | [ ] |

## SequenceCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `string` | [x] | [x] |
| `tokens` | [x] | [x] | 
| `takeWhile` | [ ] | [ ] |
| `takeWhile1` | [ ] | [ ] |
| `takeTill` | [ ] | [ ] |
| `rest` | [ ] | [ ] |

## RepetitionCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `many` | [ ] | [ ] |
| `some` | [ ] | [ ] |
| `count` | [ ] | [ ] |
| `skipMany` | [ ] | [ ] |
| `skipSome` | [ ] | [ ] |
| `between` | [ ] | [ ] |

## SeparationCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `sepBy` | [ ] | [ ] |
| `sepBy1` | [ ] | [ ] |
| `sepEndBy` | [ ] | [ ] |
| `sepEndBy1` | [ ] | [ ] |
| `endBy` | [ ] | [ ] |
| `endBy1` | [ ] | [ ] |

## NumberCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `decimal` | [ ] | [ ] |
| `natural` | [ ] | [ ] |
| `integer` | [ ] | [ ] |
| `hexadecimal` | [ ] | [ ] |

## LexerCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `lexeme` | [ ] | [ ] |
| `symbol` | [ ] | [ ] |
| `identifier` | [ ] | [ ] |
| `parens` | [ ] | [ ] |
| `brackets` | [ ] | [ ] |
| `braces` | [ ] | [ ] |
| `reserved` | [ ] | [ ] |
| `reservedOp` | [ ] | [ ] |

## LookAheadCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `try` | [ ] | [ ] |
| `lookAhead` | [ ] | [ ] |
| `notFollowedBy` | [ ] | [ ] |

## ExpressionCombinators

| Combinator | Implemented | Tested |
|---|:---:|:---:|
| `chainl1` | [ ] | [ ] |
| `chainr1` | [ ] | [ ] |
