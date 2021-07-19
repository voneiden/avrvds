module Util.ParserUtil exposing (parserDeadEndsToString)

import Parser exposing (DeadEnd, Problem(..))
parserDeadEndsToString : List DeadEnd -> String
parserDeadEndsToString deadEnds =
  String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
  problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
  case p of
   Expecting s -> "expecting '" ++ s ++ "'"
   ExpectingInt -> "expecting int"
   ExpectingHex -> "expecting hex"
   ExpectingOctal -> "expecting octal"
   ExpectingBinary -> "expecting binary"
   ExpectingFloat -> "expecting float"
   ExpectingNumber -> "expecting number"
   ExpectingVariable -> "expecting variable"
   ExpectingSymbol s -> "expecting symbol '" ++ s ++ "'"
   ExpectingKeyword s -> "expecting keyword '" ++ s ++ "'"
   ExpectingEnd -> "expecting end"
   UnexpectedChar -> "unexpected char"
   Problem s -> "problem " ++ s
   BadRepeat -> "bad repeat"
