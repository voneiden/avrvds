module CustomMarkdown exposing (defaultHtmlRenderer)
import Data.ChipCdef exposing (ChipCdef)
import Html exposing (Html)
import Html.Attributes as Attr exposing (href)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer exposing (Renderer)
import Parser exposing ((|.), (|=), Nestable(..), Parser, Step(..), chompIf, chompUntil, chompWhile, end, getChompedString, int, keyword, lineComment, loop, map, multiComment, oneOf, run, spaces, succeed, symbol)
import String exposing (fromChar)

type Code =
    Keyword String | CType String | Constant String String | Register String | Plain String | Number String | StringLiteral String | Comment String | Fail


toHtml : Code -> Html a
toHtml code =
    case code of
        Keyword keyword -> Html.span [Attr.class "keyword"] [Html.text keyword]
        CType ctype -> Html.span [Attr.class "c-type"] [Html.text ctype]
        Constant constant helpText -> Html.span [Attr.class "constant", Attr.title helpText] [Html.text constant]
        Comment comment -> Html.span [Attr.class "c-comment"] [Html.text comment]
        Register register -> Html.span [Attr.class "register"] [Html.text register]
        Plain s -> Html.span [] [Html.text s]
        _ -> Html.span [] [Html.text "Something else"]


isWhitespace : Char -> Bool
isWhitespace c = c == ' ' || c == '\n' || c == '\r'

isEof : Char -> Bool
isEof c = c == '\n' || c == '\r'

whitespace : Parser ()
whitespace =
  chompIf isWhitespace
  |. chompWhile isWhitespace

notWhitespace : Parser ()
notWhitespace =
    chompIf (not << isWhitespace)
    |. chompWhile (not << isWhitespace)

isSymbol : Char -> Bool
isSymbol c = List.member (fromChar c) symbols

plain : Parser ()
plain =
    let
        condition = \c -> not (isWhitespace c) && not (isSymbol c)
    in
    chompIf condition
    |. chompWhile condition

anyToken : Parser String
anyToken =
  getChompedString <|
    succeed ()
      |. chompWhile (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r')


keywords : List String
keywords =
    [
          "asm",
          "auto",
          "break",
          "case",
          "const",
          "continue",
          "default",
          "do",
          "else",
          "enum",
          "extern",
          "for",
          "fortran",
          "goto",
          "if",
          "inline",
          "register",
          "restrict",
          "return",
          "sizeof",
          "static",
          "struct",
          "switch",
          "typedef",
          "union",
          "volatile",
          "while",
          "_Alignas",
          "_Alignof",
          "_Atomic",
          "_Generic",
          "_Noreturn",
          "_Static_assert",
          "_Thread_local",
          "alignas",
          "alignof",
          "noreturn",
          "static_assert",
          "thread_local",
          "_Pragma"
        ]

types : List String
types = [ "uint8_t"
        , "float"
        , "double"
        , "signed"
        , "unsigned"
        , "int"
        , "short"
        , "long"
        , "char"
        , "void"
        , "_Bool"
        , "_Complex"
        , "_Imaginary"
        , "_Decimal32"
        , "_Decimal64"
        , "_Decimal128"
        , "complex"
        , "bool"
        , "imaginary"
        ]


symbols : List String
symbols = ["(", "[", "{", "}", "]", ")", ";", ":", "-", "+", "=", "."]

continue : List a -> Parser (a -> Step (List a) b)
continue revBlocks = succeed (\block -> Loop (block :: revBlocks))

codeParser : ChipCdef -> Parser (List (Code))
codeParser cdef =
  loop [] (codeParserHelp2 cdef)


newlines : Parser ()
newlines =
  chompWhile (\c -> c == '\n')


constantHelpText : String -> ChipCdef -> String
constantHelpText constantName cdef =
    case List.filter (\x -> Tuple.first x == constantName) cdef.constants of
        [constant] -> Tuple.second constant
        _ -> ""


refElement : String -> List (Html a) -> Html a
refElement page _ =
    Html.span [] [Html.sup [] [Html.a [href "#"] [Html.text <| "[D]"]]]



codeParserHelp2 : ChipCdef -> List (Code) -> Parser (Step (List (Code)) (List (Code)))
codeParserHelp2 cdef revBlocks =
    oneOf
        [ oneOf (List.map keyword keywords)
            |> getChompedString
            |> map (\s -> Loop (Keyword s :: revBlocks))
        , oneOf (List.map keyword types)
            |> getChompedString
            |> map (\s -> Loop (CType s :: revBlocks))
        , oneOf (List.map keyword cdef.registers)
            |> getChompedString
            |> map (\s -> Loop (Register s :: revBlocks))
        , oneOf (List.map keyword (List.map (\x -> Tuple.first x) cdef.constants))
            |> getChompedString
            |> map (\s -> Loop (Constant s (constantHelpText s cdef) :: revBlocks))
        , lineComment "//"
            |. newlines
            |> getChompedString
            |> map (\s -> Loop (Comment s :: revBlocks))
        , multiComment "/*" "*/" NotNestable -- multiComment is buggy
            |. symbol "*/"
            |. newlines
            |> getChompedString
            |> map (\s -> Loop (Comment s :: revBlocks))
        , oneOf (List.map symbol symbols)
            |> getChompedString
            |> map (\s -> Loop (Plain s :: revBlocks))
        , whitespace
            |> getChompedString
            |> map (\s -> Loop (Plain s :: revBlocks))
        , plain
            |> getChompedString
            |> map (\s -> Loop (Plain s :: revBlocks))
        , succeed ()
            |> map (\_ -> Done (List.reverse revBlocks))
        ]
codeParserHelp : List (Code) -> Parser (Step (List (Code)) (List (Code)))
codeParserHelp revBlocks =
  oneOf
    [
    continue revBlocks
        |= map (\s -> Keyword s) ( getChompedString <|
            succeed ()
                |. oneOf
                    (List.map keyword keywords)
            )
    , succeed (\block -> Loop (block :: revBlocks))
        |= map (\s -> CType s) ( getChompedString <|
            succeed ()
                |. oneOf
                    (List.map keyword types)
            )
    , succeed (\block -> Loop (block :: revBlocks))
        |= map (\s -> Plain s) ( getChompedString <|
            succeed ()
                |. whitespace
        )
    , succeed (\block -> Loop (block :: revBlocks))
            |= map (\s -> Plain s) ( getChompedString <|
                succeed ()
                    |. notWhitespace
            )
    , succeed ()
                |> map (\_ -> Done (List.reverse revBlocks))
    --, succeed (\block -> Loop (block :: revBlocks))
    --         |= map (\s -> Plain s) anyToken

    ]
defaultHtmlRenderer : Maybe ChipCdef -> Renderer (Html msg)
defaultHtmlRenderer maybeCdef =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , strikethrough =
        \children -> Html.del [] children
    , codeSpan =
        \content -> Html.code []
            <| [Html.text "Span code"]
                --case run codeParser content of
                --    Ok elements -> elements
                --    Err _ -> [Html.text "OOPS"]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf
        [ Markdown.Html.tag "topic" (\children -> Html.div [] children)
        , Markdown.Html.tag "reg" (\children -> Html.div [] children)
        , Markdown.Html.tag "ref" refElement
            |> Markdown.Html.withAttribute "page"

        ]
    , codeBlock =
        \{ body, language } ->
            let
                classes =
                    -- Only the first word is used in the class
                    case Maybe.map String.words language of
                        Just (actualLanguage::_) ->
                            [ Attr.class <| "language-" ++ actualLanguage ]

                        _ ->
                            []
            in
            Html.pre []
                [ Html.code classes
                    --[ Html.text body
                    --]
                    --[ Html.text "codeBlock"]
                    <|
                    case maybeCdef of
                        Just cdef ->
                            List.map toHtml (
                                case run (codeParser cdef) body of
                                    Ok elements -> elements
                                    Err _ -> [Fail]
                                )
                        Nothing ->
                            [ Html.text body ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }
