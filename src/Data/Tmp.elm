
overviewParser : Parser Section
overviewParser =
    succeed Section
        |. spaces
        |= succeed "Overview"
        |= (stringTrim <| getChompedString <| succeed () |. chompUntilEndOr "\n#")

sectionParser : Parser Section
sectionParser =
    succeed Section
        |. spaces
        |= (stringTrim <| getChompedString <| succeed () |. chompUntil "\n" |. chompIf (\c -> c == '\n'))
        --|= (stringTrim <| getChompedString <| succeed () |. chompUntilEndOr "\n#")
        |= sectionBodyParserLoop




sectionBodyParserLoop : Parser String
sectionBodyParserLoop =
    let
        looper : String -> Parser (Step (String) (String))
        looper body =
            oneOf
                [ map (\s -> Loop (body ++ s)) <| getChompedString <| succeed ()
                    |. symbol "```"
                    |. chompUntil "```"
                    |. symbol "```"
                    |. chompUntilEndOr "\n"
                 -- TODO backtrackable madness? chompIf not "#", map to some boolean, commit, problem
                ,  map (\s -> Loop (body ++ s)) <| getChompedString <| succeed ()
                    |. chompUntil "\n"
                    |. symbol "\n"
                , end
                    |> map (\_ -> Done body)
                ,  map (\s -> Loop (body ++ s)) <| getChompedString <| succeed ()
                    |. spaces
               ]
    in
    loop "" looper

chapterParser : Parser Chapter
chapterParser =
    succeed Chapter
        |. spaces
        |= (getChompedString
            <| succeed ()
                |. chompUntilEndOr "\n"
            )
        |. spaces
        |= succeed Nothing
        |= succeed []
        |= succeed []
        |> andThen chapterParserLoop

chapterParserLoop : Chapter -> Parser Chapter
chapterParserLoop baseChapter =
    let
        looper : Chapter -> Parser (Step (Chapter) (Chapter))
        looper chapter =
            oneOf
                [ succeed (\section -> Loop {chapter | overview = Just section})
                    |. symbol "## Overview\n"
                    |= overviewParser
                    |. spaces
                , succeed (\section -> Loop {chapter | modules = section :: chapter.modules})
                    |. symbol "## Module "
                    |= sectionParser
                    |. spaces
                , succeed (\section -> Loop {chapter | topics = section :: chapter.topics})
                    |. symbol "## Topic "
                    |= sectionParser
                    |. spaces
                , end
                    |> map (\_ -> Done chapter)
               ]
    in
    loop baseChapter looper

tomeParser : Parser (Tome)
tomeParser =
    loop defaultTome tomeParserHelper

parseTome : String -> Result (List DeadEnd) Tome
parseTome s =
    run tomeParser s

someSpaces : Parser ()
someSpaces =
    chompIf (\c -> c == ' ' || c == '\n' || c == '\r')
        |. chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')

tomeParserHelper : Tome -> Parser (Step (Tome) (Tome))
tomeParserHelper tome =
    oneOf
        [ succeed (\chapter -> Loop {tome | chapters = chapter :: tome.chapters})
            |. symbol "# "
            |= chapterParser
        , end
            |> map (\_ -> Done tome)
        --, chompUntilEndOr "\n"
        --    |. spaces
        --    |> map (\_ -> Loop tome)
        , someSpaces
            |> map (\_ -> Loop tome)
        ]


--addSection : Tome -> String -> Step Tome Tome
--addSection tome sectionTitle =

--addSubSection : Tome -> String -> Parser Tome
--addSubSection tome sectionTitle =
--    List.reverse tome.chapters
