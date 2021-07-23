module Data.Tome exposing (..)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), andThen, chompIf, chompUntil, chompWhile, commit, end, getChompedString, loop, map, oneOf, problem, run, succeed, symbol)
import Parser.Advanced exposing (chompUntilEndOr)
import Parser.Advanced as A exposing ((|=), (|.))
import Url exposing (Url)

type alias SubSection =
    { title : String
    , body : String
    }

type alias Section =
    { title : String
    , body : String
    , subSections: List SubSection
    }

type alias Document =
    { name : String
    , url : Url
    }

-- Chapter describes a single device
type alias Chapter =
    { title : String
    , documents: List Document
    , body: String
    , sections: List Section
    }

type alias Tome =
    { chapters : List Chapter
    , aliases : Dict String String
    }


defaultTome = Tome [] Dict.empty

stringTrim : Parser String -> Parser String
stringTrim = map (\s -> String.trim s)


addChapter : Tome -> (String, Maybe String) -> Parser (Step Tome Tome)
addChapter tome (chapterTitle, aliasName) =
    -- TODO validate documents (|> andThen validateTome)
    case aliasName of
        Just alias ->
            succeed <| Loop { tome | aliases = Dict.update chapterTitle (\_ -> Just alias) tome.aliases}
        Nothing ->
            -- Previous chapter should have
            succeed (Loop {tome | chapters = Chapter (String.trim chapterTitle) [] "" [] :: List.reverse tome.chapters,
                                  aliases = Dict.update chapterTitle (\_ -> Just chapterTitle) tome.aliases})
                |. validateLastChapter tome

validateLastChapter : Tome -> Parser ()
validateLastChapter tome =
    case List.reverse tome.chapters of
        chapter :: _ ->
            case List.filter (\d -> d.name == "Datasheet") chapter.documents of
                [_] -> succeed ()
                _ -> problem <| "Chapter \"" ++ chapter.title ++ "\" has no datasheet defined"
        _ ->
            succeed ()


validateTome : Tome -> Parser Tome
validateTome tome =
    let
        fold : Chapter -> Maybe String -> Maybe String
        fold chapter problemMsg =
            case problemMsg of
                Nothing ->
                    case List.filter (\d -> d.name == "Datasheet") chapter.documents of
                        [_] -> Nothing
                        _ -> Just <| "Chapter \"" ++ chapter.title ++ "\" has no datasheet defined"
                _ -> problemMsg
    in
    case List.foldl fold Nothing tome.chapters of
        Nothing -> succeed tome
        Just problemMsg -> problem problemMsg



type alias DocumentDefinition =
    { name: String
    , rawUrl: String
    }

addDocument : Tome -> DocumentDefinition -> Parser (Step Tome Tome)
addDocument tome documentDefinition =
    case Url.fromString documentDefinition.rawUrl of
            Just url ->
                case List.reverse tome.chapters of
                    chapter :: otherChapters ->
                        succeed <| Loop { tome | chapters = List.reverse <| {chapter | documents = chapter.documents ++ [Document documentDefinition.name url]} :: otherChapters }
                    _ ->
                        problem "Expected to see a chapter (#) before document"
            Nothing ->
                problem "Document does not contain a valid url"



addSection : Tome -> String -> Parser (Step Tome Tome)
addSection tome sectionTitle =
    let
        addSectionToChapter chapter =
            {chapter | sections = List.reverse <| Section (String.trim sectionTitle) "" [] :: List.reverse chapter.sections}
    in
    case List.reverse tome.chapters of
        chapter::otherChapters ->
            succeed <| Loop {tome | chapters = List.reverse <| addSectionToChapter chapter :: otherChapters}
        [] ->
            problem "Expected to see a chapter (#) before section (##)"

addSubSection : Tome -> String -> Parser (Step Tome Tome)
addSubSection tome subSectionTitle =
    let
        addSubSectionToSection section =
            {section | subSections = List.reverse <| SubSection (String.trim subSectionTitle) "" :: List.reverse section.subSections}
    in
    case List.reverse tome.chapters of
        chapter::otherChapters ->
            case List.reverse chapter.sections of
                section::otherSections ->
                    succeed <| Loop {tome |
                        chapters = List.reverse <| {chapter |
                            sections = List.reverse <| addSubSectionToSection section :: otherSections} :: otherChapters}
                [] ->
                    problem "Expected to see a section (##) before subsection (###)"
        [] ->
            problem "Expected to see a chapter (#) before subsection (###)"

addTextToBody : String -> String -> String
addTextToBody body text =
    String.trim (body ++ "\n" ++ text)

addText : Tome -> String -> Parser (Step Tome Tome)
addText tome text =
    case List.reverse tome.chapters of
            chapter::otherChapters ->
                case List.reverse chapter.sections of
                    section::otherSections ->
                        case List.reverse section.subSections of
                            subSection::otherSubSections ->
                                succeed <| Loop { tome | chapters = List.reverse <| {chapter | sections = List.reverse <| {section | subSections = List.reverse <| { subSection | body = addTextToBody subSection.body text} :: otherSubSections } :: otherSections} :: otherChapters }
                            [] ->
                                succeed <| Loop { tome | chapters = List.reverse <| {chapter | sections = List.reverse <| {section | body = addTextToBody section.body text} :: otherSections} :: otherChapters }
                    [] ->
                        succeed <| Loop { tome | chapters = List.reverse <| {chapter | body = addTextToBody chapter.body text} :: otherChapters}
            [] ->
                problem "Expected to see a chapter (#) before text"

parseStringUntilEnd : Parser String
parseStringUntilEnd =
    map String.trim <| getChompedString <| chompUntilEndOr "\n"



betterParser : Parser (Tome)
betterParser =
    let
        looper : Tome -> Parser (Step Tome Tome)
        looper tome =
            oneOf
            --succeed (\sectionTitle -> Loop tome)
                [ succeed ()
                    |. symbol "```"
                    |. chompUntil "```"
                    |. symbol "```"
                    |. chompUntilEndOr "\n"
                    |> getChompedString
                    |> andThen (addText tome)
                , succeed DocumentDefinition
                    |. symbol "* Document ["
                    |= (getChompedString <| chompUntil "]")
                    |. symbol "]("
                    |= (getChompedString <| chompUntil ")")
                    |. symbol ")"
                    |. chompWhile ((==) '\n')
                    |> andThen (addDocument tome)
                    --|> andThen (addDocument tome) -- TODO UH OH
                , succeed (\subSectionName -> subSectionName)
                    |. symbol "###"
                    |= (getChompedString <| chompUntilEndOr "\n")
                    |. chompWhile ((==) '\n')
                    |> andThen (addSubSection tome)
                , succeed (\sectionName -> sectionName)
                    |. symbol "##"
                    |= (getChompedString <| chompUntilEndOr "\n")
                    |. chompWhile ((==) '\n')
                    |> andThen (addSection tome)
                , succeed (\chapterName aliasName -> (String.trim (chapterName), aliasName))
                    |. symbol "#"
                    |= (getChompedString <| succeed () |. chompWhile (\c -> c /= '\n' && c /= '-'))
                    |= oneOf
                        --[ succeed (\x -> Just (String.trim x))
                        [ succeed (String.trim >> Just)
                            |. symbol "->"
                            |= (getChompedString <| succeed () |. chompWhile(\c -> c /= '\n'))
                            |. chompIf ((==) '\n')
                        , succeed (\_ -> Nothing)
                            |= chompIf ((==) '\n')
                        ]
                    |> andThen (addChapter tome)
                , end
                    |. commit ()
                    |. validateLastChapter tome
                    |> map (\_ -> Done tome)
                , succeed ()
                    |. chompUntilEndOr "\n"
                    |. chompWhile ((==) '\n')
                    |> getChompedString
                    |> andThen (addText tome)
                ]
    in
    loop defaultTome looper


parseTome : String -> Result (List DeadEnd) Tome
parseTome s =
    run betterParser s
