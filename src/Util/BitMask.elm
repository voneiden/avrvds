module Util.BitMask exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Parser exposing ((|.), (|=), Parser, chompWhile, end, getChompedString, run, succeed, symbol)
import Util.ParserUtil exposing (parserDeadEndsToString)


type Hex
    = Hex String


parseHex : Parser Hex
parseHex =
    succeed (\c -> Hex c)
        |. symbol "0x"
        |= (getChompedString <| chompWhile isHex)
        |. end


decodeHex : String -> Decoder Hex
decodeHex s =
    case run parseHex s of
        Ok hex ->
            Decode.succeed hex

        Err error ->
            Decode.fail <| "Bad bitmask: " ++ parserDeadEndsToString error


type Bits
    = Bits (List Int)


decodeBits : Hex -> Decoder Bits
decodeBits hex =
    Decode.succeed <| hexToBits hex


decodeBitMask : Bits -> Decoder BitMask
decodeBitMask bits =
    case bitRange bits of
        Just bitmask ->
            Decode.succeed <| bitmask

        Nothing ->
            Decode.fail <| "Failed to determine bitmask range"


indexOfHighBitHelper : List Int -> Int -> Maybe Int
indexOfHighBitHelper list_ offset =
    case List.take 1 list_ of
        [ x ] ->
            case x of
                1 ->
                    Just offset

                _ ->
                    indexOfHighBitHelper (List.drop 1 list_) (offset + 1)

        _ ->
            Nothing


type BitMask
    = BitIndex Int
    | BitRange Int Int


maskByte : BitMask -> Int
maskByte mask =
    case mask of
        BitIndex index ->
            index // 8
        BitRange _ low ->
            low // 8


bitRange : Bits -> Maybe BitMask
bitRange (Bits list) =
    let
        length = List.length list - 1
        leftIndex =
            indexOfHighBitHelper list 0
                |> Maybe.andThen (\x -> Just <| length - x)
        rightIndex =
            indexOfHighBitHelper (List.reverse list) 0

    in
    case ( leftIndex, rightIndex ) of
        ( Just left, Just right ) ->
            case left == right of
                True ->
                    Just (BitIndex left)

                False ->
                    Just (BitRange left right)

        _ ->
            Nothing

bitLength : BitMask -> Int
bitLength bits  =
    case bits of
        BitIndex _ -> 1
        BitRange high low ->
            high - low + 1

isHex : Char -> Bool
isHex c =
    not <| List.isEmpty <| hexToBitMap c


hexToBitMap : Char -> List Int
hexToBitMap c =
    case Char.toLower c of
        '0' ->
            [ 0, 0, 0, 0 ]

        '1' ->
            [ 0, 0, 0, 1 ]

        '2' ->
            [ 0, 0, 1, 0 ]

        '3' ->
            [ 0, 0, 1, 1 ]

        '4' ->
            [ 0, 1, 0, 0 ]

        '5' ->
            [ 0, 1, 0, 1 ]

        '6' ->
            [ 0, 1, 1, 0 ]

        '7' ->
            [ 0, 1, 1, 1 ]

        '8' ->
            [ 1, 0, 0, 0 ]

        '9' ->
            [ 1, 0, 0, 1 ]

        'a' ->
            [ 1, 0, 1, 0 ]

        'b' ->
            [ 1, 0, 1, 1 ]

        'c' ->
            [ 1, 1, 0, 0 ]

        'd' ->
            [ 1, 1, 0, 1 ]

        'e' ->
            [ 1, 1, 1, 0 ]

        'f' ->
            [ 1, 1, 1, 1 ]

        _ ->
            []


hexToBits : Hex -> Bits
hexToBits (Hex hex) =
    String.toList hex
        |> List.concatMap hexToBitMap
        |> Bits
