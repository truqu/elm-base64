module Base64.Encode exposing (encode)

import Base64.Constants as C
import Bitwise as B
import Char


encode : String -> String
encode input =
    stringToBytes input |> toBase64


toBase64 : List Int -> String
toBase64 ints =
    let
        takeSixAndShift : Int -> ( Int, Int )
        takeSixAndShift input =
            ( B.and C.x3f input, B.shiftRightZfBy 6 input )

        toQuad : Int -> String
        toQuad int =
            (B.shiftRightZfBy 18 int |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 12 int |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 6 int |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 0 int |> B.and C.x3f |> intToBase64)

        notZero : Int -> Int
        notZero i =
            if i == 0 then
                -1
            else
                i

        toLastQuad : Int -> String
        toLastQuad int =
            (B.shiftRightZfBy 18 int |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 12 int |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 6 int |> B.and C.x3f |> notZero |> intToBase64)
                ++ (B.shiftRightZfBy 0 int |> B.and C.x3f |> notZero |> intToBase64)
    in
    case ints of
        [] ->
            ""

        x :: xs ->
            List.foldl (\int string -> toQuad int ++ string) (toLastQuad x) xs


stringToBytes : String -> List Int
stringToBytes input =
    let
        addChar : Int -> Accumulator -> Accumulator
        addChar char (( a, b, c, combine ) as acc) =
            case combine of
                Nothing ->
                    if char < C.x80 then
                        add char acc
                    else if char < C.x800 then
                        acc
                            |> add (B.or C.xc0 (B.shiftRightZfBy 6 char))
                            |> add (B.or C.x80 (B.and C.x3f char))
                    else if char < C.xd800 || char >= C.xe000 then
                        acc
                            |> add (B.or C.xe0 (B.shiftRightZfBy 12 char))
                            |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 6 char)))
                            |> add (B.or C.x80 (B.and C.x3f char))
                    else
                        ( a, b, c, Just char )

                Just prev ->
                    let
                        combined : Int
                        combined =
                            prev
                                |> B.and C.x3ff
                                |> B.shiftLeftBy 10
                                |> B.or (B.and C.x3ff char)
                                |> (+) C.x10000
                    in
                    markCombined acc
                        |> add (B.or C.xf0 (B.shiftRightZfBy 18 combined))
                        |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 12 combined)))
                        |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 6 combined)))
                        |> add (B.or C.x80 (B.and C.x3f combined))

        wrapUp : Accumulator -> List Int
        wrapUp ( agg, current, count, _ ) =
            case count of
                0 ->
                    agg

                1 ->
                    B.shiftLeftBy 16 current :: agg

                _ ->
                    B.shiftLeftBy 8 current :: agg
    in
    String.foldl
        (\c acc -> addChar (Char.toCode c) acc)
        initialAcc
        input
        |> wrapUp


type alias Accumulator =
    ( List Int, Int, Int, Maybe Int )


initialAcc : Accumulator
initialAcc =
    ( [], 0, 0, Nothing )


add : Int -> Accumulator -> Accumulator
add n ( agg, current, count, combine ) =
    case count of
        2 ->
            ( B.or (B.shiftLeftBy 8 current) n :: agg, 0, 0, Nothing )

        _ ->
            ( agg, B.or (B.shiftLeftBy 8 current) n, count + 1, Nothing )


markCombined : Accumulator -> Accumulator
markCombined ( a, b, c, d ) =
    ( a, b, c, Nothing )


intToBase64 : Int -> String
intToBase64 i =
    case i of
        0 ->
            "A"

        1 ->
            "B"

        2 ->
            "C"

        3 ->
            "D"

        4 ->
            "E"

        5 ->
            "F"

        6 ->
            "G"

        7 ->
            "H"

        8 ->
            "I"

        9 ->
            "J"

        10 ->
            "K"

        11 ->
            "L"

        12 ->
            "M"

        13 ->
            "N"

        14 ->
            "O"

        15 ->
            "P"

        16 ->
            "Q"

        17 ->
            "R"

        18 ->
            "S"

        19 ->
            "T"

        20 ->
            "U"

        21 ->
            "V"

        22 ->
            "W"

        23 ->
            "X"

        24 ->
            "Y"

        25 ->
            "Z"

        26 ->
            "a"

        27 ->
            "b"

        28 ->
            "c"

        29 ->
            "d"

        30 ->
            "e"

        31 ->
            "f"

        32 ->
            "g"

        33 ->
            "h"

        34 ->
            "i"

        35 ->
            "j"

        36 ->
            "k"

        37 ->
            "l"

        38 ->
            "m"

        39 ->
            "n"

        40 ->
            "o"

        41 ->
            "p"

        42 ->
            "q"

        43 ->
            "r"

        44 ->
            "s"

        45 ->
            "t"

        46 ->
            "u"

        47 ->
            "v"

        48 ->
            "w"

        49 ->
            "x"

        50 ->
            "y"

        51 ->
            "z"

        52 ->
            "0"

        53 ->
            "1"

        54 ->
            "2"

        55 ->
            "3"

        56 ->
            "4"

        57 ->
            "5"

        58 ->
            "6"

        59 ->
            "7"

        60 ->
            "8"

        61 ->
            "9"

        62 ->
            "+"

        63 ->
            "/"

        _ ->
            "="
