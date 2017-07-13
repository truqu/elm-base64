module Base64.Encode exposing (encode)

import Base64.Constants as C
import Bitwise as B
import Char


encode : String -> String
encode input =
    String.foldl chomp initial input |> wrapUp


type alias Accumulator =
    ( UTF8Accumulator, UTF8ToBase64Accumulator )


initial : Accumulator
initial =
    ( Nothing, ( "", 0, 0 ) )


notZero : Int -> Int
notZero i =
    if i == 0 then
        -1
    else
        i


wrapUp : Accumulator -> String
wrapUp ( _, ( res, cnt, acc ) ) =
    case cnt of
        1 ->
            res
                ++ (B.shiftRightZfBy 2 acc |> B.and C.x3f |> intToBase64)
                ++ (B.shiftLeftBy 4 acc |> B.and C.x3f |> intToBase64)
                ++ "=="

        2 ->
            res
                ++ (B.shiftRightZfBy 10 acc |> B.and C.x3f |> intToBase64)
                ++ (B.shiftRightZfBy 4 acc |> B.and C.x3f |> intToBase64)
                ++ (B.shiftLeftBy 2 acc |> B.and C.x3f |> intToBase64)
                ++ "="

        _ ->
            res


type alias UTF8Accumulator =
    Maybe Int


type alias UTF8ToBase64Accumulator =
    ( String, Int, Int )


chomp : Char -> Accumulator -> Accumulator
chomp char_ ( utf8Acc, base64Acc ) =
    let
        char : Int
        char =
            Char.toCode char_
    in
    case utf8Acc of
        Nothing ->
            if char < C.x80 then
                ( Nothing
                , base64Acc
                    |> add char
                )
            else if char < C.x800 then
                ( Nothing
                , base64Acc
                    |> add (B.or C.xc0 (B.shiftRightZfBy 6 char))
                    |> add (B.or C.x80 (B.and C.x3f char))
                )
            else if char < C.xd800 || char >= C.xe000 then
                ( Nothing
                , base64Acc
                    |> add (B.or C.xe0 (B.shiftRightZfBy 12 char))
                    |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 6 char)))
                    |> add (B.or C.x80 (B.and C.x3f char))
                )
            else
                ( Just char, base64Acc )

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
            ( Nothing
            , base64Acc
                |> add (B.or C.xf0 (B.shiftRightZfBy 18 combined))
                |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 12 combined)))
                |> add (B.or C.x80 (B.and C.x3f (B.shiftRightZfBy 6 combined)))
                |> add (B.or C.x80 (B.and C.x3f combined))
            )


add : Int -> UTF8ToBase64Accumulator -> UTF8ToBase64Accumulator
add char ( res, count, acc ) =
    let
        current =
            B.or (B.shiftLeftBy 8 acc) char
    in
    case count of
        2 ->
            ( res ++ toBase64 current, 0, 0 )

        _ ->
            ( res, count + 1, current )


toBase64 : Int -> String
toBase64 int =
    (B.shiftRightZfBy 18 int |> B.and C.x3f |> intToBase64)
        ++ (B.shiftRightZfBy 12 int |> B.and C.x3f |> intToBase64)
        ++ (B.shiftRightZfBy 6 int |> B.and C.x3f |> intToBase64)
        ++ (B.shiftRightZfBy 0 int |> B.and C.x3f |> intToBase64)


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
