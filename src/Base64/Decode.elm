module Base64.Decode exposing (decode)

import Base64.Constants as C
import Bitwise as B
import Char
import Regex exposing (Regex, regex)


decode : String -> Result String String
decode input =
    input
        |> validate
        |> Result.andThen (String.foldl chomp initial >> wrapUp)
        |> Result.map (stripNulls input)


validate : String -> Result String String
validate input =
    if Regex.contains validBase64Regex input then
        Ok input
    else
        Err "Invalid base64"


validBase64Regex : Regex
validBase64Regex =
    regex "^([A-Za-z0-9\\/+]{4})*([A-Za-z0-9\\/+]{2}[A-Za-z0-9\\/+=]{2})?$"


stripNulls : String -> String -> String
stripNulls input output =
    if String.endsWith "==" input && String.endsWith "\x00\x00" output then
        String.dropRight 2 output
    else if String.endsWith "=" input && String.endsWith "\x00" output then
        String.dropRight 1 output
    else
        output


wrapUp : Accumulator -> Result String String
wrapUp ( _, _, ( _, need, res ) ) =
    if need > 0 then
        Err "Invalid UTF-16"
    else
        Ok res


type alias Accumulator =
    ( Int, Int, Utf8ToUtf16 )


type alias Base64ToUtf8 =
    ( Int, Int )


type alias Utf8ToUtf16 =
    ( Int, Int, String )


initial : Accumulator
initial =
    ( 0, 0, ( 0, 0, "" ) )


chomp : Char -> Accumulator -> Accumulator
chomp char_ ( curr, cnt, utf8ToUtf16 ) =
    let
        char : Int
        char =
            charToInt char_
    in
    case cnt of
        3 ->
            toUTF16 (B.or curr char) utf8ToUtf16

        _ ->
            ( B.or (B.shiftLeftBy ((3 - cnt) * 6) char) curr, cnt + 1, utf8ToUtf16 )


toUTF16 : Int -> Utf8ToUtf16 -> Accumulator
toUTF16 char acc =
    ( 0
    , 0
    , acc
        |> add (B.shiftRightZfBy 16 char |> B.and C.xff)
        |> add (B.shiftRightZfBy 8 char |> B.and C.xff)
        |> add (B.shiftRightZfBy 0 char |> B.and C.xff)
    )


add : Int -> Utf8ToUtf16 -> Utf8ToUtf16
add char ( curr, need, res ) =
    let
        shiftAndAdd : Int -> Int
        shiftAndAdd int =
            B.shiftLeftBy 6 curr
                |> B.or (B.and C.x3f int)
    in
    if need == 0 then
        if B.and C.x80 char == 0 then
            ( 0, 0, res ++ intToString char )
        else if B.and C.xe0 char == C.xc0 then
            ( B.and C.x1f char, 1, res )
        else if B.and C.xf0 char == C.xe0 then
            ( B.and C.xf char, 2, res )
        else
            ( B.and 7 char, 3, res )
    else if need == 1 then
        ( 0, 0, res ++ intToString (shiftAndAdd char) )
    else
        ( shiftAndAdd char, need - 1, res )


intToString : Int -> String
intToString int =
    if int <= C.x10000 then
        Char.fromCode int |> String.fromChar
    else
        let
            c =
                int - C.x10000
        in
        [ Char.fromCode (B.shiftRightZfBy 10 c |> B.or C.xd800)
        , Char.fromCode (B.and C.x3ff c |> B.or C.xdc00)
        ]
            |> String.fromList


charToInt : Char -> Int
charToInt char =
    case char of
        'A' ->
            0

        'B' ->
            1

        'C' ->
            2

        'D' ->
            3

        'E' ->
            4

        'F' ->
            5

        'G' ->
            6

        'H' ->
            7

        'I' ->
            8

        'J' ->
            9

        'K' ->
            10

        'L' ->
            11

        'M' ->
            12

        'N' ->
            13

        'O' ->
            14

        'P' ->
            15

        'Q' ->
            16

        'R' ->
            17

        'S' ->
            18

        'T' ->
            19

        'U' ->
            20

        'V' ->
            21

        'W' ->
            22

        'X' ->
            23

        'Y' ->
            24

        'Z' ->
            25

        'a' ->
            26

        'b' ->
            27

        'c' ->
            28

        'd' ->
            29

        'e' ->
            30

        'f' ->
            31

        'g' ->
            32

        'h' ->
            33

        'i' ->
            34

        'j' ->
            35

        'k' ->
            36

        'l' ->
            37

        'm' ->
            38

        'n' ->
            39

        'o' ->
            40

        'p' ->
            41

        'q' ->
            42

        'r' ->
            43

        's' ->
            44

        't' ->
            45

        'u' ->
            46

        'v' ->
            47

        'w' ->
            48

        'x' ->
            49

        'y' ->
            50

        'z' ->
            51

        '0' ->
            52

        '1' ->
            53

        '2' ->
            54

        '3' ->
            55

        '4' ->
            56

        '5' ->
            57

        '6' ->
            58

        '7' ->
            59

        '8' ->
            60

        '9' ->
            61

        '+' ->
            62

        '/' ->
            63

        _ ->
            0
