module Base64.Decode exposing (decode)

import Base64.Constants as C
import Bitwise as B
import Char
import Regex exposing (Regex, regex)


decode : String -> Result String String
decode input =
    input
        |> validate
        |> Result.andThen (decodeTriplets >> tripletsToString)
        |> Result.map (stripNulls input)


validBase64Regex : Regex
validBase64Regex =
    regex "^([A-Za-z0-9\\/+]{4})*([A-Za-z0-9\\/+]{2}[A-Za-z0-9\\/+=]{2})?$"


validate : String -> Result String String
validate input =
    if Regex.contains validBase64Regex input then
        Ok input
    else
        Err "Invalid base64"


stripNulls : String -> String -> String
stripNulls input output =
    if String.endsWith "==" input && String.endsWith "\x00\x00" output then
        String.dropRight 2 output
    else if String.endsWith "=" input && String.endsWith "\x00" output then
        String.dropRight 1 output
    else
        output


decodeTriplets : String -> List Int
decodeTriplets input =
    let
        consumeChar : Char -> ( List Int, Int, Int ) -> ( List Int, Int, Int )
        consumeChar char ( chars, cur, cnt ) =
            let
                c =
                    charToInt char
            in
            case cnt of
                3 ->
                    ( B.or cur c :: chars, 0, 0 )

                _ ->
                    ( chars, B.or (B.shiftLeftBy ((3 - cnt) * 6) c) cur, cnt + 1 )

        wrapUp : ( List Int, Int, Int ) -> List Int
        wrapUp ( chars, _, _ ) =
            chars
    in
    String.toList input
        |> List.foldl consumeChar ( [], 0, 0 )
        |> wrapUp


tripletsToString : List Int -> Result String String
tripletsToString ints =
    let
        decodeInt : Int -> Accumulator -> Accumulator
        decodeInt int acc =
            acc
                |> add (B.shiftRightZfBy 16 int |> B.and C.xff)
                |> add (B.shiftRightZfBy 8 int |> B.and C.xff)
                |> add (B.shiftRightZfBy 0 int |> B.and C.xff)
                |> flush
    in
    List.foldr decodeInt initialAcc ints
        |> finish


take8AndShift : Int -> ( Int, Int )
take8AndShift input =
    ( B.and C.xff input, B.shiftRightZfBy 8 input )


type alias Accumulator =
    { current : Int
    , needs : Int
    , has : Int
    , chars : List Char
    , result : String
    }


writeChars : Accumulator -> Accumulator
writeChars ({ current } as acc) =
    if current <= C.x10000 then
        { acc
            | chars = Char.fromCode current :: acc.chars
            , current = 0
            , needs = 0
            , has = 0
        }
    else
        let
            c =
                current - C.x10000
        in
        { acc
            | chars =
                Char.fromCode (B.and C.x3ff c |> B.or C.xdc00)
                    :: Char.fromCode (B.shiftRightZfBy 10 c |> B.or C.xd800)
                    :: acc.chars
            , current = 0
            , needs = 0
            , has = 0
        }


add : Int -> Accumulator -> Accumulator
add int ({ has } as acc) =
    let
        shiftAndAdd : Int -> Int
        shiftAndAdd int =
            B.shiftLeftBy 6 acc.current
                |> B.or (B.and C.x3f int)

        writeCharsIfNeeded : Accumulator -> Accumulator
        writeCharsIfNeeded =
            if acc.has == (acc.needs - 1) then
                writeChars
            else
                identity
    in
    case acc.needs of
        0 ->
            if B.and C.x80 int == 0 then
                { acc | chars = Char.fromCode int :: acc.chars }
            else if B.and C.xe0 int == C.xc0 then
                { acc | needs = 2, has = 1, current = B.and C.x1f int }
            else if B.and C.xf0 int == C.xe0 then
                { acc | needs = 3, has = 1, current = B.and C.xf int }
            else
                { acc | needs = 4, has = 1, current = B.and 7 int }

        _ ->
            { acc
                | current = shiftAndAdd int
                , has = acc.has + 1
            }
                |> writeCharsIfNeeded


flush : Accumulator -> Accumulator
flush acc =
    { acc
        | chars = []
        , result = acc.result ++ String.fromList (List.reverse acc.chars)
    }


finish : Accumulator -> Result String String
finish acc =
    if acc.needs /= 0 then
        Err "Invalid UTF-16"
    else
        flush acc |> .result |> Ok


initialAcc : Accumulator
initialAcc =
    { current = 0
    , needs = 0
    , has = 0
    , chars = []
    , result = ""
    }


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
