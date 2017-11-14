module Tests exposing (..)

import Base64 exposing (decode, encode)
import Expect exposing (Expectation)
import Fuzz exposing (string)
import Test exposing (..)


cases : List ( String, String )
cases =
    [ ( "", "" )
    , ( "f", "Zg==" )
    , ( "fo", "Zm8=" )
    , ( "foo", "Zm9v" )
    , ( "foob", "Zm9vYg==" )
    , ( "fooba", "Zm9vYmE=" )
    , ( "foobar", "Zm9vYmFy" )
    , ( "\n", "Cg==" )
    , ( "✓ à la mode", "4pyTIMOgIGxhIG1vZGU=" )
    , ( "💩", "8J+SqQ==" )
    , ( "💩💩💩", "8J+SqfCfkqnwn5Kp" )
    , ( "Man", "TWFu" )
    , ( String.repeat 500 "Man", String.repeat 500 "TWFu" )
    , ( String.repeat 5000 "Man", String.repeat 5000 "TWFu" )
    ]


unpadded : List ( String, String )
unpadded =
    [ ( "f", "Zg" )
    , ( "fo", "Zm8" )
    , ( "foob", "Zm9vYg" )
    , ( "fooba", "Zm9vYmE" )
    , ( "\n", "Cg" )
    , ( "✓ à la mode", "4pyTIMOgIGxhIG1vZGU" )
    , ( "💩", "8J+SqQ" )
    ]


encodeTests : Test
encodeTests =
    cases
        |> List.map
            (\( input, output ) ->
                test ("Can encode '" ++ input ++ "'") <|
                    \_ ->
                        encode input
                            |> Expect.equal output
            )
        |> describe "encode basics"


decodeTests : Test
decodeTests =
    cases
        |> List.map
            (\( output, input ) ->
                test ("Can decode '" ++ input ++ "'") <|
                    \_ ->
                        decode input
                            |> Expect.equal (Ok output)
            )
        |> describe "decode basics"


unpaddedDecodeTests : Test
unpaddedDecodeTests =
    unpadded
        |> List.map
            (\( output, input ) ->
                test ("Can decode '" ++ input ++ "'") <|
                    \_ ->
                        decode input
                            |> Expect.equal (Ok output)
            )
        |> describe "decode unpadded"


roundTrip : Test
roundTrip =
    fuzz string "Roundtrip" <|
        \input ->
            encode input
                |> decode
                |> Expect.equal (Ok input)


roundTripUnpadded : Test
roundTripUnpadded =
    fuzz string "Roundtrip with trailing = omitted" <|
        \input ->
            encode input
                |> String.filter ((/=) '=')
                |> decode
                |> Expect.equal (Ok input)


badInputTests : Test
badInputTests =
    [ "a=aa"
    , "a==="
    ]
        |> List.map
            (\input ->
                test ("Decode fails for " ++ input) <|
                    \_ ->
                        decode input
                            |> Expect.equal (Err "Invalid base64")
            )
        |> describe "bad input"


badUTF16Error : Test
badUTF16Error =
    test "Cannot decode invalid UTF-16" <|
        \_ ->
            decode "/Ng9"
                |> Expect.equal (Err "Invalid UTF-16")
