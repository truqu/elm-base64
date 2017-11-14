module Tests exposing (..)

import Base64 exposing (decode, encode, padAndDecode)
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


nonPaddedCases : List ( String, String )
nonPaddedCases =
    [ ( "", "" )
    , ( "f", "Zg" )
    , ( "fo", "Zm8" )
    , ( "foo", "Zm9v" )
    , ( "foob", "Zm9vYg" )
    , ( "foob", "Zm9vYg==" )
    , ( "fooba", "Zm9vYmE" )
    , ( "foobar", "Zm9vYmFy" )
    , ( "\n", "Cg" )
    , ( "\n", "Cg==" )
    , ( "✓ à la mode", "4pyTIMOgIGxhIG1vZGU" )
    , ( "✓ à la mode", "4pyTIMOgIGxhIG1vZGU=" )
    , ( "💩", "8J+SqQ" )
    , ( "💩", "8J+SqQ==" )
    , ( "💩💩💩", "8J+SqfCfkqnwn5Kp" )
    , ( "Man", "TWFu" )
    , ( String.repeat 500 "Man", String.repeat 500 "TWFu" )
    , ( String.repeat 5000 "Man", String.repeat 5000 "TWFu" )
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


padAndDecodeTests : Test
padAndDecodeTests =
    nonPaddedCases
        |> List.map
            (\( output, input ) ->
                test ("Can decode '" ++ input ++ "'") <|
                    \_ ->
                        padAndDecode input
                            |> Expect.equal (Ok output)
            )
        |> describe "decode non-padded base64"


roundTrip : Test
roundTrip =
    fuzz string "Roundtrip" <|
        \input ->
            encode input
                |> decode
                |> Expect.equal (Ok input)


badInputTests : Test
badInputTests =
    [ "foo"
    , "abc"
    , "a=aa"
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
