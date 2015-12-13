module Test.Ascii (tests) where

import Ascii
import ElmTest exposing (Test, assertEqual, defaultTest, suite)


fromIntTest : (Char, Int) -> Test
fromIntTest (char,int) =
  defaultTest (assertEqual (Ascii.fromInt int) (Result.Ok char))


toIntTest : (Char, Int) -> Test
toIntTest (char,int) =
  defaultTest (assertEqual (Ascii.toInt char) (Result.Ok int))


examples =
  [ (' ', 32)
  , ('.', 46)
  , (']', 93)
  , ('a', 97)
  , ('~', 126)
  ]


tests : Test
tests =
  suite "Ascii" [ suite "toInt" <| List.map toIntTest examples
                , suite "fromInt" <| List.map fromIntTest examples
                ]
