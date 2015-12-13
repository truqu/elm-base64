module Tests where

import ElmTest exposing (Test, suite)
import Test.Ascii
import Test.Base64
import Test.BitList


all : Test
all =
  suite "Main" [ Test.Ascii.tests
               , Test.Base64.tests
               , Test.BitList.tests
               ]
