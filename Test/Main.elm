module Main where

import Test.AsciiTest as AsciiTest
import Test.Base64Test as Base64Test
import Test.BitListTest as BitListTest
import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (Test,suite)
import ElmTest.Runner.Console exposing (runDisplay)

tests : Test
tests = suite "Main" [
  BitListTest.tests,
  Base64Test.tests,
  AsciiTest.tests
  ]

port requests : Signal Request
port requests = run responses (runDisplay tests)

port responses : Signal Response
