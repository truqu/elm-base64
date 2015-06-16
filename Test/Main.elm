module Main where

import Test.AsciiTest as AsciiTest
import Test.Base64Test as Base64Test
import Test.BitListTest as BitListTest
import Test.Base64Check as Base64Check
import IO.IO exposing (seq)
import IO.Runner exposing (Request, Response, run)
import ElmTest.Test exposing (Test,suite)
import ElmTest.Runner.Console exposing (runDisplay)
import Test.Console as Console
import Check exposing (quickCheck)
import Check

tests : Test
tests = suite "Main" [
  BitListTest.tests,
  Base64Test.tests,
  AsciiTest.tests
  ]

allChecks : Check.Claim
allChecks =
  Check.suite "Main"
    [ Base64Check.checks ]

port requests : Signal Request
port requests =
  let
    testResponse = runDisplay tests
    checkResponse = Console.runDisplay (quickCheck allChecks)
  in
    run responses (seq testResponse checkResponse)

port responses : Signal Response
