module Ascii (fromInt, toInt, isValid, isAsciiChar) where

import Array exposing (Array)
import String
import Dict exposing (Dict)
import List
import Maybe
import Result
import Result exposing (fromMaybe)

asciiCharsArray : Array Char
asciiCharsArray =
  Array.fromList asciiCharsList

asciiCharPairList : List (Char,Int)
asciiCharPairList =
  List.map2 (,) asciiCharsList [32..126]

insertPairIntoDict : (comparable , v) -> Dict comparable v -> Dict comparable v
insertPairIntoDict (key , value) dict =
  Dict.insert key value dict

asciiCharsMap : Dict Char Int
asciiCharsMap =
  List.foldl insertPairIntoDict Dict.empty asciiCharPairList

isAsciiChar : Char -> Bool
isAsciiChar char =
  Dict.member char asciiCharsMap

fromInt : Int -> Result String Char
fromInt index =
  fromMaybe "integer has no corresponding ascii char" (Array.get (index-32) asciiCharsArray)

toInt : Char -> Result String Int
toInt char =
  fromMaybe "char is not a supported ascii character" (Dict.get char asciiCharsMap)

isValid : String -> Bool
isValid string =
  List.all isAsciiChar (String.toList string)

asciiCharsList : List Char
asciiCharsList =
  String.toList """ !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"""
