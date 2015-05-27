module Base64 (encode, decode) where

import List
import List exposing (append)
import BitList
import BitList exposing (Bit)
import Array
import String
import Ascii
import Maybe
import Maybe exposing (andThen)
import Result
import Dict exposing (Dict)

base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
base64CharsList = String.toList base64Chars
base64CharsArray = Array.fromList base64CharsList
base64Map : Dict Char Int
base64Map = List.foldl insertPairIntoDict Dict.empty (List.indexedMap (,) base64CharsList)

isBase64Char : Char -> Bool
isBase64Char char = Dict.member char base64Map

isValid : String -> Bool
isValid string =
  let
    stripped = if | (String.endsWith "==" string) -> String.dropRight 2 string
                  | (String.endsWith "=" string) -> String.dropRight 1 string
                  | otherwise -> string
  in
    String.all isBase64Char stripped

insertPairIntoDict : (v , comparable) -> Dict comparable v -> Dict comparable v
insertPairIntoDict (value , key) dict = Dict.insert key value dict


toBase64Char : Int -> Char
toBase64Char index = Maybe.withDefault '!' (Array.get index base64CharsArray)

fromBase64Char : Char -> Int
fromBase64Char char = Maybe.withDefault 0 (Dict.get char base64Map)

toBitList : List Int -> List(Bit)
toBitList list = List.foldr List.append [] (List.map BitList.fromByte list)

partitionBits : List Int -> List Int
partitionBits list = List.map BitList.toByte (BitList.partition 6 (toBitList list))

toChars : (Int,Int,Int) -> List Char
toChars (a,b,c) = case (a,b,c) of
  (a, -1, -1) -> (dropLast 2 (List.map toBase64Char (partitionBits [a,0,0]))) `append` ['=','=']
  (a, b, -1) -> (dropLast 1 (List.map toBase64Char (partitionBits [a,b,0]))) `append` ['=']
  (a, b, c) -> (List.map toBase64Char (partitionBits [a,b,c]))

dropLast : Int -> List a -> List a
dropLast number list = List.reverse list |> List.drop number |> List.reverse

toCharList : List (Int,Int,Int) -> List Char
toCharList bitList = List.concatMap toChars bitList

toTupleList : List Int -> List (Int, Int, Int)
toTupleList list = case list of
  a :: b :: c :: l -> (a, b, c) :: toTupleList(l)
  a :: b :: [] -> [(a, b , -1)]
  a :: [] -> [(a, -1, -1)]
  [] -> []
  _ -> [(-1, -1, -1)]

toAsciiList : String -> List(Int)
toAsciiList string = List.map (asciiToInt) (String.toList string)

toBase64BitList : String -> List(Bit)
toBase64BitList string =
  let
    endingEquals = if | (String.endsWith "==" string) -> 2
                  | (String.endsWith "=" string) -> 1
                  | otherwise -> 0
    stripped = String.toList (String.dropRight endingEquals string)
    numberList = List.map base64ToInt stripped
  in
    dropLast (endingEquals*2)(List.concatMap (BitList.fromNumberWithLength 6) numberList)

base64ToInt : Char -> Int
base64ToInt char = case Dict.get char base64Map of
  Just(value) -> value
  _ -> -1

asciiToInt : Char -> Int
asciiToInt char = case Ascii.toInt char of
  Result.Ok(value) -> value
  _ -> -1

encode : String -> Result String String
encode s =
  if not(Ascii.isValid(s))
  then
    Result.Err "Error while encoding"
  else
    Result.Ok (toAsciiList s |> toTupleList |> toCharList |> String.fromList)

resultUnfold : List(Result a b) -> List b
resultUnfold list = case list of
  [] -> []
  Result.Ok(head) :: tail -> head::resultUnfold(tail)
  Result.Err(err) :: tail -> []

decode : String -> Result String String
decode s =
  if not (isValid(s))
  then
    Result.Err "Error while decoding"
  else
--    Result.Ok s
    let
       bitList : List(Int)
       bitList = List.map BitList.toByte (toBase64BitList s |> BitList.partition 8)
       charList : List(Char)
       charList = resultUnfold(List.map Ascii.fromInt bitList)
    in
       Result.Ok(String.fromList charList)
