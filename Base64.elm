module Base64 (encode, decode) where

import List
import List exposing (append)
import BitList
import BitList exposing (Bit)
import Array
import String
import Ascii
import Result
import Dict exposing (Dict)


encode : String -> Result String String
encode s =
  if not (Ascii.isValid s)
  then Result.Err "Error while encoding"
  else Result.Ok (toAsciiList s |> toTupleList |> toCharList |> String.fromList)

decode : String -> Result String String
decode s =
  if not (isValid s)
  then
    Result.Err "Error while decoding"
  else
    let bitList = List.map BitList.toByte (toBase64BitList s |> BitList.partition 8)
        charList = resultUnfold <| List.map Ascii.fromInt bitList
    in
      Result.Ok <| String.fromList charList

toAsciiList : String -> List Int
toAsciiList string =
  let toInt char = case Ascii.toInt char of
                     Result.Ok(value) -> value
                     _                -> -1
  in
    List.map toInt (String.toList string)

toTupleList : List Int -> List (Int, Int, Int)
toTupleList list =
  case list of
    a :: b :: c :: l -> (a, b, c) :: toTupleList(l)
    a :: b :: []     -> [(a, b , -1)]
    a :: []          -> [(a, -1, -1)]
    []               -> []
    _                -> [(-1, -1, -1)]

toCharList : List (Int,Int,Int) -> List Char
toCharList bitList =
  let toChars (a, b, c) =
        case (a,b,c) of
          (a, -1, -1) -> (dropLast 2 (List.map toBase64Char (partitionBits [a,0,0]))) `append` ['=','=']
          (a, b, -1)  -> (dropLast 1 (List.map toBase64Char (partitionBits [a,b,0]))) `append` ['=']
          (a, b, c)   -> (List.map toBase64Char (partitionBits [a,b,c]))
  in
    List.concatMap toChars bitList

base64CharsList : List Char
base64CharsList =
  String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64CharsArray =
  Array.fromList base64CharsList

base64Map : Dict Char Int
base64Map =
  List.foldl insertPairIntoDict Dict.empty (List.indexedMap (,) base64CharsList)

isValid : String -> Bool
isValid string =
  let isBase64Char char = Dict.member char base64Map
  in
    String.all isBase64Char
            <| if | String.endsWith "==" string -> String.dropRight 2 string
                  | String.endsWith "=" string  -> String.dropRight 1 string
                  | otherwise                   -> string

insertPairIntoDict : (v , comparable) -> Dict comparable v -> Dict comparable v
insertPairIntoDict (value , key) dict =
  Dict.insert key value dict

toBase64Char : Int -> Char
toBase64Char index =
  Maybe.withDefault '!' (Array.get index base64CharsArray)

fromBase64Char : Char -> Int
fromBase64Char char =
  Maybe.withDefault 0 (Dict.get char base64Map)

toBitList : List Int -> List(Bit)
toBitList list =
  List.foldr List.append [] (List.map BitList.fromByte list)

partitionBits : List Int -> List Int
partitionBits list =
  List.map BitList.toByte (BitList.partition 6 (toBitList list))

dropLast : Int -> List a -> List a
dropLast number list =
  List.reverse list |> List.drop number |> List.reverse

toBase64BitList : String -> List(Bit)
toBase64BitList string =
  let endingEquals = if | (String.endsWith "==" string) -> 2
                        | (String.endsWith "=" string)  -> 1
                        | otherwise                     -> 0
      stripped = String.toList (String.dropRight endingEquals string)
      numberList = List.map base64ToInt stripped
  in
    dropLast (endingEquals*2) <| List.concatMap (flip BitList.fromNumberWithSize <| 6) numberList

base64ToInt : Char -> Int
base64ToInt char =
  case Dict.get char base64Map of
    Just(value) -> value
    _           -> -1

resultUnfold : List(Result a b) -> List b
resultUnfold list =
  case list of
    []                      -> []
    Result.Ok(head) :: tail -> head :: resultUnfold(tail)
    Result.Err(err) :: tail -> []
