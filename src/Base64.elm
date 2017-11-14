module Base64 exposing (decode, encode, padAndDecode)

{-| Library for base64 encoding and decoding.

Decodes UTF-16 into UTF-8 for encoding to base64, and decodes UTF-8 into UTF-16
when decoding from base64.

    encode "👍"
    --> "8J+RjQ=="

    decode "8J+RjQ=="
    --> Ok "👍"


    padAndDecode "8J+RjQ"
    --> Ok "👍"

@docs encode, decode, padAndDecode

-}

import Base64.Decode as Internal
import Base64.Encode as Internal


{-| Encodes any Elm string into Base64.
-}
encode : String -> String
encode =
    Internal.encode


{-| Decodes Base64 into Elm strings.

This can result in an `Err "Invalid base64"` if the input is not valid base64.
If the resulting string cannot be converted to UTF-16, this will result in an
`Err "Invalid UTF-16"`.

-}
decode : String -> Result String String
decode =
    Internal.decode


{-| Decodes non padded Base64 into Elm strings.

Base64 strings might be encountered in non padded form where possible trailing
`'='` pad characters are stripped. For example JWT tokens contain non padded
format.

`padAndDecode` will first add missing `'='` padding and then use regular strict
`decode` function with same results.

-}
padAndDecode : String -> Result String String
padAndDecode =
    Internal.padAndDecode
