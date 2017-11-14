module Base64 exposing (decode, encode)

{-| Library for base64 encoding and decoding.

Decodes UTF-16 into UTF-8 for encoding to base64, and decodes UTF-8 into UTF-16
when decoding from base64.

    encode "👍"
    --> "8J+RjQ=="

    decode "8J+RjQ=="
    --> Ok "👍"


    padAndDecode "8J+RjQ"
    --> Ok "👍"

@docs encode, decode

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

Trailing `=` characters may be omitted.

-}
decode : String -> Result String String
decode =
    Internal.decode
