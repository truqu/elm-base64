module Base64 exposing (encode, decode)

{-| Library for base64 encoding and decoding.

@docs encode, decode

-}

import Base64.Decode as Internal
import Base64.Encode as Internal


{-| Encode any Elm string in Base64.

    encode "Hello world!"
    --> "SGVsbG8gd29ybGQh"

The encoding process appends `=` or `==` so that the resulting string's length
is always a multiple of 4. This is sometimes referred to as _padded encoding_.

    encode "Hi"
    --> "SGk="

During encoding, strings are converted to UTF-8. The encoder correctly handles
any valid Elm string, including characters outside the BMP, like Emoji.

    encode "👍"
    --> "8J+RjQ=="

-}
encode : String -> String
encode =
    Internal.encode


{-| Decode Base64-encoded strings into Elm strings.

    decode "SGVsbG8gd29ybGQh"
    --> Ok "Hello world!"

When decoding fails, an `Err` is returned to explain what went wrong. Currently,
this may happen under two separate circumstances: either the input is not a
valid Base64 string, or it contains invalid character sequences that cannot be
converted to an UTF-16 string.

    decode "What is this"
    --> Err "Invalid base64"

    decode "/Ng9"
    --> Err "Invalid UTF-16"

Decoding assumes that the Base64 was encoded from an UTF-8 string. Note that
this library converts the input to UTF-8 when encoding, so decoding should
always succeed with strings generated by `encode`. Trailing `=` characters
may be omitted.

    decode "8J+RjQ"
    --> Ok "👍"

    decode "8J+RjQ=="
    --> Ok "👍"

-}
decode : String -> Result String String
decode =
    Internal.decode
