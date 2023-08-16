module JsonWebTokenTests exposing (encodeWithHeadersTests, verifyTests)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonWebToken exposing (DecodeError(..), decode)
import Test exposing (Test, describe, test)
import TestHelpers
    exposing
        ( aValidToken
        , correctSecret
        , payload
        , payloadDecoder
        )


verifyTests : Test
verifyTests =
    describe "JsonWebToken.verify"
        [ test "verify token with too few parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode payloadDecoder correctSecret "foo.bar")
        , test "verify token with too many parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode payloadDecoder correctSecret "foo.bar.car.far")
        , test "verify token with valid secret" <|
            \_ ->
                Expect.equal
                    (Ok payload)
                    (decode payloadDecoder correctSecret aValidToken)
        , test "decode a token without a typ in the header" <|
            \_ ->
                let
                    {-

                       - "alg":"HS256"
                       - "payload"
                       - signed with "secret"

                    -}
                    tokenWithoutTyp : String
                    tokenWithoutTyp =
                        "eyJhbGciOiJIUzI1NiJ9.InBheWxvYWQi.xZ3HN7F1t9dBMbKCXa9pye1VW6wC2A7V93Pva5jpkpI="
                in
                Expect.equal
                    (Ok "payload")
                    (decode Decode.string "secret" tokenWithoutTyp)
        ]


encodeWithHeadersTests : Test
encodeWithHeadersTests =
    let
        token : JsonWebToken.Token
        token =
            JsonWebToken.encodeWithHeaders
                JsonWebToken.hmacSha256
                (\{ id } -> [ ( "kid", Encode.string id ) ])
                (\{ expires } -> Encode.object [ ( "exp", Encode.int expires ) ])
                "secret"
                { id = "ID", expires = 3600 }

        {-
           { headers:
               { alg: "HS256"
               , typ: "JWT"
               , kid: "ID"
               }
           , body:
               { exp: 3600
               }
           }

        -}
        validToken : String
        validToken =
            "eyJraWQiOiJJRCIsImFsZyI6IkhTMjU2IiwidHlwIjoiSldUIn0=.eyJleHAiOjM2MDB9.WbWCn7284xbgAwtW4l5oqstnuexStKk_1tIuYMdwyRw="
    in
    describe "JsonWebToken.encodeWithHeaders"
        [ test "stores custom header in addition to the default ones (alg + typ)" <|
            \_ ->
                Expect.equal token validToken
        , test "can still be decoded using the provided functions" <|
            \_ ->
                Expect.equal
                    (Ok 3600)
                    (JsonWebToken.decode (Decode.field "exp" Decode.int) "secret" token)
        ]
