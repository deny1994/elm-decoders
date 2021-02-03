module Data.OneOrMore exposing (Model, decode, toModel)

import Json.Decode as Decode


type Model a
    = Model (Data a)


type alias Data a =
    ( a, List a )


toModel : Data a -> Model a
toModel =
    Model


decode : Decode.Decoder a -> Decode.Decoder (Model a)
decode =
    Decode.oneOrMore Tuple.pair
        >> Decode.map toModel
