module Data.Address exposing (Model, decode)

import Data.Country as Country
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type Model
    = Model Address


type alias Address =
    { address : String
    , city : String
    , zip : String
    , country : Country.Model
    }


decode : Decode.Decoder Model
decode =
    Decode.map Model decodeAddress


decodeAddress : Decode.Decoder Address
decodeAddress =
    Decode.succeed Address
        |> Pipeline.required "address" Decode.string
        |> Pipeline.required "city" Decode.string
        |> Pipeline.required "zip" Decode.string
        |> Pipeline.required "country" Country.decode
