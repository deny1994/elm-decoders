module Data.Country exposing (Model, decode)

import Json.Decode as Decode


type Model
    = Model Country


type Country
    = CZ
    | SK
    | Other


decode : Decode.Decoder Model
decode =
    Decode.map Model decodeCountry


decodeCountry : Decode.Decoder Country
decodeCountry =
    Decode.map stringToCountry Decode.string


stringToCountry : String -> Country
stringToCountry countryString =
    case countryString of
        "CZ" ->
            CZ

        "SK" ->
            SK

        _ ->
            Other
