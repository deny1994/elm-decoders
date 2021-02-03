module Data.Document exposing (Model, decode)

import Data.Date as Date
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type Model
    = Model Document


type alias Document =
    { id : String
    , expireDate : Date.Model
    , documentType : DocumentType
    }


type DocumentType
    = IdentityCard
    | DrivingLicense DrivingLicenseData


type alias DrivingLicenseData =
    { group : String
    }


decode : Decode.Decoder Model
decode =
    Decode.map Model decodeDocument


decodeDocument : Decode.Decoder Document
decodeDocument =
    Decode.succeed identity
        |> Pipeline.required "documentType" Decode.string
        |> Decode.andThen
            (\documentTypeString ->
                Decode.succeed Document
                    |> Pipeline.required "id" Decode.string
                    |> Pipeline.required "expireDate" Date.decode
                    |> Pipeline.custom (decodeDocumentType documentTypeString)
            )


decodeDocumentType : String -> Decode.Decoder DocumentType
decodeDocumentType documentTypeString =
    case documentTypeString of
        "identityCard" ->
            Decode.succeed IdentityCard

        "drivingLicense" ->
            Decode.map DrivingLicense decodeDrivingLicenseData

        _ ->
            Decode.fail "Unknown document type"


decodeDrivingLicenseData : Decode.Decoder DrivingLicenseData
decodeDrivingLicenseData =
    Decode.succeed DrivingLicenseData
        |> Pipeline.required "group" Decode.string
