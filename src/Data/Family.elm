module Data.Family exposing
    ( Model
    , bothParentsDecoded
    , brotherDecoded
    , decode
    , fatherDecoded
    , motherDecoded
    , noFamilyMembersDecoded
    , sisterDecoded
    , withBothParentsAndsSiblingsDecoded
    , withSiblingsDecoded
    )

import Data.OneOrMore as OneOrMore
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type Model
    = Model Family


type alias Family =
    { parents : Parents
    , siblings : Siblings
    }


type Siblings
    = OnlyChild
    | OneBrotherOrMore (OneOrMore.Model Brother)
    | OneSisterOrMore (OneOrMore.Model Sister)
    | WithSiblings (OneOrMore.Model Sister) (OneOrMore.Model Brother)


type alias Parents =
    { mother : Maybe Mother
    , father : Maybe Father
    }


type Brother
    = Brother


type Sister
    = Sister


type Mother
    = Mother


type Father
    = Father


decode : Decode.Decoder Model
decode =
    Decode.map Model decodeFamily


decodeFamily : Decode.Decoder Family
decodeFamily =
    Decode.succeed Family
        |> Pipeline.custom decodeParents
        |> Pipeline.custom decodeSiblings


decodeParents : Decode.Decoder Parents
decodeParents =
    Decode.succeed Parents
        |> Pipeline.custom (decodeParent decodeMother)
        |> Pipeline.custom (decodeParent decodeFather)


decodeParent parentDecoder =
    Decode.succeed identity
        |> Pipeline.custom (Decode.list (Decode.maybe parentDecoder))
        |> Decode.andThen emptyOrOne


decodeMother : Decode.Decoder Mother
decodeMother =
    decodePerson "mother" Mother


decodeFather : Decode.Decoder Father
decodeFather =
    decodePerson "father" Father


decodeSister : Decode.Decoder Sister
decodeSister =
    decodePerson "sister" Sister


decodeBrother : Decode.Decoder Brother
decodeBrother =
    decodePerson "brother" Brother


decodePerson : String -> a -> Decode.Decoder a
decodePerson relationshipStringMatch person =
    Decode.succeed identity
        |> Pipeline.required "relationship" Decode.string
        |> Decode.andThen
            (\relationshipString ->
                if relationshipString == relationshipStringMatch then
                    Decode.succeed person

                else
                    Decode.fail "Unknown relationship"
            )


emptyOrOne : List (Maybe a) -> Decode.Decoder (Maybe a)
emptyOrOne list =
    case List.filter ((/=) Nothing) list of
        [] ->
            Decode.succeed Nothing

        first :: [] ->
            Decode.succeed first

        _ ->
            Decode.fail "Can't have 2 or more mothers/fathers"


decodeSiblings : Decode.Decoder Siblings
decodeSiblings =
    Decode.succeed Tuple.pair
        |> Pipeline.custom (decodeSibling decodeSister)
        |> Pipeline.custom (decodeSibling decodeBrother)
        |> Decode.map toSiblings


decodeSibling siblingDecoder =
    Decode.succeed identity
        |> Pipeline.custom (Decode.list (Decode.maybe siblingDecoder))
        |> Decode.map (List.filterMap identity)


toSiblings : ( List Sister, List Brother ) -> Siblings
toSiblings siblingsTuple =
    case siblingsTuple of
        ( [], [] ) ->
            OnlyChild

        ( sister :: sisters, [] ) ->
            OneSisterOrMore (OneOrMore.toModel ( sister, sisters ))

        ( [], brother :: brothers ) ->
            OneBrotherOrMore (OneOrMore.toModel ( brother, brothers ))

        ( sister :: sisters, brother :: brothers ) ->
            WithSiblings (OneOrMore.toModel ( sister, sisters )) (OneOrMore.toModel ( brother, brothers ))



---- For test purposes !!! ---


motherDecoded : Model
motherDecoded =
    Model
        { parents = { mother = Just Mother, father = Nothing }
        , siblings = OnlyChild
        }


fatherDecoded : Model
fatherDecoded =
    Model
        { parents = { mother = Nothing, father = Just Father }
        , siblings = OnlyChild
        }


sisterDecoded : Model
sisterDecoded =
    Model
        { parents = { mother = Nothing, father = Nothing }
        , siblings = OneSisterOrMore <| OneOrMore.toModel ( Sister, [] )
        }


brotherDecoded : Model
brotherDecoded =
    Model
        { parents = { mother = Nothing, father = Nothing }
        , siblings = OneBrotherOrMore <| OneOrMore.toModel ( Brother, [] )
        }


bothParentsDecoded : Model
bothParentsDecoded =
    Model
        { parents = { mother = Just Mother, father = Just Father }
        , siblings = OnlyChild
        }


withSiblingsDecoded : Model
withSiblingsDecoded =
    Model
        { parents = { mother = Nothing, father = Nothing }
        , siblings = WithSiblings (OneOrMore.toModel ( Sister, [] )) (OneOrMore.toModel ( Brother, [] ))
        }


withBothParentsAndsSiblingsDecoded : Model
withBothParentsAndsSiblingsDecoded =
    Model
        { parents = { mother = Just Mother, father = Just Father }
        , siblings = WithSiblings (OneOrMore.toModel ( Sister, [] )) (OneOrMore.toModel ( Brother, [] ))
        }


noFamilyMembersDecoded : Model
noFamilyMembersDecoded =
    Model
        { parents = { mother = Nothing, father = Nothing }
        , siblings = OnlyChild
        }
