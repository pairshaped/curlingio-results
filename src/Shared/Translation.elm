module Shared.Translation exposing (Translation, decodeTranslations, translate)

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required)
import List.Extra


type alias Translation =
    { key : String
    , label : String
    }


decodeTranslations : Decoder (List Translation)
decodeTranslations =
    let
        decodeTranslation : Decoder Translation
        decodeTranslation =
            Decode.succeed Translation
                |> required "key" string
                |> required "label" string
    in
    list decodeTranslation


translate : List Translation -> String -> String
translate translations key =
    -- Translates the passed key to the current labels (server determines locale by url).
    case List.Extra.find (\translation -> String.toLower translation.key == String.toLower key) translations of
        Just translation ->
            translation.label

        Nothing ->
            key
