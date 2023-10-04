module Theme exposing (Theme, decodeTheme, defaultTheme)

import Element exposing (Color, rgb255, rgba)
import Hex
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (hardcoded, optional)


type alias Theme =
    { defaultText : Color
    , white : Color
    , transparent : Color
    , grey : Color
    , greyLight : Color
    , greyDark : Color
    , primary : Color
    , secondary : Color
    }


defaultTheme : Theme
defaultTheme =
    { defaultText = rgb255 33 37 41
    , white = rgb255 255 255 255
    , transparent = rgba 1 1 1 0
    , grey = rgb255 210 210 210
    , greyLight = rgb255 248 248 248
    , greyDark = rgb255 52 58 64
    , primary = hexToColor "#ed1940"
    , secondary = hexToColor "#5c5c5c"
    }


decodeTheme : Decoder Theme
decodeTheme =
    let
        decodeStringColor : Decoder Color
        decodeStringColor =
            string
                |> Decode.andThen
                    (\str ->
                        hexToColor str
                            |> Decode.succeed
                    )
    in
    Decode.succeed Theme
        |> hardcoded defaultTheme.defaultText
        |> hardcoded defaultTheme.white
        |> hardcoded defaultTheme.transparent
        |> hardcoded defaultTheme.grey
        |> hardcoded defaultTheme.greyLight
        |> hardcoded defaultTheme.greyDark
        |> optional "primary" decodeStringColor defaultTheme.primary
        |> optional "secondary" decodeStringColor defaultTheme.secondary


hexToColor : String -> Color
hexToColor value =
    let
        val =
            String.toLower value

        hexRecord =
            { red = String.slice 1 3 val
            , green = String.slice 3 5 val
            , blue = String.slice 5 7 val
            }

        rgbRecord =
            { red = hexRecord.red |> Hex.fromString |> Result.withDefault 0
            , green = hexRecord.green |> Hex.fromString |> Result.withDefault 0
            , blue = hexRecord.blue |> Hex.fromString |> Result.withDefault 0
            }

        color =
            rgb255 rgbRecord.red rgbRecord.green rgbRecord.blue
    in
    color
