module CurrentDraws exposing (init)

import Browser
import Browser.Navigation as Navigation
import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Task
import Time
import Url



-- MODEL


timeBetweenReloads : Int
timeBetweenReloads =
    60


type alias Model =
    { flags : Flags
    , translations : WebData (List Translation)
    , draws : WebData (List Draw)
    , errorMsg : Maybe String
    }


type alias Flags =
    { host : Maybe String
    , lang : String
    , subdomain : String
    , section : String
    }


type alias Translation =
    { key : String
    , label : String
    }


type alias Draw =
    { id : Int
    , startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe String)
    }



-- DECODERS


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> optional "host" (nullable string) Nothing
        |> optional "lang" string "en"
        |> required "subdomain" string
        |> optional "section" string "leagues"


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


decodeDraws : Decoder (List Draw)
decodeDraws =
    let
        decodeDraw : Decoder Draw
        decodeDraw =
            Decode.succeed Draw
                |> required "id" int
                |> required "starts_at" string
                |> required "label" string
                |> optional "attendance" int 0
                |> required "draw_sheets" (list (nullable string))
    in
    list decodeDraw



-- HELPERS


drawUrl : Int -> Draw -> String
drawUrl eventId draw =
    "/events/" ++ String.fromInt eventId ++ "/draws/" ++ String.fromInt draw.id


translate : List Translation -> String -> String
translate translations key =
    -- Translates the passed key to the current labels (server determines locale by url).
    case List.Extra.find (\translation -> String.toLower translation.key == String.toLower key) translations of
        Just translation ->
            translation.label

        Nothing ->
            key


colorNameToRGB : String -> El.Color
colorNameToRGB color =
    case color of
        "red" ->
            El.rgb255 204 0 0

        "yellow" ->
            El.rgb255 204 204 0

        _ ->
            El.rgb255 204 204 0


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl string ->
            "Bad URL used: " ++ string

        Http.Timeout ->
            "Network timeout. Please check your internet connection."

        Http.NetworkError ->
            "Network error. Please check your internet connection."

        Http.BadStatus int ->
            "Bad status response from server. Please contact Curling I/O support if the issue persists for more than a few minutes."

        Http.BadBody string ->
            "Bad body response from server. Please contact Curling I/O support if the issue persists for more than a few minutes. Details: \"" ++ string ++ "\""


baseUrl : Flags -> String
baseUrl { host, lang } =
    let
        devUrl =
            -- Development
            "http://api.curling.test:3000/" ++ lang

        -- productionUrl =
        --     -- Production without caching
        --     "https://api.curling.io/" ++ Maybe.withDefault "en" lang
        --
        productionCachedUrl =
            -- Production cached via CDN (Fastly)
            "https://api-curlingio.global.ssl.fastly.net/" ++ lang
    in
    case host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl
                --
                -- else if String.contains ".curling.io" h then
                --     productionUrl

            else
                productionCachedUrl

        Nothing ->
            productionCachedUrl


baseClubUrl : Flags -> String
baseClubUrl flags =
    baseUrl flags ++ "/clubs/" ++ flags.subdomain ++ "/"


getTranslations : Flags -> Cmd Msg
getTranslations flags =
    let
        url =
            baseUrl flags ++ "/translations"
    in
    RemoteData.Http.get url GotTranslations decodeTranslations


getDraws : Flags -> Cmd Msg
getDraws flags =
    let
        url =
            baseClubUrl flags ++ flags.section
    in
    RemoteData.Http.get url GotDraws decodeDraws


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            ( { flags = flags
              , translations = NotAsked
              , draws = NotAsked
              , errorMsg = Nothing
              }
            , Cmd.batch
                [ getTranslations flags
                , getDraws flags
                ]
            )

        Err error ->
            let
                flags =
                    { host = Nothing
                    , lang = "en"
                    , subdomain = ""
                    , section = "leagues"
                    }
            in
            ( { flags = flags
              , translations = NotAsked
              , draws = NotAsked
              , errorMsg = Nothing
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = NoOp
    | NavigateOut String
    | GotTranslations (WebData (List Translation))
    | GotDraws (WebData (List Draw))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NavigateOut url ->
            ( model, Navigation.load url )

        GotTranslations response ->
            ( { model | translations = response, errorMsg = Nothing }, Cmd.none )

        GotDraws response ->
            ( { model | draws = response, errorMsg = Nothing }, Cmd.none )



-- COLORS


theme =
    { primary = El.rgb255 237 25 64
    , primaryFocused = El.rgb255 197 10 49
    , secondary = El.rgb255 108 117 125
    , secondaryFocused = El.rgb255 128 137 155
    , white = El.rgb255 255 255 255
    , greyLight = El.rgba 0 0 0 0.05
    , grey = El.rgba 0 0 0 0.1
    , greyMedium = El.rgba 0 0 0 0.2
    , greyStrong = El.rgba 0 0 0 0.4
    , greyDark = El.rgba 0 0 0 0.6
    , defaultText = El.rgb255 33 37 41
    , transparent = El.rgba 1 1 1 0
    }



-- VIEWS


view : Model -> Html Msg
view model =
    El.layout
        [ Font.size 16
        , Font.color theme.defaultText
        , El.width El.fill
        , El.height El.fill
        , El.padding 10
        , Font.family
            [ Font.typeface "-apple-system"
            , Font.typeface "BlinkMacSystemFont"
            , Font.typeface "Segoe UI"
            , Font.typeface "Roboto"
            , Font.typeface "Helvetica Neue"
            , Font.typeface "Arial"
            , Font.typeface "Noto Sans"
            , Font.typeface "Noto Sans"
            , Font.typeface "Liberation Sans"
            , Font.typeface "Apple Color Emoji"
            , Font.typeface "Segoe UI Emoji"
            , Font.typeface "Segoe UI Symbol"
            , Font.typeface "Noto Color Emoji"
            , Font.sansSerif
            ]
        ]
        (el [] (text "Current Draws"))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
