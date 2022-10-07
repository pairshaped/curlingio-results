module Results exposing (..)

import Browser
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, label, option, p, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, property, selected, style, tabindex, title, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode exposing (Decoder, array, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import Process
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Task
import Time



-- MODEL


type alias Model =
    { flags : Flags
    , items : WebData (List Item)
    , selectedEvent : WebData Event
    , fullScreen : Bool
    }

type alias Flags =
    { baseUrl : String
    , section : String
    , registration : Bool
    , pageSize : Int
    }

type alias Item =
    { id : Int
    , name : String
    , registrationState: Maybe RegistrationState
    , price: Maybe String
    , runs : Maybe String
    , location: Maybe String
    }

type RegistrationState
    = RegistrationOpen
    | RegistrationSoldOut
    | RegistrationClosed

type alias Event =
    { stages : List Stage
    , draws : List Draw
    , games : List Game
    }


type alias Stage =
    { id : Int
    , label : String
    }


type alias Draw =
    { id : Int
    , label : String
    , startsAt : String
    }


type alias Game =
    { id : String
    , drawId : Int
    , sheet : Int
    , name : String
    , state : GameState
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


-- DECODERS


isLocalMode : String -> Bool
isLocalMode url =
    String.contains "localhost" url


decodeItems : Decoder (List Item)
decodeItems =
    list decodeItem


decodeItem : Decoder Item
decodeItem =
    Decode.succeed Item
        |> required "id" int
        |> required "name" string
        |> optional "registration_state" decodeRegistrationState Nothing
        |> optional "price" (nullable string) Nothing
        |> optional "runs" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing

decodeRegistrationState : Decoder (Maybe RegistrationState)
decodeRegistrationState =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "open" ->
                        Decode.succeed (Just RegistrationOpen)

                    "sold_out" ->
                        Decode.succeed (Just RegistrationSoldOut)

                    "closed" ->
                        Decode.succeed (Just RegistrationClosed)

                    _ ->
                        Decode.succeed Nothing
            )



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags NotAsked NotAsked False
    , getItems flags.section flags.baseUrl
    )


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


getItems : String -> String -> Cmd Msg
getItems section baseUrl =
    let
        validSection =
            case section of
                "competitions" ->
                    "competitions"
                "products" ->
                    "products"
                _ ->
                    "leagues"
        url =
            baseUrl ++ "/api/" ++ validSection
    in
    RemoteData.Http.get url GotItems decodeItems


-- UPDATE


type Msg
    = ToggleFullScreen
    | GotItems (WebData (List Item))
    | ReloadItems


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        GotItems response ->
            ( { model | items = response }, Cmd.none )

        ReloadItems ->
            (model, getItems model.flags.section model.flags.baseUrl )



-- VIEWS


view : Model -> Html Msg
view model =
    div
        (List.append
            [ id "curlingio__results" ]
            (if model.fullScreen then
                [ style "width" "100%"
                , style "height" "100%"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "z-index" "100"
                , style "overflow-y" "auto"
                , style "backgroup-color" "#fff"
                ]

             else
                []
            )
        )
        [ case model.items of
            NotAsked ->
                viewNotReady "Initializing..."

            Loading ->
                viewNotReady "Loading..."

            Failure error ->
                viewFetchError (errorMessage error)

            Success items ->
                case model.selectedEvent of
                    Success event ->
                        viewSelectedEvent event

                    Loading ->
                        viewNotReady "Loading..."

                    Failure error ->
                        viewFetchError (errorMessage error)

                    _ ->
                        viewItems model items
        ]


viewNotReady : String -> Html Msg
viewNotReady message =
    p [ class "p-3" ] [ text message ]


viewFetchError : String -> Html Msg
viewFetchError message =
    div
        [ class "p-3" ]
        [ p [] [ text message ]
        , button [ class "btn btn-primary", onClick ReloadItems ] [ text "Reload" ]
        ]


viewItems : Model -> List Item -> Html Msg
viewItems { flags, fullScreen } items =
    div
        [ class "p-3" ]
        [ div
            [ class "d-flex justify-content-between mb-2" ]
            [ h5 [] [ text "Items" ]
            , div [ class "text-right" ]
                [ button [ class "btn btn-sm btn-primary mr-2", onClick ReloadItems ] [ text "Reload" ]
                , button [ class "btn btn-sm btn-secondary", onClick ToggleFullScreen ]
                    [ text
                        (if fullScreen then
                            "Exit"

                         else
                            "Full Screen"
                        )
                    ]
                ]
            ]
        , div
            [ class "table-responsive" ]
            [ table
                [ class "table" ]
                []
            ]
        ]



viewSelectedEvent : Event -> Html Msg
viewSelectedEvent event =
    div [] [ text "Event"]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
