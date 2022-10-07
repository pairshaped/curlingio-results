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
import Svg
import Svg.Attributes
import Task
import Time



-- MODEL


type alias Model =
    { flags : Flags
    , items : WebData (List Item)
    , selectedEvent : WebData Event
    , fullScreen : Bool
    , errorMsg : Maybe String
    }


type alias Flags =
    { baseUrl : String
    , section : ItemsSection
    , registration : Bool
    , pageSize : Int
    }


type ItemsSection
    = LeaguesSection
    | CompetitionsSection
    | ProductsSection


type alias Item =
    { id : Int
    , name : String
    , registrationState : Maybe RegistrationState
    , price : Maybe String
    , runs : Maybe String
    , location : Maybe String
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


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> required "baseUrl" string
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "pageSize" int 10


decodeSection : Decoder ItemsSection
decodeSection =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "competitions" ->
                        Decode.succeed CompetitionsSection

                    "products" ->
                        Decode.succeed ProductsSection

                    _ ->
                        Decode.succeed LeaguesSection
            )


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


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            ( Model flags NotAsked NotAsked False Nothing
            , getItems flags.section flags.baseUrl
            )

        Err error ->
            ( Model (Flags "" LeaguesSection False 10) NotAsked NotAsked False (Just (Decode.errorToString error))
            , Cmd.none
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


getItems : ItemsSection -> String -> Cmd Msg
getItems section baseUrl =
    let
        url =
            baseUrl
                ++ "/api/"
                ++ (case section of
                        LeaguesSection ->
                            "leagues"

                        CompetitionsSection ->
                            "competitions"

                        ProductsSection ->
                            "products"
                   )
    in
    RemoteData.Http.get url GotItems decodeItems


svgFullScreen =
    Svg.svg
        [ Svg.Attributes.width "32"
        , Svg.Attributes.height "32"
        , Svg.Attributes.viewBox "0 0 24 24"
        ]
        -- [ Svg.path [ Svg.Attributes.d "M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z" ] [] ]
        [ Svg.path [ Svg.Attributes.d "M5.828 10.172a.5.5 0 0 0-.707 0l-4.096 4.096V11.5a.5.5 0 0 0-1 0v3.975a.5.5 0 0 0 .5.5H4.5a.5.5 0 0 0 0-1H1.732l4.096-4.096a.5.5 0 0 0 0-.707zm4.344 0a.5.5 0 0 1 .707 0l4.096 4.096V11.5a.5.5 0 1 1 1 0v3.975a.5.5 0 0 1-.5.5H11.5a.5.5 0 0 1 0-1h2.768l-4.096-4.096a.5.5 0 0 1 0-.707zm0-4.344a.5.5 0 0 0 .707 0l4.096-4.096V4.5a.5.5 0 1 0 1 0V.525a.5.5 0 0 0-.5-.5H11.5a.5.5 0 0 0 0 1h2.768l-4.096 4.096a.5.5 0 0 0 0 .707zm-4.344 0a.5.5 0 0 1-.707 0L1.025 1.732V4.5a.5.5 0 0 1-1 0V.525a.5.5 0 0 1 .5-.5H4.5a.5.5 0 0 1 0 1H1.732l4.096 4.096a.5.5 0 0 1 0 .707z" ] [] ]



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
            ( { model | items = response, errorMsg = Nothing }, Cmd.none )

        ReloadItems ->
            ( model, getItems model.flags.section model.flags.baseUrl )



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
        [ case model.errorMsg of
            Just errorMsg ->
                viewNotReady errorMsg

            Nothing ->
                case model.items of
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
    let
        sectionTitle =
            case flags.section of
                LeaguesSection ->
                    "Leagues"

                CompetitionsSection ->
                    "Competitions"

                ProductsSection ->
                    "Products"
    in
    div
        [ class "p-3" ]
        [ div
            [ class "d-flex justify-content-between" ]
            [ h5 [] [ text sectionTitle ]

            -- , div [ class "d-flex text-right" ]
            -- [ button [ class "btn btn-sm btn-primary mr-2", onClick ReloadItems ] [ text "Reload" ]
            , div
                [ style "cursor" "pointer"
                , style "margin-right" "-10px"
                , onClick ToggleFullScreen
                ]
                [ svgFullScreen ]
            ]
        , div
            [ class "table-responsive" ]
            [ table
                [ class "table" ]
                [ tr []
                    [ th [] []
                    ]
                ]
            ]
        ]


viewSelectedEvent : Event -> Html Msg
viewSelectedEvent event =
    div [] [ text "Event" ]



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
