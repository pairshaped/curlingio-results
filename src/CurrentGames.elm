module CurrentGames exposing (main)

import Browser
import Element as El exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Shared.Theme exposing (Theme, decodeTheme, defaultTheme)
import Shared.Translation exposing (Translation, decodeTranslations, translate)



-- MODEL


type alias Model =
    { flags : Flags
    , translations : WebData (List Translation)
    , sides : WebData (List Side)
    , errorMsg : Maybe String
    }


type alias Flags =
    { subdomain : String
    , lang : String
    , host : Maybe String
    , showEndScores : Bool
    , theme : Theme
    }


type alias Side =
    { gameId : String
    , startsAt : String
    , gameName : Maybe String
    , result : Maybe SideResult
    , score : Maybe Int
    , teamShortName : String
    , gameState : GameState
    , eventName : String
    , endScores : List Int
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


type SideResult
    = SideResultWon
    | SideResultLost
    | SideResultTied
    | SideResultConceded
    | SideResultForfeited
    | SideResultTimePenalized



-- DECODERS


decodeFlags : Decoder Flags
decodeFlags =
    Decode.succeed Flags
        |> required "subdomain" string
        |> optional "lang" string "en"
        |> optional "host" (nullable string) Nothing
        |> optional "showEndScores" bool True
        |> optional "theme" decodeTheme defaultTheme


decodeSides : Decoder (List Side)
decodeSides =
    let
        decodeSide : Decoder Side
        decodeSide =
            let
                decodeGameState : Decoder GameState
                decodeGameState =
                    string
                        |> Decode.andThen
                            (\str ->
                                case str of
                                    "active" ->
                                        Decode.succeed GameActive

                                    "complete" ->
                                        Decode.succeed GameComplete

                                    _ ->
                                        Decode.succeed GamePending
                            )

                decodeSideResult : Decoder (Maybe SideResult)
                decodeSideResult =
                    string
                        |> Decode.andThen
                            (\str ->
                                case str of
                                    "won" ->
                                        Decode.succeed (Just SideResultWon)

                                    "lost" ->
                                        Decode.succeed (Just SideResultLost)

                                    "tied" ->
                                        Decode.succeed (Just SideResultTied)

                                    "conceded" ->
                                        Decode.succeed (Just SideResultConceded)

                                    "forfeited" ->
                                        Decode.succeed (Just SideResultForfeited)

                                    "time_penalized" ->
                                        Decode.succeed (Just SideResultTimePenalized)

                                    _ ->
                                        Decode.succeed Nothing
                            )
            in
            Decode.succeed Side
                |> required "game_id" string
                |> required "starts_at" string
                |> optional "game_name" (nullable string) Nothing
                |> optional "result" decodeSideResult Nothing
                |> required "score" (nullable int)
                |> required "team_short_name" string
                |> required "game_state" decodeGameState
                |> required "event_name" string
                |> optional "end_scores" (list int) []
    in
    list decodeSide



-- HELPERS


gameStateToString : List Translation -> GameState -> String
gameStateToString translations gameState =
    translate translations
        (case gameState of
            GamePending ->
                "pending"

            GameActive ->
                "active"

            GameComplete ->
                "complete"
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

        Http.BadStatus _ ->
            "Bad status response from server. Please contact Curling I/O support if the issue persists for more than a few minutes."

        Http.BadBody string ->
            "Bad body response from server. Please contact Curling I/O support if the issue persists for more than a few minutes. Details: \"" ++ string ++ "\""


baseUrl : Flags -> String
baseUrl { host, lang } =
    let
        productionCachedUrl =
            -- Production cached via CDN (Fastly)
            "https://api-curlingio.global.ssl.fastly.net/" ++ lang
    in
    case host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                let
                    devUrl =
                        -- Development
                        "http://api.curling.test:3000/" ++ lang
                in
                devUrl

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


getSides : Flags -> Cmd Msg
getSides flags =
    let
        url =
            baseClubUrl flags ++ "current_game_positions"
    in
    RemoteData.Http.get url GotSides decodeSides


groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile isSameGroup items =
    -- It's kind of strange, but the List.Extra.groupWhile function return the first matching record as the
    -- first item in a tuple, and all other matching records in a list as the second item in the tuple.
    -- This saves us from dealing with the Maybe returned from List.head, but at the cost of breaking our brains.
    -- IMO not worth, so we're making it more intuitive by returning an inclusive list of the groupings (list of lists)
    -- instead of a tuple.
    List.Extra.groupWhile isSameGroup items
        -- Convert the tuple to just a list by moving the head item over
        |> List.map (\( x, xs ) -> x :: xs)


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            ( { flags = flags
              , translations = NotAsked
              , sides = NotAsked
              , errorMsg = Nothing
              }
            , Cmd.batch
                [ getTranslations flags
                , getSides flags
                ]
            )

        Err _ ->
            let
                flags =
                    { subdomain = ""
                    , lang = "en"
                    , host = Nothing
                    , showEndScores = False
                    , theme = defaultTheme
                    }
            in
            ( { flags = flags
              , translations = NotAsked
              , sides = NotAsked
              , errorMsg = Nothing
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = GotTranslations (WebData (List Translation))
    | GotSides (WebData (List Side))
    | Reload


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTranslations response ->
            ( { model | translations = response, errorMsg = Nothing }, Cmd.none )

        GotSides response ->
            ( { model | sides = response, errorMsg = Nothing }, Cmd.none )

        Reload ->
            let
                ( translations, translationsCmd ) =
                    case model.translations of
                        Success _ ->
                            ( model.translations, Cmd.none )

                        _ ->
                            ( Loading, getTranslations model.flags )

                ( sides, sidesCmd ) =
                    case model.sides of
                        Success _ ->
                            ( model.sides, Cmd.none )

                        _ ->
                            ( Loading, getSides model.flags )
            in
            ( { model
                | translations = translations
                , sides = sides
              }
            , Cmd.batch
                [ translationsCmd
                , sidesCmd
                ]
            )



-- VIEWS


view : Model -> Html Msg
view model =
    El.layout
        [ Font.size 16
        , Font.color model.flags.theme.defaultText
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
        (case ( model.translations, model.sides ) of
            ( Success translations, Success sides ) ->
                viewGames model.flags translations sides

            ( Failure error, _ ) ->
                viewFetchError model.flags (errorMessage error)

            ( _, Failure error ) ->
                viewFetchError model.flags (errorMessage error)

            _ ->
                el [] (text "Loading...")
        )


viewFetchError : Flags -> String -> Element Msg
viewFetchError { theme } message =
    column [ El.spacing 10 ]
        [ el [] (text message)
        , button
            [ Background.color theme.primary
            , Font.color theme.white
            , El.paddingXY 12 10
            , Border.rounded 4
            , El.focused [ Background.color theme.primary ]
            ]
            { onPress = Just Reload
            , label = text "Reload"
            }
        ]


viewGames : Flags -> List Translation -> List Side -> Element Msg
viewGames { theme, showEndScores } translations sides =
    let
        viewGame : List Side -> Element Msg
        viewGame gameSides =
            let
                title =
                    case List.head gameSides of
                        Just top ->
                            top.eventName
                                ++ (case top.gameName of
                                        Just gameName ->
                                            " - " ++ gameName

                                        Nothing ->
                                            ""
                                   )

                        Nothing ->
                            -- This should never happen
                            ""

                subTitle =
                    case List.head gameSides of
                        Just top ->
                            top.startsAt ++ " - " ++ gameStateToString translations top.gameState

                        Nothing ->
                            -- This should never happen
                            ""

                viewSide : Side -> Element Msg
                viewSide side =
                    let
                        viewTeamName =
                            el [ El.width El.fill, El.clip ] (text side.teamShortName)

                        viewScore =
                            el
                                [ El.alignRight
                                , El.width (El.px 20)
                                , Font.bold
                                ]
                                (el [ El.alignRight ]
                                    (text
                                        (case side.score of
                                            Just score ->
                                                String.fromInt score

                                            Nothing ->
                                                "-"
                                        )
                                    )
                                )

                        viewEndScores =
                            let
                                viewEndScore endScore =
                                    el [ El.alignRight, El.width (El.px 10) ] (text (String.fromInt endScore))
                            in
                            if showEndScores then
                                row [ El.alignRight, El.spacing 5 ]
                                    (List.map viewEndScore side.endScores)

                            else
                                El.none
                    in
                    row
                        [ El.width El.fill
                        , El.spacing 10
                        , El.paddingXY 0 5
                        , Font.color
                            (case side.result of
                                Just SideResultWon ->
                                    theme.primary

                                _ ->
                                    theme.defaultText
                            )
                        ]
                        [ viewTeamName, viewEndScores, viewScore ]
            in
            column
                -- [ El.width El.fill
                [ El.width (El.px 280)
                , El.spacing 5
                , El.padding 5
                , Border.width 1
                , case Maybe.map .gameState (List.head gameSides) of
                    Just GameActive ->
                        Border.color theme.primary

                    _ ->
                        Border.color theme.grey
                ]
                [ El.wrappedRow [ Font.size 12 ] [ text title ]
                , el [ Font.size 12 ] (text subTitle)
                , column [ El.width El.fill ] (List.map viewSide gameSides)
                ]
    in
    if List.isEmpty sides then
        el [] (text (translate translations "no_current_games"))

    else
        El.wrappedRow [ El.width El.fill, El.spacing 10 ]
            (groupWhile (\a b -> a.gameId == b.gameId) sides
                |> List.map viewGame
            )



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
