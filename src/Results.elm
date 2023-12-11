port module Results exposing (init)

-- import Element.HexColor as HexColor

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import CustomSvg exposing (..)
import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, style)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Task
import Theme exposing (Theme, decodeTheme, defaultTheme)
import Time
import Translation exposing (Translation, decodeTranslations, translate)
import Url
import Url.Parser exposing ((</>), Parser)



-- MODEL


gridSize : Int
gridSize =
    50


timeBetweenReloads : Int
timeBetweenReloads =
    60


type alias Model =
    { flags : Flags
    , hash : String
    , device : Device
    , fullScreen : Bool
    , translations : WebData (List Translation)
    , items : WebData (List Item)
    , itemFilter : ItemFilter
    , product : WebData Product
    , event : WebData Event
    , scoringHilight : Maybe ScoringHilight
    , errorMsg : Maybe String
    , reloadIn : Int
    }


type Route
    = ItemsRoute
    | ProductRoute Int
    | EventRoute Int NestedEventRoute


type NestedEventRoute
    = DetailsRoute
    | RegistrationsRoute
    | SparesRoute
    | DrawsRoute
    | DrawRoute Int
    | GameRoute String
    | StagesRoute
    | StageRoute Int
    | TeamsRoute
    | TeamRoute Int
    | ReportsRoute
    | ReportRoute String


type ScoringHilight
    = HilightHammers
    | HilightStolenEnds
    | HilightBlankEnds
    | Hilight1PointEnds
    | Hilight2PointEnds
    | Hilight3PointEnds
    | Hilight4PointEnds
    | Hilight5PlusPointEnds


type alias Flags =
    { host : Maybe String
    , hash : Maybe String
    , lang : String
    , apiKey : Maybe String
    , subdomain : Maybe String
    , fullScreenToggle : Bool
    , section : ItemsSection
    , registration : Bool
    , showWaiversForTeams : Bool
    , excludeEventSections : List String
    , defaultEventSection : Maybe String
    , eventId : Maybe Int
    , theme : Theme
    , loggedInCurlerIds : List Int
    }


type ItemsSection
    = LeaguesSection
    | CompetitionsSection
    | ProductsSection


type alias ItemFilter =
    { page : Int
    , seasonDelta : Int
    , search : String
    , seasonSearchOpen : Bool
    }


type alias Item =
    { id : Int
    , name : String
    , summary : Maybe String
    , occursOn : Maybe String
    , location : Maybe String
    , noRegistrationMessage : Maybe String
    , price : Maybe String
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , publishResults : Bool
    }


type alias Product =
    { id : Int
    , name : String
    , summary : Maybe String
    , description : Maybe String
    , sponsor : Maybe Sponsor
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , totalWithTax : Maybe String
    , potentialDiscounts : List String
    }


type alias Event =
    { id : Int
    , name : String
    , summary : Maybe String
    , description : Maybe String
    , note : Maybe String
    , teamRestriction : String
    , ageRange : String
    , sponsor : Maybe Sponsor
    , startsOn : String
    , endsOn : String
    , state : EventState
    , noRegistrationMessage : Maybe String
    , registrationOpensAt : Maybe String
    , registrationClosesAt : Maybe String
    , spotsAvailable : Maybe Int
    , spotsRemaining : Maybe Int
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , totalWithTax : Maybe String
    , potentialDiscounts : List String
    , endScoresEnabled : Bool
    , shotByShotEnabled : Bool
    , numberOfEnds : Int
    , topRock : String
    , botRock : String
    , sheetNames : List String
    , teams : List Team
    , registrations : List Registration
    , spares : List Spare
    , stages : List Stage
    , draws : List Draw
    }


type EventState
    = EventStatePending
    | EventStateActive
    | EventStateComplete


type alias Sponsor =
    { logoUrl : String
    , name : Maybe String
    , url : Maybe String
    }


type alias Team =
    { id : Int
    , name : String
    , shortName : String
    , coach : Maybe String
    , affiliation : Maybe String
    , location : Maybe String
    , contactName : Maybe String
    , contactEmail : Maybe String
    , contactPhone : Maybe String
    , imageUrl : Maybe String
    , lineup : List TeamCurler
    }


type alias TeamCurler =
    { curlerId : Int
    , position : Maybe TeamPosition
    , skip : Bool
    , name : String
    , delivery : Maybe RockDelivery
    , clubName : Maybe String
    , clubCity : Maybe String
    , photoUrl : Maybe String
    , waiver : Bool
    }


type TeamPosition
    = TeamFourth
    | TeamThird
    | TeamSecond
    | TeamFirst
    | TeamAlternate


type RockDelivery
    = RockDeliveryRight
    | RockDeliveryLeft


type alias Registration =
    { curlerName : Maybe String
    , teamName : Maybe String
    , skipName : Maybe String
    , position : Maybe String
    , lineup : Maybe Lineup
    }


type alias Spare =
    { name : String
    , positions : List String
    }


type alias Lineup =
    { first : Maybe String
    , second : Maybe String
    , third : Maybe String
    , fourth : Maybe String
    , alternate : Maybe String
    }


type alias Stage =
    { id : Int
    , stageType : StageType
    , name : String
    , groups : Maybe (List Group)
    , games : List Game
    , standings : List Standing
    }


type alias SkillRank =
    { teamId : Int
    , skillRank : Int
    }


type StageType
    = RoundRobin
    | Bracket


type RankingMethod
    = PointsRanking
    | WinsRanking
    | SkinsRanking
    | ScoresRanking


type Tiebreaker
    = TiebreakerNone
    | TiebreakerHeadToHead
    | TiebreakerScores


type alias Draw =
    { id : Int
    , startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe String)
    }


type alias Group =
    { id : Int
    , name : String
    }


type alias Game =
    { id : String
    , name : String
    , state : GameState
    , coords : Maybe GameCoords
    , sides : List Side
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


type alias GameCoords =
    { groupId : Int
    , row : Int
    , col : Int
    }


type alias Side =
    { teamId : Maybe Int
    , topRock : Bool
    , firstHammer : Bool
    , result : Maybe SideResult
    , score : Maybe Int
    , endScores : List Int
    , winnerId : Maybe String
    , loserId : Maybe String
    , shots : List Shot
    }


type SideResult
    = SideResultWon
    | SideResultLost
    | SideResultTied
    | SideResultConceded
    | SideResultForfeited
    | SideResultTimePenalized


type alias Shot =
    { endNumber : Int
    , shotNumber : Int
    , curlerId : Maybe Int
    , turn : Maybe String
    , throw : Maybe String
    , rating : Maybe String
    }


type alias HogLineViolation =
    { drawLabel : String
    , teamId : Int
    , curlerId : Int
    , endNumber : Int
    }


type alias Standing =
    { teamId : Int
    , rank : Int
    , played : Int
    , wins : Int
    , losses : Int
    , ties : Int
    , points : Float
    }


type alias LineConnector =
    { fromCoords : ( Int, Int )
    , toCoords : ( Int, Int )
    }



-- DECODERS


decodeFlags : Decoder Flags
decodeFlags =
    let
        decodeSection : Decoder ItemsSection
        decodeSection =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "competitions" ->
                                Decode.succeed CompetitionsSection

                            "competition" ->
                                Decode.succeed CompetitionsSection

                            "products" ->
                                Decode.succeed ProductsSection

                            "product" ->
                                Decode.succeed ProductsSection

                            _ ->
                                Decode.succeed LeaguesSection
                    )
    in
    Decode.succeed Flags
        |> required "host" (nullable string)
        |> required "hash" (nullable string)
        |> optional "lang" string "en"
        |> optional "apiKey" (nullable string) Nothing
        |> optional "subdomain" (nullable string) Nothing
        |> optional "fullScreenToggle" bool False
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "showWaiversForTeams" bool False
        |> optional "excludeEventSections" (list string) []
        |> optional "defaultEventSection" (nullable string) Nothing
        |> optional "eventId" (nullable int) Nothing
        |> optional "theme" decodeTheme defaultTheme
        |> optional "loggedInCurlerIds" (list int) []


decodeItems : Decoder (List Item)
decodeItems =
    let
        decodeItem : Decoder Item
        decodeItem =
            Decode.succeed Item
                |> required "id" int
                |> required "name" string
                |> optional "summary" (nullable string) Nothing
                |> optional "occurs_on" (nullable string) Nothing
                |> optional "location" (nullable string) Nothing
                |> optional "no_registration_message" (nullable string) Nothing
                |> optional "price" (nullable string) Nothing
                |> optional "add_to_cart_url" (nullable string) Nothing
                |> optional "add_to_cart_text" (nullable string) Nothing
                |> optional "publish_results" bool False
    in
    list decodeItem


decodeSponsor : Decoder Sponsor
decodeSponsor =
    Decode.succeed Sponsor
        |> required "logo_url" string
        |> optional "name" (nullable string) Nothing
        |> optional "url" (nullable string) Nothing


decodeProduct : Decoder Product
decodeProduct =
    Decode.succeed Product
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing
        |> optional "sponsor" (nullable decodeSponsor) Nothing
        |> optional "add_to_cart_url" (nullable string) Nothing
        |> optional "add_to_cart_text" (nullable string) Nothing
        |> optional "total_with_tax" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []


decodeEvent : Decoder Event
decodeEvent =
    let
        decodeEventState : Decoder EventState
        decodeEventState =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "pending" ->
                                Decode.succeed EventStatePending

                            "active" ->
                                Decode.succeed EventStateActive

                            _ ->
                                Decode.succeed EventStateComplete
                    )

        decodeSpare : Decoder Spare
        decodeSpare =
            Decode.succeed Spare
                |> required "name" string
                |> required "positions" (list string)
    in
    Decode.succeed Event
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing
        |> optional "note" (nullable string) Nothing
        |> required "team_restriction" string
        |> required "age_range" string
        |> optional "sponsor" (nullable decodeSponsor) Nothing
        |> required "starts_on" string
        |> required "ends_on" string
        |> optional "state" decodeEventState EventStateComplete
        |> optional "no_registration_message" (nullable string) Nothing
        |> optional "registration_opens_at" (nullable string) Nothing
        |> optional "registration_closes_at" (nullable string) Nothing
        |> optional "spots_available" (nullable int) Nothing
        |> optional "spots_remaining" (nullable int) Nothing
        |> optional "add_to_cart_url" (nullable string) Nothing
        |> optional "add_to_cart_text" (nullable string) Nothing
        |> optional "total_with_tax" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []
        |> optional "end_scores_enabled" bool False
        |> optional "shot_by_shot_enabled" bool False
        |> optional "number_of_ends" int 10
        |> optional "top_rock" string "red"
        |> optional "bot_rock" string "yellow"
        |> optional "sheet_names" (list string) []
        |> optional "teams" (list decodeTeam) []
        |> optional "registrations" (list decodeRegistration) []
        |> optional "spares" (list decodeSpare) []
        |> optional "stages" (list decodeStage) []
        |> optional "draws" (list decodeDraw) []


decodeTeam : Decoder Team
decodeTeam =
    Decode.succeed Team
        |> required "id" int
        |> required "name" string
        |> required "short_name" string
        |> optional "coach" (nullable string) Nothing
        |> optional "affiliation" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> optional "contact_name" (nullable string) Nothing
        |> optional "contact_email" (nullable string) Nothing
        |> optional "contact_phone" (nullable string) Nothing
        |> optional "image_url" (nullable string) Nothing
        |> optional "lineup" (list decodeTeamCurler) []


decodeTeamCurler : Decoder TeamCurler
decodeTeamCurler =
    let
        decodeTeamPosition : Decoder TeamPosition
        decodeTeamPosition =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "fourth" ->
                                Decode.succeed TeamFourth

                            "third" ->
                                Decode.succeed TeamThird

                            "second" ->
                                Decode.succeed TeamSecond

                            "first" ->
                                Decode.succeed TeamFirst

                            _ ->
                                Decode.succeed TeamAlternate
                    )

        decodeDelivery : Decoder RockDelivery
        decodeDelivery =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "left" ->
                                Decode.succeed RockDeliveryLeft

                            _ ->
                                Decode.succeed RockDeliveryRight
                    )
    in
    Decode.succeed TeamCurler
        |> required "curler_id" int
        |> optional "position" (nullable decodeTeamPosition) Nothing
        |> optional "skip" bool False
        |> required "name" string
        |> optional "delivery" (nullable decodeDelivery) Nothing
        |> optional "club_name" (nullable string) Nothing
        |> optional "club_city" (nullable string) Nothing
        |> optional "photo_url" (nullable string) Nothing
        |> optional "waiver" bool False


decodeRegistration : Decoder Registration
decodeRegistration =
    let
        decodeLineup : Decoder Lineup
        decodeLineup =
            Decode.succeed Lineup
                |> optional "first" (nullable string) Nothing
                |> optional "second" (nullable string) Nothing
                |> optional "third" (nullable string) Nothing
                |> optional "fourth" (nullable string) Nothing
                |> optional "alternate1" (nullable string) Nothing
    in
    Decode.succeed Registration
        |> optional "curler_name" (nullable string) Nothing
        |> optional "team_name" (nullable string) Nothing
        |> optional "skip_name" (nullable string) Nothing
        |> optional "position" (nullable string) Nothing
        |> optional "lineup" (nullable decodeLineup) Nothing


decodeStage : Decoder Stage
decodeStage =
    let
        decodeStageType : Decoder StageType
        decodeStageType =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "bracket" ->
                                Decode.succeed Bracket

                            _ ->
                                Decode.succeed RoundRobin
                    )

        decodeGroup : Decoder Group
        decodeGroup =
            Decode.succeed Group
                |> required "id" int
                |> required "name" string

        decodeStanding : Decoder Standing
        decodeStanding =
            Decode.succeed Standing
                |> required "id" int
                |> required "rank" int
                |> required "played" int
                |> required "wins" int
                |> required "losses" int
                |> optional "ties" int 0
                |> optional "points" float 0.0
    in
    Decode.succeed Stage
        |> required "id" int
        |> required "type" decodeStageType
        |> required "name" string
        |> optional "groups" (nullable (list decodeGroup)) Nothing
        |> optional "games" (list decodeGame) []
        |> optional "standings" (list decodeStanding) []


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" int
        |> required "starts_at" string
        |> required "label" string
        |> optional "attendance" int 0
        |> required "draw_sheets" (list (nullable string))


decodeGame : Decoder Game
decodeGame =
    let
        decodeGameState : Decoder GameState
        decodeGameState =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "active" ->
                                Decode.succeed GameActive

                            "complete" ->
                                Decode.succeed GameComplete

                            _ ->
                                Decode.succeed GamePending
                    )

        decodeGameCoords : Decoder GameCoords
        decodeGameCoords =
            Decode.succeed GameCoords
                |> required "group_id" int
                |> required "row" int
                |> required "col" int

        decodeSide : Decoder Side
        decodeSide =
            let
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

                decodeShot : Decoder Shot
                decodeShot =
                    Decode.succeed Shot
                        |> required "end_number" int
                        |> required "shot_number" int
                        |> optional "curler_id" (nullable int) Nothing
                        |> optional "turn" (nullable string) Nothing
                        |> optional "throw" (nullable string) Nothing
                        |> optional "rating" (nullable string) Nothing
            in
            Decode.succeed Side
                |> optional "team_id" (nullable int) Nothing
                |> optional "top_rock" bool False
                |> optional "first_hammer" bool False
                |> optional "result" decodeSideResult Nothing
                |> optional "score" (nullable int) Nothing
                |> optional "end_scores" (list int) []
                |> optional "winner_id" (nullable string) Nothing
                |> optional "loser_id" (nullable string) Nothing
                |> optional "shots" (list decodeShot) []
    in
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "state" decodeGameState
        |> optional "coords" (nullable decodeGameCoords) Nothing
        |> required "game_positions" (list decodeSide)



-- HELPERS


matchRoute : Maybe String -> Parser (Route -> a) a
matchRoute defaultEventSection =
    Url.Parser.oneOf
        [ Url.Parser.map ItemsRoute Url.Parser.top
        , Url.Parser.map ProductRoute (Url.Parser.s "products" </> Url.Parser.int)
        , Url.Parser.map EventRoute (Url.Parser.s "events" </> Url.Parser.int </> matchNestedEventRoute defaultEventSection)
        ]


matchNestedEventRoute : Maybe String -> Parser (NestedEventRoute -> a) a
matchNestedEventRoute defaultEventSection =
    let
        defaultRoute =
            case defaultEventSection of
                Just section ->
                    case section of
                        "registrations" ->
                            RegistrationsRoute

                        "spares" ->
                            SparesRoute

                        "draws" ->
                            DrawsRoute

                        "stages" ->
                            StagesRoute

                        "teams" ->
                            TeamsRoute

                        "reports" ->
                            ReportsRoute

                        _ ->
                            DetailsRoute

                Nothing ->
                    DetailsRoute
    in
    Url.Parser.oneOf
        [ Url.Parser.map defaultRoute Url.Parser.top
        , Url.Parser.map DetailsRoute (Url.Parser.s "details")
        , Url.Parser.map RegistrationsRoute (Url.Parser.s "registrations")
        , Url.Parser.map SparesRoute (Url.Parser.s "spares")
        , Url.Parser.map DrawsRoute (Url.Parser.s "draws")
        , Url.Parser.map StagesRoute (Url.Parser.s "stages")
        , Url.Parser.map TeamsRoute (Url.Parser.s "teams")
        , Url.Parser.map ReportsRoute (Url.Parser.s "reports")
        , Url.Parser.map DrawRoute (Url.Parser.s "draws" </> Url.Parser.int)
        , Url.Parser.map GameRoute (Url.Parser.s "games" </> Url.Parser.string)
        , Url.Parser.map StageRoute (Url.Parser.s "stages" </> Url.Parser.int)
        , Url.Parser.map TeamRoute (Url.Parser.s "teams" </> Url.Parser.int)
        , Url.Parser.map ReportRoute (Url.Parser.s "reports" </> Url.Parser.string)
        ]


toRoute : Maybe String -> String -> Route
toRoute defaultEventSection hash =
    let
        url =
            let
                fixedHash =
                    hash
                        |> String.replace "#" ""
            in
            { host = "api.curling.io"
            , port_ = Nothing
            , protocol = Url.Https
            , query = Nothing
            , fragment = Nothing
            , path = fixedHash
            }

        parsed =
            Maybe.withDefault ItemsRoute (Url.Parser.parse (matchRoute defaultEventSection) url)
    in
    parsed


drawUrl : Int -> Draw -> String
drawUrl eventId draw =
    "/events/" ++ String.fromInt eventId ++ "/draws/" ++ String.fromInt draw.id


gameUrl : Int -> Game -> String
gameUrl eventId game =
    "/events/" ++ String.fromInt eventId ++ "/games/" ++ game.id


teamUrl : Int -> Int -> String
teamUrl eventId teamId =
    "/events/" ++ String.fromInt eventId ++ "/teams/" ++ String.fromInt teamId


stageUrl : Int -> Stage -> String
stageUrl eventId stage =
    let
        eventIdStr =
            String.fromInt eventId

        stageIdStr =
            String.fromInt stage.id
    in
    "/events/" ++ String.fromInt eventId ++ "/stages/" ++ String.fromInt stage.id


colorNameToRGB : String -> El.Color
colorNameToRGB color =
    case color of
        "red" ->
            El.rgb255 204 0 0

        "yellow" ->
            El.rgb255 204 204 0

        _ ->
            El.rgb255 204 204 0


sideResultToString : List Translation -> Maybe SideResult -> String
sideResultToString translations result =
    translate translations
        (case result of
            Just SideResultWon ->
                "won"

            Just SideResultLost ->
                "lost"

            Just SideResultTied ->
                "tied"

            Just SideResultConceded ->
                "conceded"

            Just SideResultForfeited ->
                "forfeited"

            Just SideResultTimePenalized ->
                "was time penalized"

            Nothing ->
                "unknown"
        )


teamPositionToString : List Translation -> Maybe TeamPosition -> String
teamPositionToString translations position =
    translate translations
        (case position of
            Just TeamFourth ->
                "fourth"

            Just TeamThird ->
                "third"

            Just TeamSecond ->
                "second"

            Just TeamFirst ->
                "first"

            Just TeamAlternate ->
                "alternate"

            Nothing ->
                " "
        )


deliveryToString : List Translation -> Maybe RockDelivery -> String
deliveryToString translations delivery =
    case delivery of
        Just RockDeliveryRight ->
            translate translations "right"

        Just RockDeliveryLeft ->
            translate translations "left"

        Nothing ->
            "-"


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    let
        device =
            El.classifyDevice
                { width = 800
                , height = 600
                }
    in
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            let
                newHash =
                    let
                        eventRouteMaybe =
                            case flags.eventId of
                                Just eventId ->
                                    let
                                        section =
                                            case flags.defaultEventSection of
                                                Nothing ->
                                                    "details"

                                                Just "" ->
                                                    "details"

                                                Just defaultEventSection ->
                                                    defaultEventSection
                                    in
                                    "/events/" ++ String.fromInt eventId ++ "/" ++ section

                                Nothing ->
                                    ""
                    in
                    case flags.hash of
                        Just "" ->
                            eventRouteMaybe

                        Nothing ->
                            eventRouteMaybe

                        Just hash ->
                            hash

                newModel =
                    { flags = flags
                    , hash = newHash
                    , device = device
                    , fullScreen = False
                    , translations = NotAsked
                    , items = NotAsked
                    , itemFilter = ItemFilter 1 0 "" False
                    , product = NotAsked
                    , event = NotAsked
                    , scoringHilight = Nothing
                    , errorMsg = Nothing
                    , reloadIn = timeBetweenReloads
                    }
            in
            ( newModel
            , Cmd.batch
                [ Task.perform InitDevice Browser.Dom.getViewport
                , getTranslations flags
                , Tuple.second (getItemsMaybe newModel newHash False)
                , Tuple.second (getEventMaybe newModel newHash False)
                , Tuple.second (getProductMaybe newModel newHash)
                ]
            )

        Err error ->
            let
                flags =
                    { host = Nothing
                    , hash = Nothing
                    , lang = "en"
                    , apiKey = Nothing
                    , subdomain = Nothing
                    , fullScreenToggle = False
                    , section = LeaguesSection
                    , registration = False
                    , showWaiversForTeams = False
                    , excludeEventSections = []
                    , defaultEventSection = Nothing
                    , eventId = Nothing
                    , theme = defaultTheme
                    , loggedInCurlerIds = []
                    }
            in
            ( { flags = flags
              , hash = ""
              , device = device
              , fullScreen = False
              , translations = NotAsked
              , items = NotAsked
              , itemFilter = ItemFilter 1 0 "" False
              , product = NotAsked
              , event = NotAsked
              , scoringHilight = Nothing
              , errorMsg = Just (Decode.errorToString error)
              , reloadIn = 0
              }
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


baseUrl : Flags -> String
baseUrl { host, lang } =
    let
        devUrl =
            -- Development
            -- "http://api.curling.test:3000/" ++ lang
            "https://api-curlingio.global.ssl.fastly.net/" ++ lang

        -- productionUrl =
        --     -- Production without caching
        --     "https://api.curling.io/" ++ lang
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


clubId : Flags -> String
clubId { apiKey, subdomain } =
    case ( apiKey, subdomain ) of
        ( _, Just subdomain_ ) ->
            subdomain_

        ( Just apiKey_, _ ) ->
            apiKey_

        _ ->
            ""


baseClubUrl : Flags -> String
baseClubUrl flags =
    baseUrl flags ++ "/clubs/" ++ clubId flags ++ "/"


baseClubSubdomainUrl : Flags -> String
baseClubSubdomainUrl flags =
    let
        devUrl =
            -- Development
            "http://" ++ clubId flags ++ ".curling.test:3000/" ++ flags.lang

        productionUrl =
            "https://" ++ clubId flags ++ ".curling.io/" ++ flags.lang
    in
    case flags.host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl

            else
                productionUrl

        Nothing ->
            productionUrl


getItemsMaybe : Model -> String -> Bool -> ( WebData (List Item), Cmd Msg )
getItemsMaybe { flags, itemFilter, items } hash reload =
    case toRoute flags.defaultEventSection hash of
        ItemsRoute ->
            case ( items, reload ) of
                ( Success _, False ) ->
                    -- Already have the items.
                    ( items, Cmd.none )

                _ ->
                    -- On items route and haven't loaded them yet.
                    ( Loading, getItems flags itemFilter )

        _ ->
            -- Not on items route.
            ( items, Cmd.none )


getEventMaybe : Model -> String -> Bool -> ( WebData Event, Cmd Msg )
getEventMaybe { flags, event } hash reload =
    case toRoute flags.defaultEventSection hash of
        EventRoute eventId _ ->
            if reload then
                ( Loading, getEvent flags eventId )

            else
                case event of
                    Success event_ ->
                        if event_.id /= eventId then
                            -- Different event selected, load it.
                            ( Loading, getEvent flags eventId )

                        else
                            -- Already have the event.
                            ( event, Cmd.none )

                    _ ->
                        -- Haven't loaded an event yet.
                        ( Loading, getEvent flags eventId )

        _ ->
            -- Not on event route.
            ( event, Cmd.none )


getProductMaybe : Model -> String -> ( WebData Product, Cmd Msg )
getProductMaybe { flags, product } hash =
    case toRoute flags.defaultEventSection hash of
        ProductRoute productId ->
            case product of
                Success product_ ->
                    if product_.id /= productId then
                        -- Different product selected, load it.
                        ( Loading, getProduct flags productId )

                    else
                        -- Already have the product.
                        ( product, Cmd.none )

                _ ->
                    -- Haven't loaded a product yet.
                    ( Loading, getProduct flags productId )

        _ ->
            -- Not o product route.
            ( product, Cmd.none )


getTranslations : Flags -> Cmd Msg
getTranslations flags =
    let
        url =
            baseUrl flags ++ "/translations"
    in
    RemoteData.Http.get url GotTranslations decodeTranslations


itemsSectionName : ItemsSection -> String
itemsSectionName section =
    case section of
        LeaguesSection ->
            "leagues"

        CompetitionsSection ->
            "competitions"

        ProductsSection ->
            "products"


getItems : Flags -> ItemFilter -> Cmd Msg
getItems flags itemFilter =
    let
        params =
            case ( itemFilter.page, itemFilter.seasonDelta ) of
                ( 0, 0 ) ->
                    ""

                ( 0, seasonDelta ) ->
                    "?occurred=" ++ String.fromInt seasonDelta

                ( page, 0 ) ->
                    "?page=" ++ String.fromInt page

                ( page, seasonDelta ) ->
                    "?page=" ++ String.fromInt page ++ "&occurred=" ++ String.fromInt seasonDelta

        url =
            baseClubUrl flags
                ++ itemsSectionName flags.section
                ++ params
    in
    RemoteData.Http.get url GotItems decodeItems


getEvent : Flags -> Int -> Cmd Msg
getEvent flags id =
    let
        url =
            baseClubUrl flags
                ++ "events/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url GotEvent decodeEvent


getProduct : Flags -> Int -> Cmd Msg
getProduct flags id =
    let
        url =
            baseClubUrl flags
                ++ "products/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url GotProduct decodeProduct


eventSections : List String -> Event -> List String
eventSections excludeEventSections event =
    let
        -- Check if a section is included (not in the explicitly excluded sections list).
        included section =
            List.map String.toLower excludeEventSections
                |> List.member (String.toLower section)
                |> not

        hasData section =
            let
                hasRegistrations =
                    not (List.isEmpty event.registrations)

                hasSpares =
                    not (List.isEmpty event.spares)

                hasDraws =
                    not (List.isEmpty event.draws)

                hasStages =
                    not (List.isEmpty event.stages)

                hasTeams =
                    not (List.isEmpty event.teams)

                hasCompletedGames =
                    List.any (\g -> g.state == GameComplete) (gamesFromStages event.stages)

                hasEndScores =
                    event.endScoresEnabled
            in
            case section of
                "registrations" ->
                    hasRegistrations

                "spares" ->
                    hasSpares

                "draws" ->
                    hasDraws

                "stages" ->
                    hasStages

                "teams" ->
                    hasTeams

                "reports" ->
                    (hasDraws && hasTeams)
                        || hasCompletedGames

                _ ->
                    True
    in
    [ "details", "registrations", "spares", "draws", "stages", "teams", "reports" ]
        |> List.filter included
        |> List.filter hasData


eventSectionForRoute : NestedEventRoute -> String
eventSectionForRoute route =
    case route of
        DetailsRoute ->
            "details"

        RegistrationsRoute ->
            "registrations"

        SparesRoute ->
            "spares"

        DrawsRoute ->
            "draws"

        DrawRoute _ ->
            "draws"

        StagesRoute ->
            "stages"

        StageRoute _ ->
            "stages"

        GameRoute _ ->
            "draws"

        TeamsRoute ->
            "teams"

        TeamRoute _ ->
            "teams"

        ReportsRoute ->
            "reports"

        ReportRoute _ ->
            "reports"


gamesForTeam : List Game -> Team -> List Game
gamesForTeam games team =
    let
        participatedIn sides =
            List.any (\s -> s.teamId == Just team.id) sides
    in
    List.filter (\g -> participatedIn g.sides) games


teamsWithGames : List Team -> List Game -> List Team
teamsWithGames teams games =
    let
        teamHasGames team =
            not (List.isEmpty (gamesForTeam games team))
    in
    List.filter teamHasGames teams


findTeamForSide : List Team -> Side -> Maybe Team
findTeamForSide teams side =
    List.Extra.find (\t -> Just t.id == side.teamId) teams


sheetNameForGame : Event -> Game -> String
sheetNameForGame event game =
    let
        drawHasGame : Game -> Draw -> Bool
        drawHasGame game_ draw =
            List.any (\gameId -> gameId == Just game_.id) draw.drawSheets

        sheetNumber : Game -> Draw -> Maybe Int
        sheetNumber game_ draw =
            let
                matching gameId =
                    gameId == Just game_.id
            in
            List.Extra.findIndex matching draw.drawSheets
    in
    case List.Extra.find (drawHasGame game) event.draws of
        Just draw ->
            case sheetNumber game draw of
                Just index ->
                    List.Extra.getAt index event.sheetNames
                        |> Maybe.withDefault ""

                Nothing ->
                    ""

        Nothing ->
            ""


gameScore : Game -> Maybe ( Int, Int ) -> Maybe String
gameScore game orderByTeamIds =
    let
        sides =
            -- If we passed team ids to order by, use them. Otherwise just use the default side positions.
            case orderByTeamIds of
                Just teamIds ->
                    [ List.Extra.find (\side -> side.teamId == Just (Tuple.first teamIds)) game.sides
                    , List.Extra.find (\side -> side.teamId == Just (Tuple.second teamIds)) game.sides
                    ]
                        |> List.filterMap identity

                Nothing ->
                    game.sides

        sideResults =
            List.map (\s -> s.result) sides
                |> List.filterMap identity

        intScores =
            List.map (\s -> s.score) sides
                |> List.filterMap identity

        strScores =
            List.map String.fromInt intScores

        fromScores =
            case game.state of
                GameComplete ->
                    case ( Maybe.withDefault 0 (List.head intScores), Maybe.withDefault 0 (List.Extra.getAt 1 intScores) ) of
                        ( 0, 0 ) ->
                            -- Display a W if a won, an L if a lost, or a T if they tied.
                            case List.head sideResults of
                                Nothing ->
                                    "-"

                                Just SideResultWon ->
                                    "W"

                                Just SideResultTied ->
                                    "T"

                                _ ->
                                    "L"

                        ( a, b ) ->
                            -- if a > b then
                            --     String.join " > " strScores
                            --
                            -- else if a < b then
                            --     String.join " < " strScores
                            --
                            -- else
                            --     String.join " = " strScores
                            String.join " - " strScores

                _ ->
                    ""
    in
    case fromScores of
        "" ->
            Nothing

        score ->
            Just score


blankEnds : List Game -> Team -> List Int
blankEnds games team =
    let
        -- A blank end is when neither teams gets a point.
        blankEndsForGame game =
            let
                -- We don't know if the team side is the first or second, so build a tuple so we know.
                sides =
                    ( List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                    , List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                    )

                -- We don't care about the event's number of ends, just the max of either side.
                numberOfEnds =
                    List.map (\side -> List.length side.endScores) game.sides
                        |> List.maximum
                        |> Maybe.withDefault 0

                blankEnd : Side -> Side -> Int -> Bool
                blankEnd sideFor sideAgainst endIndex =
                    let
                        endFor =
                            List.Extra.getAt (endIndex - 1) sideFor.endScores

                        endAgainst =
                            List.Extra.getAt (endIndex - 1) sideAgainst.endScores
                    in
                    ( endFor, endAgainst ) == ( Just 0, Just 0 )
            in
            case sides of
                ( Just sideFor, Just sideAgainst ) ->
                    List.range 0 numberOfEnds
                        |> List.filter (blankEnd sideFor sideAgainst)

                _ ->
                    []
    in
    List.map (\game -> blankEndsForGame game) games
        |> List.concat


stolenEnds : Bool -> List Game -> Team -> List Int
stolenEnds for games team =
    -- The for is a bool to indicate for (True) or against (False)
    let
        -- We need to look at each end score, and whether or not they had the hammer.
        stolenEndsForGame game =
            let
                -- We don't know if the team side is the first or second, so build a tuple so we know.
                sides =
                    if for then
                        ( List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                        , List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                        )

                    else
                        ( List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                        , List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                        )

                -- We don't care about the event's number of ends, just the max of either side.
                numberOfEnds =
                    List.map (\side -> List.length side.endScores) game.sides
                        |> List.maximum
                        |> Maybe.withDefault 0

                stolenEnd : Side -> Side -> Int -> Maybe Int
                stolenEnd sideFor sideAgainst endIndex =
                    let
                        hasHammer =
                            if endIndex > 0 then
                                -- Check if they lost the previous end (thus getting hammer for this end)
                                Maybe.withDefault 0 (List.Extra.getAt (endIndex - 1) sideAgainst.endScores) > 0

                            else
                                -- First end and first hammer?
                                sideFor.firstHammer == True
                    in
                    if hasHammer then
                        Nothing

                    else
                        List.Extra.getAt endIndex sideFor.endScores
            in
            case sides of
                ( Just sideFor, Just sideAgainst ) ->
                    List.range 0 numberOfEnds
                        |> List.map (stolenEnd sideFor sideAgainst)
                        |> List.filterMap identity
                        |> List.filter (\e -> e > 0)

                _ ->
                    []
    in
    List.map (\game -> stolenEndsForGame game) games
        |> List.concat


roundTwoDecimal : Float -> Float
roundTwoDecimal num =
    toFloat (round (num * 100)) / 100


gamesFromStages : List Stage -> List Game
gamesFromStages stages =
    List.map .games stages
        |> List.concat


findGameById : List Stage -> String -> Maybe Game
findGameById stages id =
    gamesFromStages stages
        |> List.Extra.find (\g -> g.id == id)


drawWithGameId : List Draw -> String -> Maybe Draw
drawWithGameId draws id =
    let
        hasGame draw =
            let
                matching gameId =
                    gameId == Just id
            in
            List.any matching draw.drawSheets
    in
    List.Extra.find hasGame draws


reloadEnabled { flags, hash, event } =
    -- Only if we're on an event, the event is active, end scores are enabled, and we're on a route / screen that is meaningfull to reload.
    case toRoute flags.defaultEventSection hash of
        EventRoute _ nestedRoute ->
            case event of
                Success event_ ->
                    (event_.state == EventStateActive)
                        && event_.endScoresEnabled
                        && not (List.member nestedRoute [ DetailsRoute, RegistrationsRoute, SparesRoute, TeamsRoute, ReportsRoute ])
                        && not (String.contains "/reports" hash)

                _ ->
                    False

        _ ->
            False



-- UPDATE


type Msg
    = NoOp
    | InitDevice Browser.Dom.Viewport
    | Tick Time.Posix
    | SetDevice Int Int
    | NavigateTo String
    | ToggleFullScreen
    | HashChanged Bool String
    | Reload
    | GotTranslations (WebData (List Translation))
    | GotItems (WebData (List Item))
    | IncrementPageBy Int
    | ToggleSeasonSearch
    | UpdateSearch String
    | UpdateSeasonDelta Int
    | NavigateOut String
    | GotEvent (WebData Event)
    | GotProduct (WebData Product)
    | ToggleScoringHilight ScoringHilight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InitDevice { viewport } ->
            let
                device =
                    El.classifyDevice
                        { width = round viewport.width
                        , height = round viewport.height
                        }
            in
            ( { model | device = device }, Cmd.none )

        Tick _ ->
            let
                newReloadIn =
                    max 0 (model.reloadIn - 1)
            in
            if reloadEnabled model then
                -- We only want to decrement the counter or do the reload when the event supports it and we're on a relevant reload screen.
                if newReloadIn == 0 then
                    update (HashChanged True model.hash) { model | reloadIn = timeBetweenReloads }

                else
                    ( { model | reloadIn = newReloadIn }, Cmd.none )

            else
                ( model, Cmd.none )

        SetDevice width height ->
            let
                device =
                    El.classifyDevice
                        { width = width
                        , height = height
                        }
            in
            ( { model | device = device }, Cmd.none )

        NavigateTo newHash ->
            ( model, navigateTo newHash )

        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        HashChanged reload hash ->
            let
                ( translations, translationsCmd ) =
                    case model.translations of
                        Success _ ->
                            ( model.translations, Cmd.none )

                        _ ->
                            ( Loading, getTranslations model.flags )

                ( items, itemsCmd ) =
                    getItemsMaybe model hash reload

                ( event, eventCmd ) =
                    getEventMaybe model hash reload

                ( product, productCmd ) =
                    getProductMaybe model hash
            in
            ( { model
                | hash = hash
                , translations = translations
                , items = items
                , product = product
                , event = event
              }
            , Cmd.batch
                [ translationsCmd
                , itemsCmd
                , productCmd
                , eventCmd
                ]
            )

        Reload ->
            update (HashChanged True model.hash) { model | reloadIn = timeBetweenReloads }

        GotTranslations response ->
            ( { model | translations = response, errorMsg = Nothing }, Cmd.none )

        GotItems response ->
            ( { model | items = response, errorMsg = Nothing }, Cmd.none )

        IncrementPageBy num ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | page = itemFilter.page + num }

                updatedModel =
                    { model | itemFilter = updatedItemFilter model.itemFilter }
            in
            ( updatedModel, getItems model.flags updatedModel.itemFilter )

        UpdateSearch val ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | search = String.toLower val }
            in
            ( { model | itemFilter = updatedItemFilter model.itemFilter }, Cmd.none )

        ToggleSeasonSearch ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | seasonSearchOpen = not itemFilter.seasonSearchOpen }
            in
            ( { model | itemFilter = updatedItemFilter model.itemFilter }, Cmd.none )

        UpdateSeasonDelta seasonDelta ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | seasonDelta = seasonDelta }

                updatedModel =
                    { model | itemFilter = updatedItemFilter model.itemFilter }
            in
            ( updatedModel, getItems model.flags updatedModel.itemFilter )

        NavigateOut url ->
            ( model, Navigation.load url )

        GotEvent response ->
            ( { model | event = response, reloadIn = timeBetweenReloads }
            , Cmd.none
            )

        GotProduct response ->
            ( { model | product = response }
            , Cmd.none
            )

        ToggleScoringHilight scoringHilight ->
            ( { model
                | scoringHilight =
                    if model.scoringHilight == Just scoringHilight then
                        Nothing

                    else
                        Just scoringHilight
              }
            , Cmd.none
            )



-- VIEWS


viewButtonPrimary theme content msg =
    button
        [ Background.color theme.primary

        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
        , El.htmlAttribute (attribute "style" "color: white !important")
        , El.paddingXY 12 10
        , Border.rounded 4
        , El.focused [ Background.color theme.primary ]
        ]
        { onPress = Just msg
        , label = text content
        }


view : Model -> Html Msg
view model =
    let
        viewMain =
            row
                [ El.htmlAttribute (class "cio__main")
                , El.width
                    (if model.fullScreen then
                        El.fill

                     else
                        El.fill
                            |> El.maximum
                                (case model.device.class of
                                    El.Phone ->
                                        599

                                    El.Tablet ->
                                        1199

                                    El.Desktop ->
                                        1200

                                    El.BigDesktop ->
                                        1920
                                )
                    )
                , El.centerX
                , El.scrollbarX
                , El.clipY
                , El.inFront
                    (row [ El.alignRight, El.spacing 10, El.paddingXY 0 10 ]
                        [ el [ El.alignTop ] (viewReloadButton theme model)
                        , if model.flags.fullScreenToggle then
                            el [] (viewFullScreenButton theme model.fullScreen)

                          else
                            El.none
                        ]
                    )
                ]
                [ case model.errorMsg of
                    Just errorMsg ->
                        viewNotReady model.fullScreen errorMsg

                    Nothing ->
                        case model.translations of
                            Success translations ->
                                viewRoute theme translations model

                            Failure error ->
                                viewFetchError model (errorMessage error)

                            _ ->
                                viewNotReady model.fullScreen "Loading..."
                ]

        theme =
            model.flags.theme
    in
    El.layout
        [ Font.size 16
        , Font.color theme.defaultText
        , El.width El.fill
        , El.padding 10
        , El.htmlAttribute (style "z-index" "400")
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
        , El.htmlAttribute (class "cio__container")
        , El.inFront
            (if model.fullScreen then
                el [ El.width El.fill, El.height El.fill, El.padding 20, El.scrollbarY, Background.color theme.white ] viewMain

             else
                El.none
            )
        ]
    <|
        if model.fullScreen then
            El.none

        else
            viewMain


viewRoute : Theme -> List Translation -> Model -> Element Msg
viewRoute theme translations model =
    let
        viewLoading =
            viewNotReady model.fullScreen "Loading..."
    in
    case toRoute model.flags.defaultEventSection model.hash of
        ItemsRoute ->
            case model.items of
                Success items ->
                    viewItems theme translations model items

                Failure error ->
                    viewFetchError model (errorMessage error)

                _ ->
                    viewLoading

        ProductRoute id ->
            case model.product of
                Success product ->
                    viewProduct theme translations model.fullScreen product

                Failure error ->
                    viewFetchError model (errorMessage error)

                _ ->
                    viewLoading

        EventRoute id nestedRoute ->
            case model.event of
                Success event ->
                    viewEvent theme translations model nestedRoute event

                Failure error ->
                    viewFetchError model (errorMessage error)

                _ ->
                    viewLoading


viewReloadButton : Theme -> Model -> Element Msg
viewReloadButton theme model =
    if reloadEnabled model && model.device.class /= El.Phone then
        el
            [ El.paddingXY 8 0
            , El.alignTop
            , Font.size 12
            , Font.color theme.secondary
            , El.htmlAttribute (class "cio__reload_button")
            ]
            (text ("Refresh in " ++ String.fromInt model.reloadIn ++ "s"))

    else
        El.none


viewFullScreenButton : Theme -> Bool -> Element Msg
viewFullScreenButton theme fullScreen =
    button [ El.focused [ Background.color theme.transparent ] ]
        { onPress = Just ToggleFullScreen
        , label =
            if fullScreen then
                El.html svgExitFullScreen

            else
                El.html svgFullScreen
        }


viewNotReady : Bool -> String -> Element Msg
viewNotReady fullScreen message =
    el [ El.htmlAttribute (class "cio__not_ready") ] (text message)


viewFetchError : Model -> String -> Element Msg
viewFetchError { flags, fullScreen, hash } message =
    row
        [ El.htmlAttribute (class "cio__fetch_error") ]
        [ column [ El.spacing 10 ]
            [ el [] (text message)
            , viewButtonPrimary flags.theme "Reload" Reload
            ]
        ]


viewItems : Theme -> List Translation -> Model -> List Item -> Element Msg
viewItems theme translations { flags, fullScreen, itemFilter } items =
    let
        viewPaging =
            let
                viewPageButton content msg =
                    button
                        [ Font.size 14
                        , El.padding 8
                        , Border.rounded 3

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important")
                        , Background.color theme.primary
                        , El.focused [ Background.color theme.primary ]
                        ]
                        { onPress = Just msg
                        , label = text content
                        }
            in
            row [ El.htmlAttribute (class "cio__paging") ]
                [ if List.length items >= 10 then
                    viewPageButton "Next >" (IncrementPageBy 1)

                  else
                    El.none
                , if itemFilter.page > 1 then
                    viewPageButton "< Previous" (IncrementPageBy -1)

                  else
                    El.none
                ]

        viewSeasonDropDown =
            let
                times =
                    [ ( 1, "next_season" )
                    , ( 0, "this_season" )
                    , ( -1, "Last_season" )
                    , ( -2, "two_seasons_ago" )
                    , ( -3, "three_seasons_ago" )
                    ]

                seasonOption ( delta, label ) =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateSeasonDelta delta)
                        , if delta == itemFilter.seasonDelta then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations label))

                seasonOptions =
                    if itemFilter.seasonSearchOpen then
                        column
                            [ El.width El.fill
                            , Border.width 1
                            , Border.color theme.grey
                            , Background.color theme.white
                            ]
                            (List.map seasonOption times)

                    else
                        El.none

                seasonSelected =
                    List.filter (\time -> Tuple.first time == itemFilter.seasonDelta) times
                        |> List.map Tuple.second
                        |> List.head
                        |> Maybe.withDefault "this_season"
                        |> translate translations
            in
            row
                [ El.width (El.px 184)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleSeasonSearch
                , El.below seasonOptions
                , El.htmlAttribute (class "cio__season_dropdown")
                ]
                [ el [] (text seasonSelected)
                , el [ El.alignRight ]
                    (text
                        (if itemFilter.seasonSearchOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        filteredItems =
            case String.trim itemFilter.search of
                "" ->
                    items

                _ ->
                    let
                        matches item =
                            String.contains itemFilter.search (String.toLower item.name)
                                || (case item.location of
                                        Just location ->
                                            String.contains itemFilter.search (String.toLower location)

                                        Nothing ->
                                            False
                                   )
                    in
                    List.filter matches items
    in
    column [ El.spacing 10, El.width El.fill, El.height (El.fill |> El.minimum 210) ]
        [ row [ El.spacing 20 ]
            [ Input.text
                [ El.width (El.px 200)
                , Border.width 1
                , Border.color theme.grey
                , El.padding 10
                , El.htmlAttribute (class "cio__search")
                ]
                { placeholder = Just (Input.placeholder [] (text (translate translations "search")))
                , text = itemFilter.search
                , onChange = UpdateSearch
                , label = Input.labelHidden ""
                }
            , viewSeasonDropDown
            ]
        , if List.isEmpty filteredItems then
            el [ El.padding 10 ] (text "No results found.")

          else
            let
                viewItemName item =
                    let
                        newPath =
                            case flags.section of
                                ProductsSection ->
                                    "/products/" ++ String.fromInt item.id

                                _ ->
                                    "/events/" ++ String.fromInt item.id
                    in
                    column
                        [ El.spacingXY 0 5
                        , El.paddingXY 10 15
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_name")
                        ]
                        [ el [ Font.color theme.primary, El.pointer, Events.onClick (NavigateTo newPath) ] (text item.name)
                        , el [ Font.size 13 ] (text (Maybe.withDefault " " item.summary))
                        ]

                viewItemCell content =
                    el
                        [ El.paddingXY 10 24
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_cell")
                        ]
                        content

                viewItemOccursOn item =
                    viewItemCell (el [ El.centerX, El.htmlAttribute (class "cio__item_occurs_on") ] (text (Maybe.withDefault " " item.occursOn)))

                viewItemPrice item =
                    if flags.registration then
                        viewItemCell (el [ El.alignRight, El.htmlAttribute (class "cio__item_price") ] (text (Maybe.withDefault " " item.price)))

                    else
                        viewItemCell (text " ")

                viewItemRegister item =
                    if flags.registration then
                        case item.noRegistrationMessage of
                            Just msg ->
                                viewItemCell (el [ El.alignRight, El.htmlAttribute (class "cio__item_register") ] (text msg))

                            Nothing ->
                                case ( item.addToCartUrl, item.addToCartText ) of
                                    ( Just addToCartUrl, Just addToCartText ) ->
                                        el
                                            [ El.paddingXY 10 17
                                            , Border.color theme.grey
                                            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                                            , El.htmlAttribute (class "cio__item_register")
                                            ]
                                            (button
                                                [ Background.color theme.primary

                                                -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                                                , El.htmlAttribute (attribute "style" "color: white !important")
                                                , Font.size 14
                                                , El.alignRight
                                                , El.padding 8
                                                , Border.rounded 3
                                                , El.focused [ Background.color theme.primary ]
                                                ]
                                                { onPress = Just (NavigateOut addToCartUrl)
                                                , label = text addToCartText
                                                }
                                            )

                                    _ ->
                                        viewItemCell (text " ")

                    else
                        viewItemCell (text " ")
            in
            row [ El.width El.fill ]
                [ El.table [ El.spacingXY 0 15, El.htmlAttribute (class "cio__items_table") ]
                    { data = filteredItems
                    , columns =
                        [ { header = El.none
                          , width = El.fill
                          , view = viewItemName
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemOccursOn
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemPrice
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemRegister
                          }
                        ]
                    }
                ]
        , viewPaging
        ]


viewNoDataForRoute : List Translation -> Element Msg
viewNoDataForRoute translations =
    el [] (text (translate translations "no_data_for_route"))


viewSponsor : Sponsor -> Element Msg
viewSponsor sponsor =
    column [ El.spacing 10, El.width El.fill, El.alignTop, El.htmlAttribute (class "cio__sponsor") ]
        [ case sponsor.url of
            Just url ->
                el [ El.width El.fill, El.pointer, Events.onClick (NavigateTo url) ]
                    (El.image [ El.alignRight ] { src = sponsor.logoUrl, description = Maybe.withDefault "" sponsor.name })

            Nothing ->
                El.image [] { src = sponsor.logoUrl, description = Maybe.withDefault "" sponsor.name }
        , case sponsor.name of
            Just name ->
                el [ El.alignRight ] (text name)

            Nothing ->
                El.none
        ]


viewProduct : Theme -> List Translation -> Bool -> Product -> Element Msg
viewProduct theme translations fullScreen product =
    row
        [ El.spacing 20
        , El.width El.fill
        , El.htmlAttribute (class "cio__product")
        ]
        [ column
            [ El.spacing 20
            , El.width El.fill
            , El.height El.fill
            , El.alignTop
            ]
            [ el [ Font.size 28 ] (text product.name)
            , case ( product.description, product.summary ) of
                ( Just description, _ ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_description") ] [ text description ]

                ( _, Just summary ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_summary") ] [ text summary ]

                ( Nothing, Nothing ) ->
                    El.none
            , case product.totalWithTax of
                Just totalWithTax ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__product_total") ]
                        [ el [ Font.bold ] (text (translate translations "total_with_tax"))
                        , el [] (text totalWithTax)
                        ]

                _ ->
                    El.none
            , case ( product.addToCartUrl, product.addToCartText ) of
                ( Just addToCartUrl, Just addToCartText ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_add_to_cart") ]
                        [ viewButtonPrimary theme addToCartText (NavigateOut addToCartUrl)
                        ]

                _ ->
                    El.none
            , if not (List.isEmpty product.potentialDiscounts) then
                column [ El.spacing 5, El.htmlAttribute (class "cio__product_discounts") ]
                    [ el [ Font.bold ] (text (translate translations "potential_discounts"))
                    , column [] (List.map (\d -> el [] (text d)) product.potentialDiscounts)
                    ]

              else
                El.none
            ]
        , case product.sponsor of
            Just sponsor ->
                viewSponsor sponsor

            Nothing ->
                El.none
        ]


viewEvent : Theme -> List Translation -> Model -> NestedEventRoute -> Event -> Element Msg
viewEvent theme translations { flags, device, scoringHilight, fullScreen } nestedRoute event =
    let
        viewNavItem eventSection =
            let
                isActiveRoute =
                    -- TODO: This needs a bit of work. I don't like the string pattern matching, would prefer patterning on toRoute result.
                    eventSection == eventSectionForRoute nestedRoute

                newPath =
                    "/events/" ++ String.fromInt event.id ++ "/" ++ eventSection
            in
            el [ El.htmlAttribute (class "cio__event_nav_item") ]
                (if isActiveRoute then
                    button
                        [ El.paddingXY 16 12
                        , Border.rounded 4
                        , Background.color theme.primary

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important")
                        , El.focused [ Background.color theme.primary ]
                        ]
                        { onPress = Just (NavigateTo newPath)
                        , label = text (translate translations eventSection)
                        }

                 else
                    button
                        [ El.paddingXY 16 12
                        , Font.color theme.primary
                        , Border.rounded 4
                        , El.focused [ Background.color theme.transparent ]
                        ]
                        { onPress = Just (NavigateTo newPath)
                        , label = text (translate translations eventSection)
                        }
                )
    in
    column
        [ El.width El.fill
        , El.height El.fill
        , El.spacing 20
        , El.htmlAttribute (class "cio__event")
        ]
        [ el [ Font.size 28, El.width El.fill, Font.medium, El.htmlAttribute (class "cio__event_name") ] (text event.name)
        , El.wrappedRow [ El.width El.fill, El.htmlAttribute (class "cio__event_nav") ]
            (List.map viewNavItem (eventSections flags.excludeEventSections event)
                ++ (if flags.eventId == Nothing then
                        [ el [ El.alignRight ]
                            (button
                                [ El.padding 16
                                , Font.color theme.primary
                                , Border.rounded 4
                                , El.focused [ Background.color theme.white ]
                                ]
                                { onPress = Just (NavigateTo "/events")
                                , label = text (translate translations (itemsSectionName flags.section) ++ " »")
                                }
                            )
                        ]

                    else
                        []
                   )
            )
        , case nestedRoute of
            DetailsRoute ->
                viewDetails theme device translations event

            RegistrationsRoute ->
                viewRegistrations theme translations event.registrations

            SparesRoute ->
                viewSpares theme translations flags event.id event.spares

            DrawsRoute ->
                viewDraws theme translations scoringHilight event

            DrawRoute drawId ->
                case List.Extra.find (\d -> d.id == drawId) event.draws of
                    Just draw ->
                        viewDraw theme translations scoringHilight event draw

                    Nothing ->
                        viewNoDataForRoute translations

            GameRoute gameId ->
                let
                    findDraw =
                        drawWithGameId event.draws gameId

                    findGame =
                        findGameById event.stages gameId
                in
                case ( findDraw, findGame ) of
                    ( Just draw, Just game ) ->
                        let
                            sheetLabel =
                                sheetNameForGame event game
                        in
                        viewGame theme translations scoringHilight event sheetLabel True draw game

                    _ ->
                        viewNoDataForRoute translations

            StagesRoute ->
                case List.head event.stages of
                    Just stage ->
                        viewStages theme device translations event stage

                    Nothing ->
                        viewNoDataForRoute translations

            StageRoute id ->
                case List.Extra.find (\s -> s.id == id) event.stages of
                    Just stage ->
                        viewStages theme device translations event stage

                    Nothing ->
                        viewNoDataForRoute translations

            TeamsRoute ->
                viewTeams theme translations event

            TeamRoute id ->
                case List.Extra.find (\t -> t.id == id) event.teams of
                    Just team ->
                        viewTeam theme translations flags event team

                    Nothing ->
                        viewNoDataForRoute translations

            ReportsRoute ->
                viewReports theme translations event

            ReportRoute report ->
                viewReport theme translations scoringHilight event report
        ]


viewDetails : Theme -> Device -> List Translation -> Event -> Element Msg
viewDetails theme device translations event =
    row [ El.spacing 20, El.htmlAttribute (class "cio__event_details") ]
        [ column [ El.spacing 20, El.width El.fill, El.alignTop ]
            [ case ( event.description, event.summary ) of
                ( Just description, _ ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_description") ] [ El.html (Markdown.toHtml [] description) ]

                ( _, Just summary ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_summary") ] [ text summary ]

                ( Nothing, Nothing ) ->
                    El.none
            , case event.totalWithTax of
                Just totalWithTax ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__event_total") ]
                        [ el [ Font.bold ] (text (translate translations "total_with_tax"))
                        , el [] (text totalWithTax)
                        ]

                _ ->
                    El.none
            , case ( event.addToCartUrl, event.addToCartText ) of
                ( Just addToCartUrl, Just addToCartText ) ->
                    El.paragraph [ El.paddingEach { top = 10, right = 0, bottom = 20, left = 0 }, El.htmlAttribute (class "cio__event_add_to_cart") ]
                        [ viewButtonPrimary theme addToCartText (NavigateOut addToCartUrl)
                        ]

                _ ->
                    El.none
            , row [ El.width El.fill, El.spacing 20 ]
                [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_starts_on") ]
                    [ el [ Font.bold ] (text (translate translations "starts_on"))
                    , el [] (text event.startsOn)
                    ]
                , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_ends_on") ]
                    [ el [ Font.bold ] (text (translate translations "ends_on"))
                    , el [] (text event.endsOn)
                    ]
                ]
            , row [ El.width El.fill, El.spacing 20 ]
                [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_opens_at") ]
                    [ el [ Font.bold ] (text (translate translations "registration_opens_at"))
                    , el [] (text (Maybe.withDefault "" event.registrationOpensAt))
                    ]
                , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_closes_at") ]
                    [ el [ Font.bold ] (text (translate translations "registration_closes_at"))
                    , el [] (text (Maybe.withDefault "" event.registrationClosesAt))
                    ]
                ]
            , row [ El.width El.fill, El.spacing 20 ]
                [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_team_restriction") ]
                    [ el [ Font.bold ] (text (translate translations "team_restriction"))
                    , el [] (text event.teamRestriction)
                    ]
                , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_age_range") ]
                    [ el [ Font.bold ] (text (translate translations "age_range"))
                    , el [] (text event.ageRange)
                    ]
                ]
            , row [ El.width El.fill, El.spacing 20 ]
                [ case ( event.spotsAvailable, event.spotsRemaining ) of
                    ( Just spotsAvailable, Just spotsRemaining ) ->
                        column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_spots_available") ]
                            [ el [ Font.bold ] (text (translate translations "spots_available"))
                            , el [] (text (String.fromInt spotsRemaining ++ " / " ++ String.fromInt spotsAvailable))
                            ]

                    _ ->
                        El.none
                , if not (List.isEmpty event.potentialDiscounts) then
                    column [ El.width El.fill, El.spacing 5, El.htmlAttribute (class "cio__event_discounts") ]
                        [ el [ Font.bold ] (text (translate translations "potential_discounts"))
                        , column [ El.spacing 5, El.paddingXY 4 5 ] (List.map (\d -> el [] (text ("• " ++ d))) event.potentialDiscounts)
                        ]

                  else
                    El.none
                ]
            ]
        , case event.sponsor of
            Just sponsor ->
                if device.class == El.Phone then
                    El.none

                else
                    viewSponsor sponsor

            Nothing ->
                El.none
        ]


viewRegistrations : Theme -> List Translation -> List Registration -> Element Msg
viewRegistrations theme translations registrations =
    let
        hasCurlers =
            List.any (\r -> r.curlerName /= Nothing) registrations

        hasTeamNames =
            List.any (\r -> r.teamName /= Nothing) registrations

        hasSkipNames =
            List.any (\r -> r.skipName /= Nothing) registrations

        hasPositions =
            List.any (\r -> r.position /= Nothing) registrations

        hasLineups =
            List.any (\r -> r.lineup /= Nothing) registrations

        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.padding 12
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.padding 12
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                (text content)

        curlerColumn =
            if hasCurlers then
                Just
                    { header = tableHeader "curler"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.curlerName)
                    }

            else
                Nothing

        teamColumn =
            if hasTeamNames then
                Just
                    { header = tableHeader "team"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.teamName)
                    }

            else
                Nothing

        skipColumn =
            if hasSkipNames then
                Just
                    { header = tableHeader "skip"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.skipName)
                    }

            else
                Nothing

        positionColumn =
            if hasPositions then
                Just
                    { header = tableHeader "position"
                    , width = El.fill
                    , view =
                        \i reg ->
                            tableCell i
                                (case reg.position of
                                    Just pos ->
                                        translate translations pos

                                    Nothing ->
                                        "-"
                                )
                    }

            else
                Nothing

        lineupColumn =
            if hasLineups then
                Just
                    { header = tableHeader "lineup"
                    , width = El.fill
                    , view =
                        \i reg ->
                            tableCell i
                                (case reg.lineup of
                                    Just lineup ->
                                        [ lineup.first
                                        , lineup.second
                                        , lineup.third
                                        , lineup.fourth
                                        , lineup.alternate
                                        ]
                                            |> List.filterMap identity
                                            |> String.join ", "

                                    Nothing ->
                                        "-"
                                )
                    }

            else
                Nothing

        tableColumns =
            List.filterMap identity [ curlerColumn, teamColumn, skipColumn, positionColumn, lineupColumn ]
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_registrations") ]
        (if List.isEmpty registrations then
            El.paragraph [] [ text (translate translations "no_registrations") ]

         else
            El.indexedTable [ El.htmlAttribute (class "cio__event_registrations_table") ]
                { data = registrations
                , columns = tableColumns
                }
        )


viewSpares : Theme -> List Translation -> Flags -> Int -> List Spare -> Element Msg
viewSpares theme translations flags eventId spares =
    let
        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.padding 12
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.padding 12
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                (text content)

        nameColumn =
            { header = tableHeader "curler"
            , width = El.fill
            , view = \i spare -> tableCell i spare.name
            }

        positionsColumn =
            { header = tableHeader "position"
            , width = El.fill
            , view =
                \i spare ->
                    tableCell i
                        (if List.isEmpty spare.positions then
                            "-"

                         else
                            List.map (\pos -> translate translations pos) spare.positions
                                |> String.join ", "
                        )
            }
    in
    column [ El.width El.fill, El.spacing 30, El.htmlAttribute (class "cio__event_spares") ]
        [ if List.isEmpty spares then
            El.paragraph [] [ text (translate translations "no_spares") ]

          else
            El.indexedTable [ El.htmlAttribute (class "cio__event_spares_table") ]
                { data = spares
                , columns = [ nameColumn, positionsColumn ]
                }
        , button
            [ Font.color theme.primary, El.focused [ Background.color theme.white ] ]
            { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/events/" ++ String.fromInt eventId ++ "/spares"))
            , label = text (translate translations "members_login_to_see_contact_info")
            }
        ]


viewDraws : Theme -> List Translation -> Maybe ScoringHilight -> Event -> Element Msg
viewDraws theme translations scoringHilight event =
    let
        isDrawActive : Draw -> Bool
        isDrawActive draw =
            let
                findGame gameId =
                    gamesFromStages event.stages
                        |> List.Extra.find (\g -> Just g.id == gameId)

                activeGame : Game -> Bool
                activeGame game =
                    List.any (\g -> g.id == game.id && g.state == GameActive) (gamesFromStages event.stages)
            in
            -- Are there any games in the draw that are active
            List.map findGame draw.drawSheets
                |> List.filterMap identity
                |> List.filter activeGame
                |> List.isEmpty
                |> not

        drawLink : Draw -> String -> Element Msg
        drawLink draw label =
            if event.endScoresEnabled then
                button
                    [ Font.color theme.primary, El.focused [ Background.color theme.white ] ]
                    { onPress = Just (NavigateTo (drawUrl event.id draw))
                    , label = text label
                    }

            else
                text label

        gameLink : Game -> Element Msg
        gameLink game =
            let
                gameNameWithResult =
                    case game.state of
                        GameComplete ->
                            let
                                teamNameForSide side =
                                    findTeamForSide event.teams side
                                        |> Maybe.map .shortName

                                winningSide =
                                    List.Extra.find (\s -> s.result == Just SideResultWon) game.sides

                                losingSide =
                                    case winningSide of
                                        Just winningSide_ ->
                                            List.Extra.find (\s -> s.teamId /= winningSide_.teamId) game.sides

                                        Nothing ->
                                            Nothing

                                tied =
                                    List.any (\s -> s.result == Just SideResultTied) game.sides

                                sortedTeamNames =
                                    if tied then
                                        List.map teamNameForSide game.sides
                                            |> List.filterMap identity

                                    else
                                        [ winningSide, losingSide ]
                                            |> List.filterMap identity
                                            |> List.map teamNameForSide
                                            |> List.filterMap identity
                            in
                            String.join
                                (String.toLower
                                    (if tied then
                                        " " ++ translate translations "tied" ++ " "

                                     else
                                        " " ++ translate translations "defeated" ++ " "
                                    )
                                )
                                sortedTeamNames

                        _ ->
                            game.name
            in
            if event.endScoresEnabled then
                button
                    [ Font.color theme.primary
                    , El.focused [ Background.color theme.white ]
                    , case game.state of
                        GameActive ->
                            Font.bold

                        _ ->
                            Font.regular
                    ]
                    { onPress = Just (NavigateTo (gameUrl event.id game))
                    , label = text game.name
                    }

            else
                el [] (text game.name)

        tableColumns =
            let
                hasAttendance =
                    List.any (\d -> d.attendance > 0) event.draws

                tableHeader align content =
                    let
                        contentId =
                            String.replace " " "_" content
                                |> String.toLower
                    in
                    row
                        [ Font.bold
                        , El.paddingXY 12 16
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class ("cio__event_draws_header cio__event_draws_header_" ++ contentId))
                        , Border.color theme.grey
                        ]
                        [ el [ align ] (text (translate translations content)) ]

                tableCell align isActive content =
                    row
                        [ El.paddingXY 12 16
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        , El.htmlAttribute (class "cio__event_draws_cell")
                        , if isActive then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        [ el [ align ] content ]

                labelColumn =
                    Just
                        { header = tableHeader El.alignLeft "draw"
                        , width = El.px 65
                        , view = \draw -> tableCell El.alignLeft (isDrawActive draw) (drawLink draw draw.label)
                        }

                startsAtColumn =
                    Just
                        { header = tableHeader El.alignLeft "starts_at"
                        , width = El.px 220
                        , view = \draw -> tableCell El.alignLeft (isDrawActive draw) (drawLink draw draw.startsAt)
                        }

                attendanceColumn =
                    if hasAttendance then
                        Just
                            { header = tableHeader El.alignLeft "attendance"
                            , width = El.px 65
                            , view =
                                \draw ->
                                    tableCell El.alignLeft (isDrawActive draw) (text (String.fromInt draw.attendance))
                            }

                    else
                        Nothing

                sheetColumn columnIndex sheetName =
                    Just
                        { header = tableHeader El.centerX sheetName
                        , width = El.fill
                        , view =
                            \draw ->
                                tableCell El.centerX
                                    (isDrawActive draw)
                                    (case List.Extra.getAt columnIndex draw.drawSheets of
                                        Just (Just gameId) ->
                                            case List.Extra.find (\g -> g.id == gameId) (gamesFromStages event.stages) of
                                                Just game ->
                                                    gameLink game

                                                Nothing ->
                                                    text "-"

                                        _ ->
                                            text "-"
                                    )
                        }
            in
            ([ labelColumn, startsAtColumn ] ++ List.indexedMap sheetColumn event.sheetNames ++ [ attendanceColumn ])
                |> List.filterMap identity
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_draws") ]
        (if List.isEmpty event.draws then
            El.paragraph [] [ text (translate translations "no_draws") ]

         else
            El.table [ El.htmlAttribute (class "cio__event_draws_table") ]
                { data = event.draws
                , columns = tableColumns
                }
        )


viewTeams : Theme -> List Translation -> Event -> Element Msg
viewTeams theme translations event =
    let
        hasCoaches =
            List.any (\t -> t.coach /= Nothing) event.teams

        hasAffiliations =
            List.any (\t -> t.affiliation /= Nothing) event.teams

        hasLocations =
            List.any (\t -> t.location /= Nothing) event.teams

        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.paddingXY 12 16
                , El.htmlAttribute (class "cio__event_teams_header")
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.paddingXY 12 16
                , El.htmlAttribute (class "cio__event_teams_cell")
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                content

        teamColumn =
            Just
                { header = tableHeader "team"
                , width = El.fill
                , view =
                    \i team ->
                        tableCell i
                            (button
                                [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (teamUrl event.id team.id))
                                , label = text team.name
                                }
                            )
                }

        coachColumn =
            if hasCoaches then
                Just
                    { header = tableHeader "coach"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.coach))
                    }

            else
                Nothing

        affiliationColumn =
            if hasAffiliations then
                Just
                    { header = tableHeader "affiliation"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.affiliation))
                    }

            else
                Nothing

        locationColumn =
            if hasLocations then
                Just
                    { header = tableHeader "location"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.location))
                    }

            else
                Nothing

        tableColumns =
            List.filterMap identity [ teamColumn, coachColumn, affiliationColumn, locationColumn ]
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_teams") ]
        (if List.isEmpty event.teams then
            El.paragraph [] [ text (translate translations "no_teams") ]

         else
            El.indexedTable [ El.htmlAttribute (class "cio__event_teams_table") ]
                { data = event.teams
                , columns = tableColumns
                }
        )


viewStages : Theme -> El.Device -> List Translation -> Event -> Stage -> Element Msg
viewStages theme device translations event onStage =
    let
        viewStageLink stage =
            button
                [ El.paddingXY 18 10
                , El.focused [ Background.color theme.transparent ]
                , Border.color theme.grey
                , Font.color
                    (if stage.id == onStage.id then
                        theme.defaultText

                     else
                        theme.primary
                    )
                , Border.widthEach
                    (if stage.id == onStage.id then
                        { bottom = 0, left = 1, right = 1, top = 1 }

                     else
                        { bottom = 1, left = 0, right = 0, top = 0 }
                    )
                , Border.rounded 3
                , El.htmlAttribute (class "cio__event_stage_link")
                ]
                { onPress = Just (NavigateTo (stageUrl event.id stage))
                , label = text stage.name
                }

        viewRoundRobin =
            let
                teams =
                    -- Only teams that have IDs in the current stages standings.
                    List.filter (\t -> List.any (\s -> s.teamId == t.id) onStage.standings) event.teams

                teamForStanding standing =
                    List.Extra.find (\t -> t.id == standing.teamId) teams

                hasTies =
                    List.any (\standing -> standing.ties > 0) onStage.standings

                hasPoints =
                    List.any (\standing -> standing.points > 0) onStage.standings

                tableHeader content =
                    el
                        [ Font.bold
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        , El.paddingXY 12 16
                        ]
                        (text (translate translations content))

                tableCell i content =
                    el
                        [ El.paddingXY 12 16
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        content

                teamColumn =
                    Just
                        { header = tableHeader " "
                        , width = El.fill
                        , view =
                            \i standing ->
                                case teamForStanding standing of
                                    Just team ->
                                        let
                                            teamName =
                                                if device.class == El.Phone then
                                                    team.shortName

                                                else
                                                    team.name
                                        in
                                        tableCell i
                                            (button
                                                [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateTo (teamUrl event.id standing.teamId))
                                                , label = text teamName
                                                }
                                            )

                                    Nothing ->
                                        text " "
                        }

                gamesColumn =
                    Just
                        { header =
                            tableHeader
                                (if device.class == El.Phone then
                                    "G"

                                 else
                                    "games"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i (text (String.fromInt standing.played))
                        }

                winsColumn =
                    Just
                        { header =
                            tableHeader
                                (if device.class == El.Phone then
                                    "W"

                                 else
                                    "wins"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i (text (String.fromInt standing.wins))
                        }

                lossesColumn =
                    Just
                        { header =
                            tableHeader
                                (if device.class == El.Phone then
                                    "L"

                                 else
                                    "losses"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i (text (String.fromInt standing.losses))
                        }

                tiesColumn =
                    if hasTies then
                        Just
                            { header =
                                tableHeader
                                    (if device.class == El.Phone then
                                        "T"

                                     else
                                        "ties"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i (text (String.fromInt standing.ties))
                            }

                    else
                        Nothing

                pointsColumn =
                    if hasPoints then
                        Just
                            { header =
                                tableHeader
                                    (if device.class == El.Phone then
                                        "P"

                                     else
                                        "points"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i (text (String.fromFloat standing.points))
                            }

                    else
                        Nothing

                tableColumns =
                    List.filterMap identity [ teamColumn, gamesColumn, winsColumn, lossesColumn, tiesColumn, pointsColumn ]
            in
            El.indexedTable [ El.htmlAttribute (class "cio__event_round_robin_table") ]
                { data = onStage.standings
                , columns = tableColumns
                }

        viewBracket =
            let
                viewGroup group =
                    let
                        gamesForGroup =
                            onStage.games
                                |> List.filter (\g -> Maybe.map (\c -> c.groupId) g.coords == Just group.id)

                        colsForGames =
                            gamesForGroup
                                |> List.map (\g -> Maybe.map (\coords -> coords.col + 5) g.coords |> Maybe.withDefault 0)
                                |> List.maximum
                                |> Maybe.withDefault 10

                        rowsForGroup =
                            gamesForGroup
                                |> List.filter (\g -> Maybe.map (\coords -> coords.groupId == group.id) g.coords |> Maybe.withDefault False)
                                |> List.map (\g -> Maybe.map (\coords -> coords.row + 3) g.coords |> Maybe.withDefault 4)
                                |> List.maximum
                                |> Maybe.withDefault 4

                        viewGroupGame game =
                            case game.coords of
                                Just coords ->
                                    let
                                        viewSide position side =
                                            let
                                                label =
                                                    case ( side.teamId, side.winnerId, side.loserId ) of
                                                        ( Just id, _, _ ) ->
                                                            case List.Extra.find (\t -> t.id == id) event.teams of
                                                                Just team ->
                                                                    team.name

                                                                Nothing ->
                                                                    "TBD"

                                                        ( _, Just winnerId, _ ) ->
                                                            "W: "
                                                                ++ (List.Extra.find (\g -> g.id == winnerId) onStage.games
                                                                        |> Maybe.map .name
                                                                        |> Maybe.withDefault "TBD"
                                                                   )

                                                        ( _, _, Just loserId ) ->
                                                            "L: "
                                                                ++ (List.Extra.find (\g -> g.id == loserId) onStage.games
                                                                        |> Maybe.map .name
                                                                        |> Maybe.withDefault "TBD"
                                                                   )

                                                        _ ->
                                                            "TBD"
                                            in
                                            el
                                                [ El.width El.fill
                                                , El.height (El.px 25)
                                                , El.clip
                                                , El.paddingEach { left = 3, right = 0, top = 6, bottom = 0 }
                                                , if side.result == Just SideResultWon then
                                                    Font.bold

                                                  else
                                                    Font.regular
                                                , if position == 0 then
                                                    Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }

                                                  else
                                                    Border.width 0
                                                , Border.color theme.grey
                                                ]
                                                (text label)

                                        -- Only link if the game has been scheduled
                                        gameHasBeenScheduled =
                                            case drawWithGameId event.draws game.id of
                                                Just _ ->
                                                    True

                                                Nothing ->
                                                    False
                                    in
                                    button [ El.focused [ Background.color theme.transparent ] ]
                                        { onPress =
                                            if gameHasBeenScheduled then
                                                Just (NavigateTo (gameUrl event.id game))

                                            else
                                                Nothing
                                        , label =
                                            column
                                                [ El.width (El.px 178)
                                                , Background.color theme.greyLight
                                                , Border.width 1
                                                , Border.color theme.grey
                                                , Font.size 12
                                                , El.htmlAttribute (style "position" "absolute")
                                                , El.htmlAttribute (style "left" (String.fromInt (coords.col * gridSize) ++ "px"))
                                                , El.htmlAttribute (style "top" (String.fromInt (coords.row * gridSize) ++ "px"))
                                                , El.htmlAttribute (class "cio__event_bracket_game")
                                                ]
                                                [ el
                                                    [ El.width El.fill
                                                    , El.height (El.px 20)
                                                    , El.paddingXY 4 3
                                                    , Font.color theme.white
                                                    , Background.color
                                                        (if game.state == GameActive then
                                                            theme.primary

                                                         else
                                                            theme.secondary
                                                        )
                                                    ]
                                                    (el [] (text game.name))
                                                , column [ El.width El.fill ] (List.indexedMap viewSide game.sides)
                                                ]
                                        }

                                Nothing ->
                                    El.none

                        viewSvgConnectors : Html Msg
                        viewSvgConnectors =
                            let
                                lineConnectors : List LineConnector
                                lineConnectors =
                                    let
                                        connectors : Game -> List (Maybe LineConnector)
                                        connectors toGame =
                                            let
                                                connectorForPosition : Int -> Side -> Maybe LineConnector
                                                connectorForPosition toPosition side =
                                                    let
                                                        fromCoords fromGameId =
                                                            case List.Extra.find (\g -> g.id == fromGameId) gamesForGroup of
                                                                Just fromGame ->
                                                                    case fromGame.coords of
                                                                        Just coords ->
                                                                            Just
                                                                                ( coords.col * gridSize + 175
                                                                                , coords.row
                                                                                    * gridSize
                                                                                    + 45
                                                                                )

                                                                        _ ->
                                                                            Nothing

                                                                _ ->
                                                                    Nothing

                                                        toCoords =
                                                            case toGame.coords of
                                                                Just coords ->
                                                                    Just
                                                                        ( coords.col * gridSize + 1
                                                                        , coords.row
                                                                            * gridSize
                                                                            + (if toPosition == 0 then
                                                                                32

                                                                               else
                                                                                57
                                                                              )
                                                                        )

                                                                Nothing ->
                                                                    Nothing
                                                    in
                                                    case side.winnerId of
                                                        Just winnerId ->
                                                            case ( fromCoords winnerId, toCoords ) of
                                                                ( Just from, Just to ) ->
                                                                    Just (LineConnector from to)

                                                                _ ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing
                                            in
                                            List.indexedMap connectorForPosition toGame.sides
                                    in
                                    List.map connectors gamesForGroup
                                        |> List.concat
                                        |> List.filterMap identity
                            in
                            viewSvgConnector ((colsForGames + 1) * gridSize) ((rowsForGroup - 1) * gridSize) lineConnectors
                    in
                    column
                        [ El.width El.fill, El.spacing 20, El.paddingXY 0 20 ]
                        [ el
                            [ El.width El.fill
                            , El.paddingEach { left = 10, top = 7, right = 0, bottom = 7 }
                            , Background.color theme.greyLight
                            , Font.size 20
                            , Font.medium
                            , El.htmlAttribute (class "cio__event_bracket_group")
                            ]
                            (text ("☷ " ++ group.name))
                        , column [ El.width El.fill, El.htmlAttribute (style "position" "relative") ]
                            [ el [ El.width El.fill ] (El.html viewSvgConnectors)
                            , el [ El.width El.fill, El.height (El.px 12), El.htmlAttribute (style "position" "absolute") ]
                                (column [ El.htmlAttribute (style "position" "relative") ] (List.map viewGroupGame gamesForGroup))
                            ]
                        ]
            in
            case onStage.groups of
                Just groups ->
                    column [ El.width El.fill, El.htmlAttribute (class "cio__event_bracket") ] (List.map viewGroup groups)

                Nothing ->
                    el [] (text "No groups")
    in
    column [ El.width El.fill, El.htmlAttribute (class "cio__event_stages") ]
        [ row [] (List.map viewStageLink event.stages)
        , case onStage.stageType of
            RoundRobin ->
                viewRoundRobin

            Bracket ->
                viewBracket
        ]


viewDraw : Theme -> List Translation -> Maybe ScoringHilight -> Event -> Draw -> Element Msg
viewDraw theme translations scoringHilight event draw =
    let
        viewDrawSheet gameId =
            let
                maybeGame =
                    gamesFromStages event.stages
                        |> List.Extra.find (\g -> Just g.id == gameId)

                sheetLabel game =
                    sheetNameForGame event game
            in
            case maybeGame of
                Just game ->
                    viewGame theme translations scoringHilight event (sheetLabel game) False draw game

                Nothing ->
                    El.none
    in
    column [ El.width El.fill, El.spacing 20 ]
        [ el
            [ El.width El.fill, El.padding 16, Font.color theme.greyDark, Background.color theme.greyLight ]
            (text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt))
        , column [ El.width El.fill, El.spacing 30 ] (List.map viewDrawSheet draw.drawSheets)
        ]


viewGame : Theme -> List Translation -> Maybe ScoringHilight -> Event -> String -> Bool -> Draw -> Game -> Element Msg
viewGame theme translations scoringHilight event sheetLabel detailed draw game =
    let
        maxNumberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        teamName side =
            findTeamForSide event.teams side
                |> Maybe.map .name
                |> Maybe.withDefault "TBD"

        wonOrTied side =
            (side.result == Just SideResultWon)
                || (side.result == Just SideResultTied)

        viewGameCaption =
            let
                label =
                    case game.state of
                        GameComplete ->
                            let
                                winner =
                                    List.Extra.find (\s -> s.result == Just SideResultWon) game.sides

                                loser =
                                    List.Extra.find (\s -> s.result /= Just SideResultWon) game.sides

                                sideResultText result =
                                    sideResultToString translations result
                            in
                            if List.any (\s -> s.result == Just SideResultTied) game.sides then
                                let
                                    joinedResult =
                                        List.map teamName game.sides
                                            |> String.join (" " ++ sideResultText (Just SideResultTied) ++ " ")
                                in
                                text (joinedResult ++ ".")

                            else
                                case ( winner, loser ) of
                                    ( Just w, Just l ) ->
                                        text (teamName w ++ " " ++ sideResultText w.result ++ ", " ++ teamName l ++ " " ++ sideResultText l.result ++ ".")

                                    _ ->
                                        text "Unknown result."

                        GameActive ->
                            text (translate translations "game_in_progress" ++ ": " ++ game.name)

                        GamePending ->
                            text (translate translations "upcoming_game" ++ ": " ++ game.name)
            in
            if detailed then
                el [ Font.italic, Font.color theme.greyDark, El.padding 8 ] label

            else
                button
                    [ Font.color theme.primary
                    , Font.italic
                    , El.padding 8
                    , El.focused [ Background.color theme.primary ]
                    ]
                    { onPress = Just (NavigateTo gamePath), label = label }

        viewGameHilight =
            case scoringHilight of
                Just hilight ->
                    button
                        [ El.alignRight
                        , El.padding 8

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important")
                        , Border.rounded 4
                        , Background.color theme.secondary
                        , El.focused [ Background.color theme.secondary ]
                        ]
                        { onPress = Just (ToggleScoringHilight hilight)
                        , label =
                            text
                                (translate translations
                                    (case hilight of
                                        HilightHammers ->
                                            "hammers"

                                        HilightStolenEnds ->
                                            "stolen_ends"

                                        HilightBlankEnds ->
                                            "blank_ends"

                                        Hilight1PointEnds ->
                                            "one_point_ends"

                                        Hilight2PointEnds ->
                                            "two_point_ends"

                                        Hilight3PointEnds ->
                                            "three_point_ends"

                                        Hilight4PointEnds ->
                                            "four_point_ends"

                                        Hilight5PlusPointEnds ->
                                            "five_plus_point_ends"
                                    )
                                )
                        }

                Nothing ->
                    El.none

        gamePath =
            gameUrl event.id game

        drawPath =
            drawUrl event.id draw

        viewEndScore : Int -> Side -> Element Msg
        viewEndScore endNumber side =
            let
                hasHammer =
                    if endNumber == 1 && side.firstHammer then
                        True

                    else
                        case List.Extra.getAt (endNumber - 2) side.endScores of
                            Just es ->
                                es == 0

                            Nothing ->
                                False

                endScoreStr =
                    case List.Extra.getAt (endNumber - 1) side.endScores of
                        Just es ->
                            String.fromInt es

                        Nothing ->
                            "-"

                endScoreInt =
                    List.Extra.getAt (endNumber - 1) side.endScores
                        |> Maybe.withDefault 0

                stolenEnd =
                    (endScoreInt > 0) && not hasHammer

                blankEnd =
                    case List.Extra.getAt (endNumber - 1) side.endScores of
                        Just 0 ->
                            True

                        _ ->
                            False

                isHilighted =
                    (scoringHilight == Just HilightHammers && hasHammer)
                        || (scoringHilight == Just HilightStolenEnds && stolenEnd)
                        || (scoringHilight == Just HilightBlankEnds && blankEnd)
                        || (scoringHilight == Just Hilight1PointEnds && (endScoreInt == 1))
                        || (scoringHilight == Just Hilight2PointEnds && (endScoreInt == 2))
                        || (scoringHilight == Just Hilight3PointEnds && (endScoreInt == 3))
                        || (scoringHilight == Just Hilight4PointEnds && (endScoreInt == 4))
                        || (scoringHilight == Just Hilight5PlusPointEnds && (endScoreInt > 4))
            in
            el
                [ El.paddingXY 10 15
                , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                , Border.color theme.grey
                , Background.color
                    (if hasHammer && not isHilighted then
                        theme.greyLight

                     else if isHilighted then
                        theme.secondary

                     else
                        theme.transparent
                    )
                , Font.color
                    (if isHilighted then
                        theme.white

                     else
                        theme.defaultText
                    )
                , if isHilighted then
                    Font.bold

                  else
                    Font.regular
                ]
                (el [ El.centerX ] (text endScoreStr))

        teamColumn =
            let
                teamNameElement side =
                    el
                        [ El.paddingXY 10 12
                        , Border.widthEach { left = 1, top = 0, right = 1, bottom = 1 }
                        , Border.color theme.grey
                        , Font.color
                            (if side.firstHammer && scoringHilight == Just HilightHammers then
                                theme.primary

                             else
                                theme.defaultText
                            )
                        , if wonOrTied side then
                            Font.bold

                          else
                            Font.regular
                        ]
                        (el
                            [ El.paddingXY 0 2
                            , Border.widthEach { top = 0, right = 0, bottom = 3, left = 0 }
                            , Border.color
                                (colorNameToRGB
                                    (if side.topRock then
                                        event.topRock

                                     else
                                        event.botRock
                                    )
                                )
                            ]
                            (text
                                (teamName side
                                    ++ (if side.firstHammer then
                                            " *"

                                        else
                                            ""
                                       )
                                )
                            )
                        )
            in
            { header =
                row
                    [ El.paddingXY 10 15
                    , El.spacing 10
                    , Border.widthEach { left = 1, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.semiBold
                    ]
                    [ text sheetLabel
                    , if detailed then
                        El.none

                      else
                        button
                            [ Font.color theme.primary
                            , Font.size 12
                            , El.focused [ Background.color theme.transparent ]
                            ]
                            { onPress = Just (NavigateTo gamePath)
                            , label = text game.name
                            }
                    ]
            , width = El.fill
            , view = teamNameElement
            }

        endColumn endNumber =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text (String.fromInt endNumber)))
            , width = El.px 50
            , view = viewEndScore endNumber
            }

        totalColumn =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text (translate translations "total")))
            , width = El.px 64
            , view =
                \side ->
                    el
                        [ El.paddingXY 10 15
                        , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                        , Border.color theme.grey
                        , if wonOrTied side then
                            Font.bold

                          else
                            Font.regular
                        ]
                        (el [ El.centerX ] (text (String.fromInt (List.sum side.endScores))))
            }

        tableColumns =
            [ teamColumn ] ++ List.map endColumn (List.range 1 maxNumberOfEnds) ++ [ totalColumn ]
    in
    column [ El.width El.fill, El.spacing 10 ]
        -- Breadcrumb
        [ if detailed then
            el [ El.width El.fill, El.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ]
                (row
                    [ El.width El.fill
                    , El.paddingXY 10 15
                    , El.spacing 10
                    , Background.color theme.greyLight
                    , Font.color theme.greyDark
                    ]
                    [ el []
                        (button
                            [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                            { onPress = Just (NavigateTo drawPath)
                            , label = text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt)
                            }
                        )
                    , el [] (text "/")
                    , el [] (text game.name)
                    ]
                )

          else
            El.none

        -- Table
        , El.table []
            { data = game.sides
            , columns = tableColumns
            }

        -- Won / Lost Caption
        , row [ El.width El.fill ]
            [ viewGameCaption
            , viewGameHilight
            ]

        -- Scaring Analysis
        , if detailed then
            column [ El.width El.fill, El.paddingXY 0 10 ]
                [ List.map (findTeamForSide event.teams) game.sides
                    |> List.filterMap identity
                    |> viewReportScoringAnalysis theme translations scoringHilight event
                ]

          else
            El.none
        ]


viewTeam : Theme -> List Translation -> Flags -> Event -> Team -> Element Msg
viewTeam theme translations flags event team =
    let
        viewTeamLineup : Element Msg
        viewTeamLineup =
            let
                hasPosition =
                    List.any (\c -> c.position /= Nothing || c.skip) team.lineup

                hasDelivery =
                    List.any (\c -> c.delivery /= Nothing) team.lineup

                hasClubName =
                    List.any (\c -> c.clubName /= Nothing) team.lineup

                hasClubCity =
                    List.any (\c -> c.clubCity /= Nothing) team.lineup

                hasPhotoUrl =
                    List.any (\c -> c.photoUrl /= Nothing) team.lineup

                hasLoggedInCurler =
                    List.any (\c -> List.member c.curlerId flags.loggedInCurlerIds) team.lineup

                viewTeamCurler : TeamCurler -> Element Msg
                viewTeamCurler curler =
                    let
                        isLoggedInCurler =
                            List.member curler.curlerId flags.loggedInCurlerIds
                    in
                    -- Card
                    column
                        [ Border.width 1
                        , Border.color theme.grey
                        , El.clip
                        , El.padding 20
                        , El.spacing 20
                        , El.width (El.px 250)
                        ]
                        [ if hasPhotoUrl then
                            el [ El.width El.fill ]
                                (case curler.photoUrl of
                                    Just photoUrl ->
                                        El.image [ El.height (El.px 142), El.centerX ]
                                            { src = photoUrl
                                            , description = curler.name
                                            }

                                    Nothing ->
                                        el [ El.height (El.px 150), El.centerX ] (El.html svgNoImage)
                                )

                          else
                            El.none
                        , column []
                            [ el [ Font.size 18, Font.semiBold ] (text curler.name)
                            , column [ El.spacing 5 ]
                                [ el
                                    [ El.paddingEach { top = 5, right = 0, bottom = 2, left = 0 }
                                    , El.onRight
                                        (if curler.position /= Nothing && curler.skip then
                                            el [ El.paddingXY 0 5, Font.size 12 ] (text (" " ++ translate translations "skip"))

                                         else
                                            El.none
                                        )
                                    ]
                                    (case curler.position of
                                        Just position ->
                                            text (teamPositionToString translations curler.position)

                                        Nothing ->
                                            if curler.skip then
                                                text (translate translations "skip")

                                            else if hasPosition then
                                                text " "

                                            else
                                                El.none
                                    )
                                , if hasDelivery then
                                    -- small
                                    el [ Font.size 12 ] (text (translate translations "delivery" ++ ": " ++ deliveryToString translations curler.delivery))

                                  else
                                    El.none
                                , if hasClubName then
                                    -- small
                                    el [ Font.size 12 ] (text (Maybe.withDefault "-" curler.clubName))

                                  else
                                    El.none
                                , if hasClubCity then
                                    -- small
                                    el [ Font.size 12 ] (text (Maybe.withDefault "-" curler.clubCity))

                                  else
                                    El.none
                                , if flags.showWaiversForTeams then
                                    row [ El.spacing 5 ]
                                        [ el [] (text (translate translations "waiver" ++ ":"))
                                        , if curler.waiver then
                                            el [] (text "✓")

                                          else if hasLoggedInCurler && isLoggedInCurler then
                                            button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/curlers"))
                                                , label = text (translate translations "none")
                                                }

                                          else
                                            el [] (text (translate translations "none"))
                                        ]

                                  else
                                    El.none
                                , if hasLoggedInCurler then
                                    -- small
                                    if isLoggedInCurler then
                                        button [ Font.size 12, Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                            { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/curlers"))
                                            , label = text (translate translations "edit_curler")
                                            }

                                    else
                                        El.none

                                  else
                                    El.none
                                ]
                            ]
                        ]
            in
            El.wrappedRow [ El.spacing 20 ] (List.map viewTeamCurler team.lineup)

        viewTeamInfo : Element Msg
        viewTeamInfo =
            let
                hasTeamContactInfo =
                    [ team.contactName, team.contactEmail, team.contactPhone ]
                        |> List.filterMap identity
                        |> List.isEmpty
                        |> not

                hasTeamOtherInfo =
                    [ team.coach, team.affiliation, team.location ]
                        |> List.filterMap identity
                        |> List.isEmpty
                        |> not
            in
            row [ El.width El.fill, El.spacing 20 ]
                [ if hasTeamContactInfo then
                    El.table [ El.width El.fill ]
                        { data =
                            [ { label = "contact_name", data = team.contactName }
                            , { label = "contact_email", data = team.contactEmail }
                            , { label = "contact_phone", data = team.contactPhone }
                            ]
                        , columns =
                            [ { header = El.none
                              , width = El.px 160
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Font.semiBold
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (translate translations item.label))
                              }
                            , { header = El.none
                              , width = El.fill
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (Maybe.withDefault "-" item.data))
                              }
                            ]
                        }

                  else
                    El.none
                , if hasTeamOtherInfo then
                    El.table [ El.width El.fill ]
                        { data =
                            [ { label = "coach", data = team.coach }
                            , { label = "affiliation", data = team.affiliation }
                            , { label = "location", data = team.location }
                            ]
                        , columns =
                            [ { header = El.none
                              , width = El.px 160
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Font.semiBold
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (translate translations item.label))
                              }
                            , { header = El.none
                              , width = El.fill
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (Maybe.withDefault "-" item.data))
                              }
                            ]
                        }

                  else
                    El.none
                ]

        viewTeamSchedule : Element Msg
        viewTeamSchedule =
            let
                hasSideForTeam game =
                    List.any (\s -> s.teamId == Just team.id) game.sides

                drawsAndGames : List { draw : Draw, game : Game }
                drawsAndGames =
                    let
                        gameForDraw draw =
                            List.filterMap identity draw.drawSheets
                                |> List.map (findGameById event.stages)
                                |> List.filterMap identity
                                |> List.filter hasSideForTeam
                                |> List.head
                    in
                    event.draws
                        |> List.map
                            (\draw ->
                                case gameForDraw draw of
                                    Just g ->
                                        Just { draw = draw, game = g }

                                    Nothing ->
                                        Nothing
                            )
                        |> List.filterMap identity

                opponent game =
                    List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                        |> Maybe.andThen (findTeamForSide event.teams)

                drawAndSheetName draw game =
                    draw.label ++ " - " ++ sheetNameForGame event game

                viewTeamDrawLabel { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw))
                                , label = text (drawAndSheetName draw game)
                                }
                            )

                    else
                        tableCell (text (drawAndSheetName draw game))

                viewTeamDrawStartsAt { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw))
                                , label = text draw.startsAt
                                }
                            )

                    else
                        tableCell (text draw.startsAt)

                viewTeamDrawResult { draw, game } =
                    let
                        resultText =
                            let
                                sideResultText res =
                                    sideResultToString translations res
                            in
                            case game.state of
                                GameComplete ->
                                    List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                                        |> Maybe.map .result
                                        |> Maybe.map sideResultText

                                GameActive ->
                                    Just (translate translations "game_in_progress")

                                GamePending ->
                                    Nothing
                    in
                    case resultText of
                        Just t ->
                            if event.endScoresEnabled then
                                tableCell
                                    (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                        { onPress = Just (NavigateTo (gameUrl event.id game))
                                        , label = text t
                                        }
                                    )

                            else
                                tableCell (text t)

                        Nothing ->
                            tableCell (text "-")

                viewTeamDrawScore { draw, game } =
                    case opponent game of
                        Just oppo ->
                            case gameScore game (Just ( team.id, oppo.id )) of
                                Just score ->
                                    if event.endScoresEnabled then
                                        tableCell
                                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateTo (gameUrl event.id game))
                                                , label = text score
                                                }
                                            )

                                    else
                                        tableCell (text score)

                                Nothing ->
                                    tableCell (text "-")

                        Nothing ->
                            tableCell (text "-")

                viewTeamDrawOpponent { draw, game } =
                    case opponent game of
                        Just oppo ->
                            let
                                oppoPath =
                                    teamUrl event.id oppo.id
                            in
                            tableCell
                                (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo oppoPath)
                                    , label = text oppo.name
                                    }
                                )

                        Nothing ->
                            tableCell (text "-")

                tableHeader content =
                    el
                        [ Font.bold
                        , El.paddingXY 12 16
                        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        ]
                        (text (translate translations content))

                tableCell content =
                    el
                        [ El.paddingXY 12 16
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        ]
                        content
            in
            El.table [ El.width El.fill, El.spacingXY 0 15 ]
                { data = drawsAndGames
                , columns =
                    [ { header = tableHeader "draw"
                      , width = El.fill
                      , view = viewTeamDrawLabel
                      }
                    , { header = tableHeader "starts_at"
                      , width = El.fillPortion 2
                      , view = viewTeamDrawStartsAt
                      }
                    , { header = tableHeader "result"
                      , width = El.fill
                      , view = viewTeamDrawResult
                      }
                    , { header = tableHeader "score"
                      , width = El.fill
                      , view = viewTeamDrawScore
                      }
                    , { header = tableHeader "opponent"
                      , width = El.fillPortion 2
                      , view = viewTeamDrawOpponent
                      }
                    ]
                }
    in
    column [ El.spacing 20, El.paddingEach { top = 0, right = 0, bottom = 20, left = 0 }, El.htmlAttribute (class "cio__event_team") ]
        [ el [ Font.size 24, Font.semiBold ] (text team.name)
        , viewTeamLineup
        , viewTeamInfo
        , if not (List.isEmpty event.draws) then
            viewTeamSchedule

          else
            El.none
        ]



-- REPORTS


viewReports : Theme -> List Translation -> Event -> Element Msg
viewReports theme translations event =
    let
        hasAttendance =
            (List.map .attendance event.draws |> List.sum) > 0

        hasShots =
            True

        reportButton id =
            let
                reportLink =
                    "/events/" ++ String.fromInt event.id ++ "/reports/" ++ id
            in
            button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                { onPress = Just (NavigateTo reportLink)
                , label = text ("• " ++ translate translations id)
                }
    in
    column [ El.spacing 15, El.padding 15, El.htmlAttribute (class "cio__event_reports") ]
        ([ reportButton "competition_matrix"
         , reportButton "team_rosters"
         , if hasAttendance then
            reportButton "attendance"

           else
            El.none
         ]
            ++ (if event.endScoresEnabled then
                    [ reportButton "scoring_analysis", reportButton "scoring_analysis_by_hammer" ]

                else
                    []
               )
            ++ (if event.endScoresEnabled && event.shotByShotEnabled then
                    [ reportButton "cumulative_statistics_by_team"
                    , reportButton "hog_line_violation"
                    , reportButton "positional_percentage_comparison"
                    , reportButton "scoring_and_percentages"
                    , reportButton "statistics_by_team"
                    ]

                else
                    []
               )
        )


viewReport : Theme -> List Translation -> Maybe ScoringHilight -> Event -> String -> Element Msg
viewReport theme translations scoringHilight event report =
    case report of
        "competition_matrix" ->
            viewReportCompetitionMatrix theme translations event

        "team_rosters" ->
            viewReportTeamRosters theme translations event.teams

        "attendance" ->
            viewReportAttendance theme translations event.draws

        "scoring_analysis" ->
            let
                games =
                    gamesFromStages event.stages
            in
            viewReportScoringAnalysis theme translations scoringHilight event (teamsWithGames event.teams games)

        "scoring_analysis_by_hammer" ->
            viewReportScoringAnalysisByHammer theme translations event

        "cumulative_statistics_by_team" ->
            viewReportCumulativeStatisticsByTeam theme translations event.draws

        "hog_line_violation" ->
            viewReportHogLineViolation theme translations event

        "positional_percentage_comparison" ->
            viewReportPositionalPercentageComparison theme translations event.draws

        "scoring_and_percentages" ->
            viewReportScoringAndPercentages theme translations event.draws

        "statistics_by_team" ->
            viewReportStatisticsByTeam theme translations event.draws

        _ ->
            viewNoDataForRoute translations


viewReportScoringAnalysis : Theme -> List Translation -> Maybe ScoringHilight -> Event -> List Team -> Element Msg
viewReportScoringAnalysis theme translations scoringHilight event teams =
    let
        rows =
            -- TODO: Structure the data so that for and against are just rows, but when rendering we know due to missing data or a flag which is which.
            List.Extra.interweave teams teams

        isHilighted onHilight =
            scoringHilight == Just onHilight

        isForGame =
            List.length teams == 2

        fullReportUrl =
            "/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"

        games team =
            let
                participatedIn sides =
                    List.any (\s -> s.teamId == Just team.id) sides
            in
            List.filter (\g -> g.state /= GamePending) (gamesFromStages event.stages)
                |> List.filter (\g -> participatedIn g.sides)

        sidesFor team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId == Just team.id)

        sidesAgainst team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId /= Just team.id)

        endsFor team =
            List.map .endScores (sidesFor team)
                |> List.concat

        endsAgainst team =
            List.map .endScores (sidesAgainst team)
                |> List.concat

        firstHammerCountFor team =
            List.filter (\s -> s.firstHammer == True) (sidesFor team)
                |> List.length

        firstHammerCountAgainst team =
            List.filter (\s -> s.firstHammer == True) (sidesAgainst team)
                |> List.length

        blankEndsForOrAgainst team =
            blankEnds (games team) team
                |> List.length

        onePointEndsFor team =
            List.filter (\i -> i == 1) (endsFor team)
                |> List.length

        onePointEndsAgainst team =
            List.filter (\i -> i == 1) (endsAgainst team)
                |> List.length

        twoPointEndsFor team =
            List.filter (\i -> i == 2) (endsFor team)
                |> List.length

        twoPointEndsAgainst team =
            List.filter (\i -> i == 2) (endsAgainst team)
                |> List.length

        threePointEndsFor team =
            List.filter (\i -> i == 3) (endsFor team)
                |> List.length

        threePointEndsAgainst team =
            List.filter (\i -> i == 3) (endsAgainst team)
                |> List.length

        fourPointEndsFor team =
            List.filter (\i -> i == 4) (endsFor team)
                |> List.length

        fourPointEndsAgainst team =
            List.filter (\i -> i == 4) (endsAgainst team)
                |> List.length

        fivePlusPointEndsFor team =
            List.filter (\i -> i > 4) (endsFor team)
                |> List.length

        fivePlusPointEndsAgainst team =
            List.filter (\i -> i > 4) (endsAgainst team)
                |> List.length

        totalPointsFor team =
            List.sum (endsFor team)

        totalPointsAgainst team =
            List.sum (endsAgainst team)

        averagePointsFor team =
            -- total points divided by number of ends. We multiple by 100 then round then divide by 100 so that we get 2 decimal places.
            if totalPointsFor team == 0 then
                0.0

            else
                toFloat (round ((toFloat (totalPointsFor team) / toFloat (List.length (endsFor team))) * 100)) / 100

        averagePointsAgainst team =
            -- see averagePointsFor
            toFloat (round ((toFloat (totalPointsAgainst team) / toFloat (List.length (endsAgainst team))) * 100)) / 100

        stolenEndsCount team for =
            List.length (stolenEnds for (games team) team)

        stolenPoints team for =
            List.sum (stolenEnds for (games team) team)

        tableHeader content align onPress sup =
            el
                [ El.width El.fill
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , Background.color theme.greyLight
                ]
                (button
                    [ El.focused [ Background.color theme.transparent ]
                    , align
                    , Font.semiBold
                    , Font.color
                        (case onPress of
                            Just _ ->
                                if isForGame then
                                    theme.primary

                                else
                                    theme.defaultText

                            Nothing ->
                                theme.defaultText
                        )
                    ]
                    { onPress =
                        if isForGame then
                            onPress

                        else
                            Nothing
                    , label =
                        el
                            [ El.onRight
                                (case sup of
                                    Just sup_ ->
                                        el [ Font.size 10 ] (text sup_)

                                    Nothing ->
                                        El.none
                                )
                            ]
                            (text content)
                    }
                )

        tableCell align bottomBorder content =
            el
                [ El.width El.fill
                , El.padding 10
                , Border.widthEach { top = 0, right = 1, bottom = bottomBorder, left = 1 }
                , Border.color theme.grey
                ]
                (el [ align ] content)
    in
    column
        [ El.width El.fill, El.htmlAttribute (class "cio__event_reports_scoring_analysis") ]
        [ row [ El.width El.fill, El.spacing 10, El.paddingXY 0 20, Font.size 24 ]
            [ text (translate translations "scoring_analysis")
            , if isForGame then
                button [ El.alignRight, Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                    { onPress = Just (NavigateTo fullReportUrl)
                    , label = el [ Font.size 12 ] (text (translate translations "full_report" ++ " →"))
                    }

              else
                El.none
            ]
        , El.indexedTable [ El.width El.fill, Font.size 13 ]
            { data = rows
            , columns =
                [ { header = tableHeader (translate translations "team") El.alignLeft Nothing Nothing
                  , width = El.fillPortion 5 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                button
                                    [ Font.color theme.primary
                                    , El.focused [ Background.color theme.transparent ]
                                    ]
                                    { onPress = Just (NavigateTo (teamUrl event.id team.id))
                                    , label =
                                        team.name
                                            |> text
                                            |> tableCell El.alignLeft 0
                                    }

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader (translate translations "games") El.centerX Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                games team
                                    |> List.length
                                    |> String.fromInt
                                    |> text
                                    |> tableCell El.centerX 0

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader (translate translations "ends") El.centerX Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- It doesn't matter if we use endsFor or endsAgainst, the count is the same since these the ends they participated in, just whether it's the top or bot.
                                endsFor team
                                    |> List.length
                                    |> String.fromInt
                                    |> text
                                    |> tableCell El.centerX 0

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader " " El.alignLeft Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                tableCell El.centerX 1 (text "For")

                            else
                                tableCell El.centerX 1 (text "Against")
                  }

                -- LSFE / First Hammer
                , { header =
                        tableHeader "LSFE" El.centerX (Just (ToggleScoringHilight HilightHammers)) (Just "1")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (firstHammerCountFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (firstHammerCountAgainst team)))
                  }

                -- Stolen Ends
                , { header =
                        tableHeader "SE" El.centerX (Just (ToggleScoringHilight HilightStolenEnds)) (Just "2")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (stolenEndsCount team True)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (stolenEndsCount team False)))
                  }

                -- Blank Ends
                , { header =
                        tableHeader "BE" El.centerX Nothing (Just "3")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsForOrAgainst team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsForOrAgainst team)))
                  }

                -- 1 point ends
                , { header = tableHeader "1pt" El.centerX (Just (ToggleScoringHilight Hilight1PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (onePointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (onePointEndsAgainst team)))
                  }

                -- 2 point ends
                , { header = tableHeader "2pt" El.centerX (Just (ToggleScoringHilight Hilight2PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (twoPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (twoPointEndsAgainst team)))
                  }

                -- 3 point ends
                , { header = tableHeader "3pt" El.centerX (Just (ToggleScoringHilight Hilight3PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (threePointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (threePointEndsAgainst team)))
                  }

                -- 4 point ends
                , { header = tableHeader "4pt" El.centerX (Just (ToggleScoringHilight Hilight4PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (fourPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (fourPointEndsAgainst team)))
                  }

                -- 5+ point ends
                , { header = tableHeader "5pt" El.centerX (Just (ToggleScoringHilight Hilight5PlusPointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (fivePlusPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (fivePlusPointEndsAgainst team)))
                  }

                -- Total points
                , { header = tableHeader "Tot" El.centerX Nothing Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (totalPointsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (totalPointsAgainst team)))
                  }

                -- Average points
                , { header = tableHeader "Avg" El.centerX Nothing Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromFloat (averagePointsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromFloat (averagePointsAgainst team)))
                  }

                -- Stolen points
                , { header = tableHeader "SP" El.centerX (Just (ToggleScoringHilight HilightStolenEnds)) (Just "4")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (stolenPoints team True)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (stolenPoints team False)))
                  }
                ]
            }
        , El.column [ El.paddingXY 0 20, El.spacing 5, Font.size 12 ]
            [ el [] (text "1 - LSFE: Last Shot First End")
            , el [] (text "2 - SE: Stolen Ends")
            , el [] (text "3 - BE: Blank Ends")
            , el [] (text "4 - SP: Stolen Points")
            ]
        ]


viewReportScoringAnalysisByHammer : Theme -> List Translation -> Event -> Element Msg
viewReportScoringAnalysisByHammer theme translations event =
    let
        games : List Game
        games =
            -- Not pending
            List.filter (\g -> g.state /= GamePending) (gamesFromStages event.stages)

        viewByHammer withHammer =
            let
                teams =
                    teamsWithGames event.teams games

                viewHeader { portion, align, content } =
                    el
                        [ El.width (El.fillPortion portion |> El.minimum 85)
                        , El.padding 10
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        ]
                        (el [ align ] (text (translate translations content)))

                viewCell { portion, align, content } =
                    el
                        [ El.width (El.fillPortion portion |> El.minimum 85)
                        , El.padding 10
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        ]
                        (el [ align ] content)

                viewTeamByHammer rowNumber team =
                    let
                        gamesCount =
                            gamesForTeam games team
                                |> List.length

                        sidesFor =
                            List.map .sides games
                                |> List.concat
                                |> List.filter (\s -> s.teamId == Just team.id)

                        sidesAgainst =
                            List.map .sides (gamesForTeam games team)
                                |> List.concat
                                |> List.filter (\s -> s.teamId /= Just team.id)

                        endsFor =
                            List.map .endScores sidesFor
                                |> List.concat

                        endsAgainst =
                            List.map .endScores sidesAgainst
                                |> List.concat

                        endsCount =
                            endsFor |> List.length

                        blankEndsForOrAgainst =
                            blankEnds (gamesForTeam games team) team
                                |> List.length

                        blankEndsForOrAgainstPercent =
                            round ((toFloat blankEndsForOrAgainst / toFloat (endsFor |> List.length)) * 100)

                        stolenEndsAgainst =
                            List.length (stolenEnds False games team)

                        stolenEndsAgainstPercent =
                            round ((toFloat stolenEndsAgainst / toFloat (endsAgainst |> List.length)) * 100)

                        singlePointsFor =
                            List.filter (\e -> e == 1) endsFor
                                |> List.length

                        singlePointsForPercent =
                            round ((toFloat singlePointsFor / toFloat (endsFor |> List.length)) * 100)

                        multiPointsFor =
                            List.filter (\e -> e > 1) endsFor
                                |> List.length

                        multiPointsForPercent =
                            round ((toFloat multiPointsFor / toFloat (endsFor |> List.length)) * 100)
                    in
                    row
                        [ El.width El.fill
                        , Background.color
                            (if modBy 2 rowNumber == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        [ viewCell
                            { portion = 2
                            , align = El.alignLeft
                            , content =
                                button
                                    [ Font.color theme.primary
                                    , El.focused [ Background.color theme.transparent ]
                                    ]
                                    { onPress = Just (NavigateTo (teamUrl event.id team.id))
                                    , label = text team.name
                                    }
                            }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt gamesCount) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt endsCount) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt blankEndsForOrAgainst) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt blankEndsForOrAgainstPercent) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt stolenEndsAgainst) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt stolenEndsAgainstPercent) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt singlePointsFor) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt singlePointsForPercent) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt multiPointsFor) }
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt multiPointsForPercent) }
                        ]
            in
            column [ El.width El.fill ]
                ([ row [ El.width El.fill, Font.semiBold ]
                    [ viewHeader
                        { portion = 4
                        , align = El.alignLeft
                        , content =
                            if withHammer then
                                "with_hammer"

                            else
                                "without_hammer"
                        }
                    , viewHeader { portion = 2, align = El.alignRight, content = "blank_ends_for" }
                    , viewHeader { portion = 2, align = El.alignRight, content = "stolen_ends_against" }
                    , viewHeader { portion = 2, align = El.alignRight, content = "single_points_for" }
                    , viewHeader { portion = 2, align = El.alignRight, content = "multi_points_for" }
                    ]
                 , row [ El.width El.fill, Font.semiBold ]
                    [ viewHeader { portion = 2, align = El.alignLeft, content = "team" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "games" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "ends" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "#" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "%" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "#" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "%" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "#" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "%" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "#" }
                    , viewHeader { portion = 1, align = El.alignRight, content = "%" }
                    ]
                 ]
                    ++ List.indexedMap viewTeamByHammer teams
                )
    in
    column [ El.spacing 30, El.width El.fill, El.htmlAttribute (class "cio__event_reports_scoring_analysis_by_hammer") ]
        [ el [ Font.size 24 ] (text (translate translations "scoring_analysis_by_hammer"))
        , column [ El.spacing 80, El.width El.fill ]
            [ viewByHammer True
            , viewByHammer False
            ]
        ]


viewReportTeamRosters : Theme -> List Translation -> List Team -> Element Msg
viewReportTeamRosters theme translations teams =
    let
        hasDelivery =
            let
                hasForTeam team =
                    List.any (\c -> c.delivery /= Nothing) team.lineup
            in
            List.filter hasForTeam teams
                |> List.isEmpty
                |> not

        viewTeamRoster team =
            El.table []
                { data = team.lineup
                , columns =
                    [ { header =
                            el
                                [ El.padding 15
                                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                , Border.color theme.grey
                                , Font.semiBold
                                , Background.color theme.greyLight
                                ]
                                (text team.name)
                      , width = El.fill
                      , view =
                            \curler ->
                                el
                                    [ El.padding 15
                                    , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                    , Border.color theme.grey
                                    ]
                                    (text curler.name)
                      }
                    , { header =
                            el
                                [ El.padding 15
                                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                , Border.color theme.grey
                                , Font.semiBold
                                , Background.color theme.greyLight
                                ]
                                (text " ")
                      , width = El.fill
                      , view =
                            \curler ->
                                row
                                    [ El.padding 15
                                    , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                    , Border.color theme.grey
                                    , El.spacing 5
                                    ]
                                    [ el [] (text (teamPositionToString translations curler.position))
                                    , if curler.skip then
                                        el [ Font.size 12, El.alignLeft ] (text (translate translations "skip"))

                                      else
                                        El.none
                                    ]
                      }
                    , { header =
                            el
                                [ El.padding 15
                                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                , Border.color theme.grey
                                , Font.semiBold
                                , Background.color theme.greyLight
                                ]
                                (text " ")
                      , width = El.fill
                      , view =
                            \curler ->
                                el
                                    [ El.padding 15
                                    , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                                    , Border.color theme.grey
                                    ]
                                    (if hasDelivery then
                                        text (deliveryToString translations curler.delivery)

                                     else
                                        text " "
                                    )
                      }
                    ]
                }
    in
    column [ El.spacing 20, El.width El.fill, El.htmlAttribute (class "cio__event_reports_team_rosters") ]
        [ el [ Font.size 24, Font.semiBold ] (text (translate translations "team_rosters"))
        , column [ El.width El.fill ] (List.map viewTeamRoster teams)
        ]


viewReportCompetitionMatrix : Theme -> List Translation -> Event -> Element Msg
viewReportCompetitionMatrix theme translations event =
    let
        viewStageMatrix stage =
            let
                -- Only the teams participating in this stage.
                teams =
                    -- Filter the teams so only team ids included in sides included in games included in the passed stage.
                    let
                        teamIncluded team =
                            let
                                inSide side =
                                    side.teamId == Just team.id

                                inGame game =
                                    List.any inSide game.sides
                            in
                            List.any inGame stage.games
                    in
                    -- Filter the event.teams to include only those teams involved in games that are in this stage.
                    List.filter teamIncluded event.teams

                viewTeamCell : Team -> Team -> Element Msg
                viewTeamCell teamA teamB =
                    let
                        teamIdsForGame g =
                            List.map .teamId g.sides
                                |> List.filterMap identity

                        matchingGame =
                            List.Extra.find (\g -> [ teamA.id, teamB.id ] == teamIdsForGame g || [ teamB.id, teamA.id ] == teamIdsForGame g) stage.games
                    in
                    case matchingGame of
                        Just game ->
                            el
                                [ El.clip
                                , El.width (El.px 115)
                                , El.height (El.px 47)
                                , El.padding 7
                                , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                                , Border.color theme.grey
                                ]
                                (el [ El.centerX, El.centerY ]
                                    (case gameScore game (Just ( teamA.id, teamB.id )) of
                                        Just score ->
                                            if event.endScoresEnabled then
                                                let
                                                    -- Only link if the game has been scheduled
                                                    gameHasBeenScheduled =
                                                        case drawWithGameId event.draws game.id of
                                                            Just _ ->
                                                                True

                                                            Nothing ->
                                                                False

                                                    gamePath =
                                                        gameUrl event.id game
                                                in
                                                if gameHasBeenScheduled then
                                                    button
                                                        [ Font.color theme.primary
                                                        , El.focused [ Background.color theme.transparent ]
                                                        ]
                                                        { onPress = Just (NavigateTo gamePath)
                                                        , label = text score
                                                        }

                                                else
                                                    text score

                                            else
                                                text score

                                        Nothing ->
                                            text
                                                (case game.state of
                                                    GameComplete ->
                                                        " "

                                                    _ ->
                                                        " "
                                                )
                                    )
                                )

                        Nothing ->
                            el
                                [ El.width (El.px 115)
                                , El.height (El.px 47)
                                , Background.color theme.greyLight
                                , El.padding 20
                                , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                                , Border.color theme.grey
                                ]
                                (text " ")

                viewHeader team =
                    el
                        [ El.clip
                        , El.width (El.px 115)
                        , El.height (El.px 47)
                        , El.padding 7
                        , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                        , Border.color theme.grey
                        ]
                        (case team of
                            Just t ->
                                button
                                    [ El.centerX
                                    , El.centerY
                                    , Font.color theme.primary
                                    , El.focused [ Background.color theme.transparent ]
                                    ]
                                    { onPress = Just (NavigateTo (teamUrl event.id t.id))
                                    , label = text t.shortName
                                    }

                            Nothing ->
                                text " "
                        )

                viewTableColumn idx =
                    let
                        team =
                            List.Extra.getAt idx teams
                    in
                    { header = viewHeader team
                    , width = El.px 115
                    , view =
                        \teamA ->
                            case team of
                                Just teamB ->
                                    viewTeamCell teamA teamB

                                Nothing ->
                                    viewHeader Nothing
                    }
            in
            if List.isEmpty stage.games then
                El.none

            else
                column [ El.spacing 10, El.alignTop ]
                    [ el [ Font.size 20 ] (text stage.name)
                    , El.table [ Border.widthEach { top = 0, right = 0, bottom = 1, left = 1 }, Border.color theme.grey, Font.size 14 ]
                        { data = teams
                        , columns =
                            [ { header = viewHeader Nothing
                              , width = El.px 115
                              , view = \team -> viewHeader (Just team)
                              }
                            ]
                                ++ List.map viewTableColumn (List.range 0 (List.length teams - 1))
                        }
                    ]
    in
    column [ El.width El.fill, El.spacing 30, El.htmlAttribute (class "cio__event_reports_competition_matrix") ]
        [ el [ Font.size 24 ] (text (translate translations "competition_matrix"))
        , El.wrappedRow [ El.spacing 30 ] (List.filter (\s -> s.stageType == RoundRobin) event.stages |> List.map viewStageMatrix)
        ]


viewReportAttendance : Theme -> List Translation -> List Draw -> Element Msg
viewReportAttendance theme translations draws =
    let
        viewHeader content =
            el
                [ El.padding 20
                , Font.semiBold
                , Border.widthEach { top = 0, right = 1, bottom = 1, left = 0 }
                , Border.color theme.grey
                ]
                (el [ El.alignRight ] (text (translate translations content)))

        viewCell idx content =
            el
                [ El.padding 20
                , Border.widthEach { top = 0, right = 1, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Background.color
                    (if modBy 2 idx == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                (el [ El.alignRight ] (text content))
    in
    column [ El.spacing 20, El.htmlAttribute (class "cio__event_reports_attendance") ]
        [ el [ Font.size 24 ] (text (translate translations "attendance"))
        , El.indexedTable [ Border.widthEach { top = 1, right = 0, bottom = 0, left = 1 }, Border.color theme.grey ]
            { data = draws
            , columns =
                [ { header = viewHeader "draw"
                  , width = El.fill
                  , view = \idx draw -> viewCell idx draw.label
                  }
                , { header = viewHeader "attendance"
                  , width = El.fill
                  , view = \idx draw -> viewCell idx (String.fromInt draw.attendance)
                  }
                , { header = viewHeader "total"
                  , width = El.fill
                  , view =
                        \idx draw ->
                            viewCell idx
                                (List.take (idx + 1) draws
                                    |> List.map (\d -> d.attendance)
                                    |> List.sum
                                    |> String.fromInt
                                )
                  }
                ]
            }
        ]


viewReportCumulativeStatisticsByTeam : Theme -> List Translation -> List Draw -> Element Msg
viewReportCumulativeStatisticsByTeam theme translations draws =
    column [ El.spacing 20 ]
        [ el [ Font.size 24 ] (text (translate translations "cumulative_statistics_by_team"))
        , el [] (text "Coming Soon!")
        ]


viewReportHogLineViolation : Theme -> List Translation -> Event -> Element Msg
viewReportHogLineViolation theme translations { teams, stages, draws } =
    let
        hogLineViolations : List HogLineViolation
        hogLineViolations =
            -- Kind of annoying, but we need to dig down to shots while picking up the draw label, and side's team id.
            -- Ignore it if we're missing a draw (unscheduled), team (no point in reporting), curler (no point in reporting), or it's not a violation.
            gamesFromStages stages
                |> List.map
                    (\game ->
                        case drawWithGameId draws game.id of
                            Just draw ->
                                game.sides
                                    |> List.map
                                        (\side ->
                                            case side.teamId of
                                                Just teamId ->
                                                    side.shots
                                                        |> List.map
                                                            (\shot ->
                                                                case ( shot.curlerId, shot.rating ) of
                                                                    ( Just curlerId, Just "V" ) ->
                                                                        Just (HogLineViolation draw.label teamId curlerId shot.endNumber)

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                        |> List.filterMap identity

                                                Nothing ->
                                                    []
                                        )
                                    -- Lift the shots up to the sides
                                    |> List.concat

                            Nothing ->
                                []
                    )
                -- Lift the sides up to the games
                |> List.concat

        viewHeader label =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Font.semiBold
                , Background.color theme.greyLight
                ]
                (text (translate translations label))

        viewCell content =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                ]
                (text content)

        teamName teamId =
            case List.Extra.find (\team -> team.id == teamId) teams of
                Just team ->
                    team.name

                Nothing ->
                    "TBD"

        curlerName teamId curlerId =
            case List.Extra.find (\team -> team.id == teamId) teams of
                Just team ->
                    case List.Extra.find (\tc -> tc.curlerId == curlerId) team.lineup of
                        Just teamCurler ->
                            teamCurler.name

                        Nothing ->
                            "TBD"

                Nothing ->
                    "TBD"
    in
    -- We are looking for any shots with a rating value of "V" (for violation)
    -- Then we want to report the athlete name, the team name, the draw label, and the end number.
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "hog_line_violation"))
        , El.table []
            { data = hogLineViolations
            , columns =
                [ { header = viewHeader "curler"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell (curlerName hogLineViolation.teamId hogLineViolation.curlerId)
                  }
                , { header = viewHeader "team"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell (teamName hogLineViolation.teamId)
                  }
                , { header = viewHeader "draw"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell hogLineViolation.drawLabel
                  }
                , { header = viewHeader "end_number"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell (String.fromInt hogLineViolation.endNumber)
                  }
                ]
            }
        ]


viewReportPositionalPercentageComparison : Theme -> List Translation -> List Draw -> Element Msg
viewReportPositionalPercentageComparison theme translations draws =
    column [ El.spacing 20 ]
        [ el [ Font.size 24 ] (text (translate translations "positional_percentage_comparison"))
        , el [] (text "Coming Soon!")
        ]


viewReportScoringAndPercentages : Theme -> List Translation -> List Draw -> Element Msg
viewReportScoringAndPercentages theme translations draws =
    column [ El.spacing 20 ]
        [ el [ Font.size 24 ] (text (translate translations "scoring_and_percentages"))
        , el [] (text "Coming Soon!")
        ]


viewReportStatisticsByTeam : Theme -> List Translation -> List Draw -> Element Msg
viewReportStatisticsByTeam theme translations draws =
    column [ El.spacing 20 ]
        [ el [ Font.size 24 ] (text (translate translations "statistics_by_team"))
        , el [] (text "Coming Soon!")
        ]



-- PORTS


port navigateTo : String -> Cmd msg


port hashChangeReceiver : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ hashChangeReceiver (HashChanged False)
        , Browser.Events.onResize (\values -> SetDevice values)
        , Time.every 1000 Tick
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
