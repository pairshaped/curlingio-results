port module Results exposing (init)

import Browser
import CustomSvg exposing (..)
import Html exposing (Html, a, button, caption, div, h3, h4, h5, h6, i, img, input, label, li, nav, ol, option, p, select, small, span, strong, sup, table, tbody, td, text, th, thead, tr, u, ul)
import Html.Attributes exposing (alt, attribute, class, classList, colspan, disabled, href, id, placeholder, rowspan, selected, src, style, target, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Time
import Url
import Url.Parser exposing ((</>), Parser)



-- MODEL


gridSize : Int
gridSize =
    50


timeBetweenReloads : Int
timeBetweenReloads =
    10


type alias Model =
    { flags : Flags
    , hash : String
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


type alias Translation =
    { key : String
    , label : String
    }


type alias Flags =
    { host : Maybe String
    , hash : Maybe String
    , lang : Maybe String
    , apiKey : String
    , section : ItemsSection
    , registration : Bool
    , excludeEventSections : List String
    , defaultEventSection : Maybe String
    , eventId : Maybe Int
    }


type ItemsSection
    = LeaguesSection
    | CompetitionsSection
    | ProductsSection


type alias ItemFilter =
    { page : Int
    , seasonDelta : Int
    , search : String
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
    , position : TeamPosition
    , skip : Bool
    , name : String
    , delivery : Maybe RockDelivery
    , clubName : Maybe String
    , clubCity : Maybe String
    , photoUrl : Maybe String
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


type alias Lineup =
    { first : Maybe String
    , second : Maybe String
    , third : Maybe String
    , fourth : Maybe String
    , alternate : Maybe String
    }


type alias Stage =
    { id : Int
    , parentId : Maybe Int
    , stageType : StageType
    , name : String
    , iterations : Int
    , rankingMethod : RankingMethod
    , pointsPerWin : Float
    , pointsPerTie : Float
    , pointsPerLoss : Float
    , pointsPerEnd : List Float
    , tiebreaker : Tiebreaker
    , groups : Maybe (List Group)
    }


type StageType
    = RoundRobin
    | Bracket


type RankingMethod
    = PointsRanking
    | SkinsRanking
    | ScoresRanking


type Tiebreaker
    = TiebreakerNone
    | TiebreakerHeadToHead
    | TiebreakerHeadToHeadThenSkillRank
    | TiebreakerScores


type alias Draw =
    { id : Int
    , startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe Game)
    }


type alias Group =
    { id : Int
    , name : String
    }


type alias Game =
    { id : String
    , name : String
    , stageId : Int
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
    }


type SideResult
    = SideResultWon
    | SideResultLost
    | SideResultTied
    | SideResultConceded
    | SideResultForfeited
    | SideResultTimePenalized


type alias TeamResult =
    { team : Team
    , gamesPlayed : Int
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

                            "products" ->
                                Decode.succeed ProductsSection

                            _ ->
                                Decode.succeed LeaguesSection
                    )
    in
    Decode.succeed Flags
        |> optional "host" (nullable string) Nothing
        |> optional "hash" (nullable string) Nothing
        |> optional "lang" (nullable string) Nothing
        |> required "apiKey" string
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "excludeEventSections" (list string) []
        |> optional "defaultEventSection" (nullable string) Nothing
        |> optional "eventId" (nullable int) Nothing


decodeTranslations : Decoder (List Translation)
decodeTranslations =
    list decodeTranslation


decodeTranslation : Decoder Translation
decodeTranslation =
    Decode.succeed Translation
        |> required "key" string
        |> required "label" string


decodeItems : Decoder (List Item)
decodeItems =
    list decodeItem


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
        |> required "position" decodeTeamPosition
        |> optional "skip" bool False
        |> required "name" string
        |> optional "delivery" (nullable decodeDelivery) Nothing
        |> optional "club_name" (nullable string) Nothing
        |> optional "club_city" (nullable string) Nothing
        |> optional "photo_url" (nullable string) Nothing


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

        decodeRankingMethod : Decoder RankingMethod
        decodeRankingMethod =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "skins" ->
                                Decode.succeed SkinsRanking

                            "scores" ->
                                Decode.succeed ScoresRanking

                            _ ->
                                Decode.succeed PointsRanking
                    )

        decodePointsPerEnd : Decoder (List Float)
        decodePointsPerEnd =
            string
                |> Decode.andThen
                    (\str ->
                        String.replace " " "" str
                            |> String.split ","
                            |> List.map (\s -> String.toFloat s)
                            |> List.filterMap identity
                            |> Decode.succeed
                    )

        decodeTiebreaker : Decoder Tiebreaker
        decodeTiebreaker =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "head_to_head" ->
                                Decode.succeed TiebreakerHeadToHead

                            "head_to_head_then_skill_rank" ->
                                Decode.succeed TiebreakerHeadToHeadThenSkillRank

                            "scores" ->
                                Decode.succeed TiebreakerScores

                            _ ->
                                Decode.succeed TiebreakerNone
                    )

        decodeGroup : Decoder Group
        decodeGroup =
            Decode.succeed Group
                |> required "id" int
                |> required "name" string
    in
    Decode.succeed Stage
        |> required "id" int
        |> optional "parent_id" (nullable int) Nothing
        |> required "type" decodeStageType
        |> required "name" string
        |> optional "iterations" int 1
        |> optional "ranking_method" decodeRankingMethod PointsRanking
        |> optional "points_per_win" float 0
        |> optional "points_per_tie" float 0
        |> optional "points_per_loss" float 0
        |> optional "points_per_end" decodePointsPerEnd []
        |> optional "tiebreaker" decodeTiebreaker TiebreakerNone
        |> optional "groups" (nullable (list decodeGroup)) Nothing


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" int
        |> required "starts_at" string
        |> required "label" string
        |> optional "attendance" int 0
        |> required "draw_sheets" (list (nullable decodeGame))


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
    in
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "stage_id" int
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
    Url.Parser.oneOf
        -- [ Url.Parser.map
        --     (case defaultEventSection of
        --         Just "registrations" ->
        --             RegistrationsRoute
        --
        --         Just "draws" ->
        --             DrawsRoute
        --
        --         Just "stages" ->
        --             StagesRoute
        --
        --         Just "teams" ->
        --             TeamsRoute
        --
        --         Just "reports" ->
        --             ReportsRoute
        --
        --         _ ->
        --             DetailsRoute
        --     )
        --     Url.Parser.top
        [ Url.Parser.map DetailsRoute Url.Parser.top
        , Url.Parser.map DetailsRoute (Url.Parser.s "details")
        , Url.Parser.map RegistrationsRoute (Url.Parser.s "registrations")
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


teamUrl : Int -> Team -> String
teamUrl eventId team =
    "/events/" ++ String.fromInt eventId ++ "/teams/" ++ String.fromInt team.id


stageUrl : Int -> Stage -> String
stageUrl eventId stage =
    let
        eventIdStr =
            String.fromInt eventId

        stageIdStr =
            String.fromInt stage.id
    in
    "/events/" ++ String.fromInt eventId ++ "/stages/" ++ String.fromInt stage.id


translate : WebData (List Translation) -> String -> String
translate translationsData key =
    -- Translates the passed key to the current labels (server determines locale by url).
    case translationsData of
        Success translations ->
            case List.Extra.find (\translation -> String.toLower translation.key == String.toLower key) translations of
                Just translation ->
                    translation.label

                Nothing ->
                    key

        _ ->
            key


colorNameToRGB : String -> String
colorNameToRGB color =
    case color of
        "red" ->
            "rgb(204, 0, 0)"

        "yellow" ->
            "rgb(204, 204, 0)"

        _ ->
            color


sideResultToString : WebData (List Translation) -> Maybe SideResult -> String
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


teamPositionToString : WebData (List Translation) -> TeamPosition -> String
teamPositionToString translations position =
    translate translations
        (case position of
            TeamFourth ->
                "fourth"

            TeamThird ->
                "third"

            TeamSecond ->
                "second"

            TeamFirst ->
                "first"

            TeamAlternate ->
                "alternate"
        )


deliveryToString : WebData (List Translation) -> Maybe RockDelivery -> String
deliveryToString translations delivery =
    case delivery of
        Just RockDeliveryRight ->
            translate translations "right"

        Just RockDeliveryLeft ->
            translate translations "right"

        Nothing ->
            "-"


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
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
                                    "#/events/" ++ String.fromInt eventId ++ "/" ++ section

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
                    Model flags newHash False NotAsked NotAsked (ItemFilter 1 0 "") NotAsked NotAsked Nothing Nothing timeBetweenReloads
            in
            ( newModel
            , Cmd.batch
                [ getTranslations flags
                , Tuple.second (getItemsMaybe newModel newHash False)
                , Tuple.second (getEventMaybe newModel newHash False)
                , Tuple.second (getProductMaybe newModel newHash)
                ]
            )

        Err error ->
            let
                flags =
                    Flags Nothing Nothing Nothing "" LeaguesSection False [] Nothing Nothing
            in
            ( Model flags "" False NotAsked NotAsked (ItemFilter 1 0 "") NotAsked NotAsked Nothing (Just (Decode.errorToString error)) 0
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
            "http://api.curling.test:3000/" ++ Maybe.withDefault "en" lang

        productionUrl =
            -- Production without caching
            "https://api.curling.io/" ++ Maybe.withDefault "en" lang

        productionCachedUrl =
            -- Production cached via CDN (Fastly)
            "https://api-curlingio.global.ssl.fastly.net/" ++ Maybe.withDefault "en" lang
    in
    case host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl

            else if String.contains ".curling.io" h then
                productionUrl

            else
                productionCachedUrl

        Nothing ->
            productionCachedUrl


baseClubUrl : Flags -> String
baseClubUrl flags =
    baseUrl flags ++ "/clubs/" ++ flags.apiKey ++ "/"


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
                ++ (case flags.section of
                        LeaguesSection ->
                            "leagues"

                        CompetitionsSection ->
                            "competitions"

                        ProductsSection ->
                            "products"
                   )
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

                hasDraws =
                    not (List.isEmpty event.draws)

                hasStages =
                    not (List.isEmpty event.stages)

                hasTeams =
                    not (List.isEmpty event.teams)

                hasCompletedGames =
                    List.any (\g -> g.state == GameComplete) (gamesFromDraws event.draws)
            in
            case section of
                "registrations" ->
                    hasRegistrations

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
    [ "details", "registrations", "draws", "stages", "teams", "reports" ]
        |> List.filter included
        |> List.filter hasData


eventSectionForRoute : NestedEventRoute -> String
eventSectionForRoute route =
    case route of
        DetailsRoute ->
            "details"

        RegistrationsRoute ->
            "registrations"

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


teamResultsForGames : Stage -> List Team -> List Game -> List TeamResult
teamResultsForGames stage teams games =
    let
        sides team =
            List.filter (\g -> g.state == GameComplete) games
                |> List.map .sides
                |> List.concat
                |> List.filter (\side -> side.teamId == Just team.id)

        gamesPlayed team =
            List.length (sides team)

        results team =
            sides team
                |> List.map (\side -> side.result)
                |> List.filterMap identity

        wins team =
            results team
                |> List.filter (\result -> result == SideResultWon)
                |> List.length

        losses team =
            let
                isLoss result =
                    (result == SideResultLost)
                        || (result == SideResultConceded)
                        || (result == SideResultForfeited)
                        || (result == SideResultTimePenalized)
            in
            results team
                |> List.filter isLoss
                |> List.length

        ties team =
            results team
                |> List.filter (\result -> result == SideResultTied)
                |> List.length

        assignPoints teamResult =
            { teamResult
                | points =
                    case stage.rankingMethod of
                        PointsRanking ->
                            (toFloat (wins teamResult.team) * stage.pointsPerWin)
                                + (toFloat (ties teamResult.team) * stage.pointsPerTie)
                                + (toFloat (losses teamResult.team) * stage.pointsPerLoss)

                        SkinsRanking ->
                            let
                                pointsPerEnd index score =
                                    if score > 0 then
                                        List.Extra.getAt index stage.pointsPerEnd
                                            |> Maybe.withDefault 0.0

                                    else
                                        0.0
                            in
                            List.map (\side -> side.endScores) (sides teamResult.team)
                                |> List.map (\endScores -> List.indexedMap pointsPerEnd endScores)
                                |> List.concat
                                |> List.sum

                        ScoresRanking ->
                            List.map (\side -> side.endScores) (sides teamResult.team)
                                |> List.concat
                                |> List.sum
                                |> toFloat
            }
    in
    List.map (\team -> TeamResult team 0 0 0 0 0) teams
        |> List.map (\teamResult -> { teamResult | gamesPlayed = gamesPlayed teamResult.team })
        |> List.map (\teamResult -> { teamResult | wins = wins teamResult.team })
        |> List.map (\teamResult -> { teamResult | losses = losses teamResult.team })
        |> List.map (\teamResult -> { teamResult | ties = ties teamResult.team })
        |> List.map assignPoints


teamResultsRankedByPoints : List TeamResult -> List TeamResult
teamResultsRankedByPoints teamResults =
    List.sortBy .points teamResults
        |> List.reverse


teamHasDetails : Team -> Bool
teamHasDetails team =
    -- Check if there are any team details to show.
    not (List.isEmpty team.lineup)
        || (team.contactName /= Nothing)
        || (team.contactEmail /= Nothing)
        || (team.contactPhone /= Nothing)


findTeamForSide : List Team -> Side -> Maybe Team
findTeamForSide teams side =
    List.Extra.find (\t -> Just t.id == side.teamId) teams


sheetNameForGame : Event -> Game -> String
sheetNameForGame event game =
    let
        drawHasGame : Draw -> Bool
        drawHasGame draw =
            draw.drawSheets
                |> List.filterMap identity
                |> List.any (\g -> g.id == game.id)

        sheetNumber : Draw -> Maybe Int
        sheetNumber draw =
            let
                matching g =
                    case g of
                        Just g_ ->
                            g_.id == game.id

                        Nothing ->
                            False
            in
            List.Extra.findIndex matching draw.drawSheets
    in
    case List.Extra.find drawHasGame event.draws of
        Just draw ->
            case sheetNumber draw of
                Just index ->
                    List.Extra.getAt index event.sheetNames
                        |> Maybe.withDefault ""

                Nothing ->
                    ""

        Nothing ->
            ""


gameScore : Game -> Maybe String
gameScore game =
    let
        intScores =
            List.map (\s -> s.score) game.sides
                |> List.filterMap identity
                |> List.sort
                |> List.reverse

        strScores =
            List.map String.fromInt intScores

        fromScores =
            case game.state of
                GameComplete ->
                    if Maybe.withDefault 0 (List.head intScores) > Maybe.withDefault 0 (List.Extra.getAt 1 intScores) then
                        String.join " > " strScores

                    else
                        String.join " = " strScores

                _ ->
                    ""
    in
    case fromScores of
        "" ->
            Nothing

        score ->
            Just score


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


gamesFromDraws : List Draw -> List Game
gamesFromDraws draws =
    List.map .drawSheets draws
        |> List.concat
        |> List.filterMap identity


findGameById : String -> List Draw -> Maybe Game
findGameById id draws =
    gamesFromDraws draws
        |> List.Extra.find (\g -> g.id == id)


drawWithGameId : String -> List Draw -> Maybe Draw
drawWithGameId id draws =
    let
        hasGame draw =
            let
                matching drawSheet =
                    case drawSheet of
                        Just game ->
                            game.id == id

                        Nothing ->
                            False
            in
            List.any matching draw.drawSheets
    in
    List.Extra.find hasGame draws



-- UPDATE


type Msg
    = Tick Time.Posix
    | NavigateTo String
    | ToggleFullScreen
    | HashChanged Bool String
    | Reload
    | GotTranslations (WebData (List Translation))
    | GotItems (WebData (List Item))
    | IncrementPageBy Int
    | UpdateSearch String
    | UpdateSeasonDelta String
    | GotEvent (WebData Event)
    | GotProduct (WebData Product)
    | ToggleScoringHilight ScoringHilight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newReloadIn =
                    max 0 (model.reloadIn - 1)
            in
            ( { model | reloadIn = newReloadIn }, Cmd.none )

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

        UpdateSeasonDelta val ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | seasonDelta = Maybe.withDefault 0 (String.toInt val) }

                updatedModel =
                    { model | itemFilter = updatedItemFilter model.itemFilter }
            in
            ( updatedModel, getItems model.flags updatedModel.itemFilter )

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


view : Model -> Html Msg
view model =
    let
        viewLoading =
            viewNotReady model.fullScreen (translate model.translations "Loading")
    in
    div
        (List.append
            [ id "curlingio__results"
            , style "position" "relative"
            , style "background-color" "#fff"
            ]
            (if model.fullScreen then
                [ style "width" "100%"
                , style "height" "100%"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "z-index" "100"
                , style "overflow-y" "auto"
                ]

             else
                []
            )
        )
        [ viewReloadButton model
        , div
            [ style "cursor" "pointer"
            , style "position" "absolute"
            , style "top"
                (if model.fullScreen then
                    "10px"

                 else
                    "0"
                )
            , style "right"
                (if model.fullScreen then
                    "10px"

                 else
                    "0"
                )
            , onClick ToggleFullScreen
            , class "d-print-none"
            ]
            [ if model.fullScreen then
                svgExitFullScreen

              else
                svgFullScreen
            ]
        , case model.errorMsg of
            Just errorMsg ->
                viewNotReady model.fullScreen errorMsg

            Nothing ->
                case toRoute model.flags.defaultEventSection model.hash of
                    ItemsRoute ->
                        case model.items of
                            Success items ->
                                viewItems model items

                            Failure error ->
                                viewFetchError model (errorMessage error)

                            _ ->
                                viewLoading

                    ProductRoute id ->
                        case model.product of
                            Success product ->
                                viewProduct model.fullScreen model.translations product

                            Failure error ->
                                viewFetchError model (errorMessage error)

                            _ ->
                                viewLoading

                    EventRoute id nestedRoute ->
                        case model.event of
                            Success event ->
                                viewEvent model nestedRoute event

                            Failure error ->
                                viewFetchError model (errorMessage error)

                            _ ->
                                viewLoading
        ]


viewReloadButton : Model -> Html Msg
viewReloadButton { flags, hash, fullScreen, reloadIn, event } =
    case toRoute flags.defaultEventSection hash of
        EventRoute _ nestedRoute ->
            case event of
                Success event_ ->
                    let
                        reloadEnabled =
                            -- Only if the event is active, end scores are enabled, and we're on a route / screen that is meaningfull to reload.
                            (event_.state == EventStateActive)
                                && event_.endScoresEnabled
                                && not (List.member nestedRoute [ DetailsRoute, TeamsRoute, ReportsRoute ])
                    in
                    if reloadEnabled then
                        button
                            [ style "position" "absolute"
                            , style "top"
                                (if fullScreen then
                                    "5px"

                                 else
                                    "-5px"
                                )
                            , style "right"
                                (if fullScreen then
                                    "50px"

                                 else
                                    "40px"
                                )
                            , onClick Reload
                            , classList
                                [ ( "btn", True )
                                , ( "btn-sm", True )
                                , ( "d-print-none", True )
                                , ( "btn-default", reloadIn > 0 )
                                , ( "btn-link", reloadIn == 0 )
                                ]
                            , disabled (reloadIn > 0)
                            ]
                            [ text
                                (if reloadIn > 0 then
                                    "Reload in " ++ String.fromInt reloadIn ++ "s"

                                 else
                                    "Reload"
                                )
                            ]

                    else
                        text ""

                _ ->
                    text ""

        _ ->
            text ""


viewNotReady : Bool -> String -> Html Msg
viewNotReady fullScreen message =
    p [ classList [ ( "p-3", fullScreen ) ] ] [ text message ]


viewFetchError : Model -> String -> Html Msg
viewFetchError { fullScreen, hash } message =
    div
        [ classList [ ( "p-3", fullScreen ) ] ]
        [ p [] [ text message ]
        , button [ class "btn btn-primary", onClick Reload ] [ text "Reload" ]
        ]


viewItems : Model -> List Item -> Html Msg
viewItems { flags, fullScreen, itemFilter, translations } items =
    let
        viewPaging =
            div [ class "d-flex" ]
                [ if itemFilter.page > 1 then
                    button [ class "btn btn-primary btn-sm mr-1", onClick (IncrementPageBy -1) ] [ text "< Previous" ]

                  else
                    text ""
                , if List.length items >= 10 then
                    button [ class "btn btn-primary btn-sm", onClick (IncrementPageBy 1) ] [ text "Next >" ]

                  else
                    text ""
                ]

        viewSeasonDropDown =
            div [] [ text "season" ]

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

        viewItem item =
            tr []
                ([ td []
                    [ let
                        newPath =
                            case flags.section of
                                ProductsSection ->
                                    "/products/" ++ String.fromInt item.id

                                _ ->
                                    "/events/" ++ String.fromInt item.id
                      in
                      button [ class "btn btn-link p-0 m-0", onClick (NavigateTo newPath), class "btn btn-default" ] [ text item.name ]
                    , small [ class "d-block" ] [ text (Maybe.withDefault "" item.summary) ]
                    ]
                 , td [] [ text (Maybe.withDefault "" item.occursOn) ]
                 , if flags.registration then
                    td []
                        [ text
                            (case item.price of
                                Just price ->
                                    price

                                Nothing ->
                                    ""
                            )
                        ]

                   else
                    text ""
                 ]
                    ++ (if flags.registration then
                            [ td [ class "text-right" ]
                                [ case item.noRegistrationMessage of
                                    Just msg ->
                                        case item.addToCartUrl of
                                            Just url ->
                                                a [ href url, class "btn btn-sm btn-primary" ] [ text msg ]

                                            Nothing ->
                                                text msg

                                    Nothing ->
                                        case ( item.addToCartUrl, item.addToCartText ) of
                                            ( Just addToCartUrl, Just addToCartText ) ->
                                                a [ href addToCartUrl, class "btn btn-sm btn-primary" ] [ text addToCartText ]

                                            _ ->
                                                text ""
                                ]
                            ]

                        else
                            [ td [] [ text (Maybe.withDefault "" item.location) ]
                            ]
                       )
                )

        seasonOptions =
            let
                times =
                    [ ( 1, "next_season" )
                    , ( 0, "this_season" )
                    , ( -1, "Last_season" )
                    , ( -2, "two_seasons_ago" )
                    , ( -3, "three_seasons_ago" )
                    ]

                seasonOption ( delta, label ) =
                    option
                        [ value (String.fromInt delta)
                        , selected (delta == itemFilter.seasonDelta)
                        ]
                        [ text (translate translations label) ]
            in
            List.map seasonOption times
    in
    div
        [ classList [ ( "p-3", fullScreen ) ] ]
        [ div
            [ class "d-flex" ]
            [ div [ class "form-group mr-2" ]
                [ input
                    [ class "form-control"
                    , placeholder (translate translations "search")
                    , value itemFilter.search
                    , onInput UpdateSearch
                    ]
                    []
                ]
            , div [ class "form-group" ]
                [ select
                    [ class "form-control"
                    , value (String.fromInt itemFilter.seasonDelta)
                    , onInput UpdateSeasonDelta
                    ]
                    seasonOptions
                ]
            ]
        , if List.isEmpty filteredItems then
            text ""

          else
            div
                [ class "table-responsive" ]
                [ table
                    [ class "table" ]
                    (List.map viewItem filteredItems)
                ]
        , viewPaging
        ]


viewNoDataForRoute : WebData (List Translation) -> Html Msg
viewNoDataForRoute translations =
    div [] [ text (translate translations "no_data_for_route") ]


viewSponsor : Sponsor -> Html Msg
viewSponsor sponsor =
    div [ class "mb-5 ml-5 pb-4", style "float" "right" ]
        [ div [ class "text-right" ]
            [ case sponsor.url of
                Just url ->
                    a [ href url ]
                        [ img [ alt (Maybe.withDefault "" sponsor.name), src sponsor.logoUrl ] []
                        ]

                Nothing ->
                    img [ alt (Maybe.withDefault "" sponsor.name), src sponsor.logoUrl ] []
            , case sponsor.name of
                Just name ->
                    p [] [ text name ]

                Nothing ->
                    text ""
            ]
        ]


viewProduct : Bool -> WebData (List Translation) -> Product -> Html Msg
viewProduct fullScreen translations product =
    div [ classList [ ( "p-3", fullScreen ) ] ]
        [ h3 [ class "mb-3 d-none d-md-block" ] [ text product.name ]
        , h6 [ class "mb-3 d-md-none" ] [ text product.name ]
        , case product.sponsor of
            Just sponsor ->
                viewSponsor sponsor

            Nothing ->
                text ""
        , case ( product.description, product.summary ) of
            ( Just description, _ ) ->
                p [] [ text description ]

            ( _, Just summary ) ->
                p [] [ text summary ]

            ( Nothing, Nothing ) ->
                text ""
        , case product.totalWithTax of
            Just totalWithTax ->
                div []
                    [ div [ class "font-weight-bold" ] [ text (translate translations "total_with_tax") ]
                    , div [ class "value" ] [ text totalWithTax ]
                    ]

            _ ->
                text ""
        , case ( product.addToCartUrl, product.addToCartText ) of
            ( Just addToCartUrl, Just addToCartText ) ->
                p [ class "mt-2 mb-5" ]
                    [ a [ class "btn btn-primary", href addToCartUrl ] [ text addToCartText ]
                    ]

            _ ->
                text ""
        , if not (List.isEmpty product.potentialDiscounts) then
            div []
                [ strong [] [ text (translate translations "potential_discounts") ]
                , ul [] (List.map (\d -> li [] [ text d ]) product.potentialDiscounts)
                ]

          else
            text ""
        ]


viewEvent : Model -> NestedEventRoute -> Event -> Html Msg
viewEvent { flags, translations, scoringHilight, fullScreen } nestedRoute event =
    let
        viewNavItem eventSection =
            let
                isActiveRoute =
                    -- TODO: This needs a bit of work. I don't like the string pattern matching, would prefer patterning on toRoute result.
                    eventSection == eventSectionForRoute nestedRoute

                newPath =
                    "#/events/" ++ String.fromInt event.id ++ "/" ++ eventSection
            in
            li [ class "nav-item" ]
                [ button
                    [ classList
                        [ ( "nav-link", True )
                        , ( "btn", True )
                        , ( "btn-link", True )
                        , ( "active", isActiveRoute )
                        ]
                    , onClick (NavigateTo newPath)
                    ]
                    [ text (translate translations eventSection) ]
                ]
    in
    div [ classList [ ( "p-3", fullScreen ) ] ]
        [ h3 [ class "mb-3 d-none d-md-block" ] [ text event.name ]
        , h6 [ class "mb-3 d-md-none" ] [ text event.name ]
        , ul [ class "nav nav-pills d-print-none mb-3" ]
            (List.map viewNavItem (eventSections flags.excludeEventSections event))
        , h4 [ class "d-none d-print-block" ] [ text (translate translations (eventSectionForRoute nestedRoute)) ]
        , case nestedRoute of
            DetailsRoute ->
                viewDetails translations event

            RegistrationsRoute ->
                viewRegistrations translations event.registrations

            DrawsRoute ->
                viewDrawSchedule translations scoringHilight event

            DrawRoute drawId ->
                case List.Extra.find (\d -> d.id == drawId) event.draws of
                    Just draw ->
                        viewDraw translations scoringHilight event draw

                    Nothing ->
                        viewNoDataForRoute translations

            GameRoute gameId ->
                let
                    findDraw =
                        drawWithGameId gameId event.draws

                    findGame =
                        findGameById gameId event.draws
                in
                case ( findDraw, findGame ) of
                    ( Just draw, Just game ) ->
                        let
                            sheetLabel =
                                sheetNameForGame event game
                        in
                        viewGame translations scoringHilight event sheetLabel True draw game

                    _ ->
                        viewNoDataForRoute translations

            StagesRoute ->
                case List.head event.stages of
                    Just stage ->
                        viewStages translations event stage

                    Nothing ->
                        viewNoDataForRoute translations

            StageRoute id ->
                case List.Extra.find (\s -> s.id == id) event.stages of
                    Just stage ->
                        viewStages translations event stage

                    Nothing ->
                        viewNoDataForRoute translations

            TeamsRoute ->
                viewTeams translations event

            TeamRoute id ->
                case List.Extra.find (\t -> t.id == id) event.teams of
                    Just team ->
                        viewTeam translations event team

                    Nothing ->
                        viewNoDataForRoute translations

            ReportsRoute ->
                viewReports translations event

            ReportRoute report ->
                viewReport translations scoringHilight event report
        ]


viewDetails : WebData (List Translation) -> Event -> Html Msg
viewDetails translations event =
    div []
        [ case event.sponsor of
            Just sponsor ->
                viewSponsor sponsor

            Nothing ->
                text ""
        , case ( event.description, event.summary ) of
            ( Just description, _ ) ->
                p [] [ text description ]

            ( _, Just summary ) ->
                p [] [ text summary ]

            ( Nothing, Nothing ) ->
                text ""
        , case event.totalWithTax of
            Just totalWithTax ->
                div []
                    [ div [ class "font-weight-bold" ] [ text (translate translations "total_with_tax") ]
                    , div [ class "value" ] [ text totalWithTax ]
                    ]

            _ ->
                text ""
        , case ( event.addToCartUrl, event.addToCartText ) of
            ( Just addToCartUrl, Just addToCartText ) ->
                p [ class "mt-2 mb-5" ]
                    [ a [ class "btn btn-primary", href addToCartUrl ] [ text addToCartText ]
                    ]

            _ ->
                text ""
        , div [ class "row" ]
            [ div [ class "col-12 col-md-6 my-2" ]
                [ strong [ class "d-block" ] [ text (translate translations "starts_on") ]
                , div [] [ text event.startsOn ]
                ]
            , div [ class "col-12 col-md-6 my-2" ]
                [ strong [ class "d-block" ] [ text (translate translations "ends_on") ]
                , div [] [ text event.endsOn ]
                ]
            , case event.registrationOpensAt of
                Just registrationOpensAt ->
                    div [ class "col-12 col-md-6 my-2" ]
                        [ strong [ class "d-block" ] [ text (translate translations "registration_opens_at") ]
                        , div [] [ text registrationOpensAt ]
                        ]

                Nothing ->
                    text ""
            , case event.registrationClosesAt of
                Just registrationClosesAt ->
                    div [ class "col-12 col-md-6 my-2" ]
                        [ strong [ class "d-block" ] [ text (translate translations "registration_closes_at") ]
                        , div [] [ text registrationClosesAt ]
                        ]

                Nothing ->
                    text ""
            , div [ class "col-12 col-md-6 my-2" ]
                [ strong [ class "d-block" ] [ text (translate translations "team_restriction") ]
                , div [] [ text event.teamRestriction ]
                ]
            , div [ class "col-12 col-md-6 my-2" ]
                [ strong [ class "d-block" ] [ text (translate translations "age_range") ]
                , div [] [ text event.ageRange ]
                ]
            , if not (List.isEmpty event.potentialDiscounts) then
                div [ class "col-12 mt-3" ]
                    [ strong [] [ text (translate translations "potential_discounts") ]
                    , ul [] (List.map (\d -> li [] [ text d ]) event.potentialDiscounts)
                    ]

              else
                text ""
            ]
        ]


viewRegistrations : WebData (List Translation) -> List Registration -> Html Msg
viewRegistrations translations registrations =
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

        viewRegistration registration =
            let
                positionToString =
                    case registration.position of
                        Just pos ->
                            translate translations pos

                        Nothing ->
                            "-"

                lineupToString =
                    case registration.lineup of
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
            in
            tr []
                [ if hasCurlers then
                    td [] [ text (Maybe.withDefault "-" registration.curlerName) ]

                  else
                    text ""
                , if hasTeamNames then
                    td [] [ text (Maybe.withDefault "-" registration.teamName) ]

                  else
                    text ""
                , if hasSkipNames then
                    td [] [ text (Maybe.withDefault "-" registration.skipName) ]

                  else
                    text ""
                , if hasPositions then
                    td [] [ text positionToString ]

                  else
                    text ""
                , if hasLineups then
                    td [] [ text lineupToString ]

                  else
                    text ""
                ]
    in
    div []
        [ if List.isEmpty registrations then
            p [] [ text (translate translations "no_registrations") ]

          else
            div [ class "table-responsive" ]
                [ table [ class "table table-striped" ]
                    [ thead []
                        [ tr []
                            [ if hasCurlers then
                                th [ style "border-top" "none", style "min-width" "150px" ] [ text (translate translations "curler") ]

                              else
                                text ""
                            , if hasTeamNames then
                                th [ style "border-top" "none", style "min-width" "120px" ] [ text (translate translations "team_name") ]

                              else
                                text ""
                            , if hasSkipNames then
                                th [ style "border-top" "none", style "min-width" "120px" ] [ text (translate translations "skip_name") ]

                              else
                                text ""
                            , if hasPositions then
                                th [ style "border-top" "none" ] [ text (translate translations "position") ]

                              else
                                text ""
                            , if hasLineups then
                                th [ style "border-top" "none", style "min-width" "220px" ] [ text (translate translations "lineup") ]

                              else
                                text ""
                            ]
                        ]
                    , tbody [] (List.map viewRegistration registrations)
                    ]
                ]
        ]


viewDrawSchedule : WebData (List Translation) -> Maybe ScoringHilight -> Event -> Html Msg
viewDrawSchedule translations scoringHilight event =
    let
        isDrawActive : Draw -> Bool
        isDrawActive draw =
            let
                activeGame : Game -> Bool
                activeGame game =
                    List.any (\g -> g.id == game.id && g.state == GameActive) (gamesFromDraws event.draws)
            in
            -- Are there any games in the draw that are active
            List.filterMap identity draw.drawSheets
                |> List.filter activeGame
                |> List.isEmpty
                |> not

        drawLink : Draw -> String -> Html Msg
        drawLink draw label =
            if event.endScoresEnabled then
                button [ class "btn btn-link p-0 m-0", onClick (NavigateTo (drawUrl event.id draw)) ] [ text label ]

            else
                text label

        gameLink : Game -> Html Msg
        gameLink game =
            let
                stateClass =
                    case game.state of
                        GamePending ->
                            "text-primary"

                        GameComplete ->
                            "text-secondary"

                        GameActive ->
                            "text-primary font-weight-bold"

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
                    [ class ("btn btn-link p-0 m-0 " ++ stateClass)
                    , title gameNameWithResult
                    , onClick (NavigateTo (gameUrl event.id game))
                    ]
                    [ text game.name ]

            else
                a
                    [ class stateClass
                    , title gameNameWithResult
                    ]
                    [ text game.name ]

        viewTableSchedule =
            let
                viewDrawRow : Draw -> Html Msg
                viewDrawRow draw =
                    let
                        viewDrawSheet : Maybe Game -> Html Msg
                        viewDrawSheet drawSheet =
                            td [ class "text-center" ]
                                [ case drawSheet of
                                    Just game ->
                                        gameLink game

                                    Nothing ->
                                        text ""
                                ]
                    in
                    tr
                        [ classList
                            [ ( "bg-light", isDrawActive draw )
                            ]
                        ]
                        ([ td [] [ drawLink draw draw.label ] ]
                            ++ [ td [] [ drawLink draw draw.startsAt ]
                               ]
                            ++ List.map viewDrawSheet draw.drawSheets
                        )
            in
            table [ class "table" ]
                [ thead []
                    [ tr []
                        ([ th [ style "border-top" "none", style "min-width" "65px" ] [ text (translate translations "draw") ] ]
                            ++ [ th [ style "border-top" "none", style "min-width" "220px" ] [ text (translate translations "starts_at") ] ]
                            ++ List.map (\sheetName -> th [ class "text-center", style "border-top" "none", style "min-width" "198px" ] [ text sheetName ]) event.sheetNames
                        )
                    ]
                , tbody []
                    (List.map viewDrawRow event.draws)
                ]

        viewListSchedule =
            let
                viewDrawListItem draw =
                    let
                        viewDrawSheet game =
                            li [] [ gameLink game ]
                    in
                    div []
                        [ h6 [ class "mb-0" ] [ drawLink draw (translate translations "draw" ++ " " ++ draw.label) ]
                        , small [] [ drawLink draw draw.startsAt ]
                        , ul [ class "mt-2" ] (List.filterMap identity draw.drawSheets |> List.map viewDrawSheet)
                        ]
            in
            div [] (List.map viewDrawListItem event.draws)
    in
    div []
        [ div [ class "table-responsive d-none d-md-block d-print-none" ] [ viewTableSchedule ]
        , div [ class "d-md-none d-print-block" ] [ viewListSchedule ]
        ]


viewTeams : WebData (List Translation) -> Event -> Html Msg
viewTeams translations event =
    let
        hasCoaches =
            List.any (\t -> t.coach /= Nothing) event.teams

        hasAffiliations =
            List.any (\t -> t.affiliation /= Nothing) event.teams

        hasLocations =
            List.any (\t -> t.location /= Nothing) event.teams

        viewTeamRow team =
            tr []
                [ td []
                    [ if teamHasDetails team then
                        button [ class "btn btn-link p-0 m-0", onClick (NavigateTo (teamUrl event.id team)) ] [ text team.name ]

                      else
                        -- No point in linking to team details if there are no more details.
                        text team.name
                    ]
                , if hasCoaches then
                    td [] [ text (Maybe.withDefault "-" team.coach) ]

                  else
                    text ""
                , if hasAffiliations then
                    td [] [ text (Maybe.withDefault "-" team.affiliation) ]

                  else
                    text ""
                , if hasLocations then
                    td [] [ text (Maybe.withDefault "-" team.location) ]

                  else
                    text ""
                ]
    in
    div [ class "table-responsive" ]
        [ table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ style "min-width" "150px", style "border-top" "none" ] [ text (translate translations "team") ]
                    , if hasCoaches then
                        th [ style "min-width" "150px", style "border-top" "none" ] [ text (translate translations "coach") ]

                      else
                        text ""
                    , if hasAffiliations then
                        th [ style "min-width" "150px", style "border-top" "none" ] [ text (translate translations "affiliation") ]

                      else
                        text ""
                    , if hasLocations then
                        th [ style "min-width" "150px", style "border-top" "none" ] [ text (translate translations "location") ]

                      else
                        text ""
                    ]
                ]
            , tbody [] (List.map viewTeamRow event.teams)
            ]
        ]


viewStages : WebData (List Translation) -> Event -> Stage -> Html Msg
viewStages translations event onStage =
    let
        games =
            List.filter (\g -> g.stageId == onStage.id || Just g.stageId == onStage.parentId) (gamesFromDraws event.draws)

        teams =
            teamsWithGames event.teams games

        viewStageLink stage =
            li [ class "nav-item" ]
                [ button
                    [ onClick (NavigateTo (stageUrl event.id stage))
                    , classList
                        [ ( "nav-link", True )
                        , ( "btn", True )
                        , ( "btn-link", True )
                        , ( "active", stage.id == onStage.id )
                        ]
                    ]
                    [ text stage.name ]
                ]

        viewRoundRobin =
            let
                teamResults =
                    List.filter (\g -> g.stageId == onStage.id) games
                        |> teamResultsForGames onStage teams
                        |> teamResultsRankedByPoints

                hasTies =
                    List.any (\teamResult -> teamResult.ties > 0) teamResults

                viewRow teamResult =
                    tr []
                        [ td []
                            [ if teamHasDetails teamResult.team then
                                button
                                    [ class "btn btn-link p-0 m-0"
                                    , onClick (NavigateTo (teamUrl event.id teamResult.team))
                                    ]
                                    [ text teamResult.team.name ]

                              else
                                text teamResult.team.name
                            ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.gamesPlayed) ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.wins) ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.losses) ]
                        , if hasTies then
                            td [ class "text-right" ] [ text (String.fromInt teamResult.ties) ]

                          else
                            text ""
                        , td [ class "text-right" ] [ text (String.fromFloat teamResult.points) ]
                        ]
            in
            div [ class "table-responsive" ]
                [ table [ class "table table-striped" ]
                    [ thead []
                        [ tr []
                            [ th [ style "min-width" "150px", style "border-top" "none" ] [ text (translate translations "team") ]
                            , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "games") ]
                            , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "wins") ]
                            , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "losses") ]
                            , if hasTies then
                                th [ class "text-right", style "border-top" "none" ] [ text (translate translations "ties") ]

                              else
                                text ""
                            , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "points") ]
                            ]
                        ]
                    , tbody [] (List.map viewRow teamResults)
                    ]
                ]

        viewBracket =
            let
                viewGroup group =
                    let
                        gamesForGroup =
                            games
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
                                        viewSide : Int -> Side -> Html Msg
                                        viewSide position side =
                                            let
                                                label =
                                                    case ( side.teamId, side.winnerId, side.loserId ) of
                                                        ( Just id, _, _ ) ->
                                                            case List.Extra.find (\t -> t.id == id) teams of
                                                                Just team ->
                                                                    team.name

                                                                Nothing ->
                                                                    "TBD"

                                                        ( _, Just winnerId, _ ) ->
                                                            "W: "
                                                                ++ (List.Extra.find (\g -> g.id == winnerId) games
                                                                        |> Maybe.map .name
                                                                        |> Maybe.withDefault "TBD"
                                                                   )

                                                        ( _, _, Just loserId ) ->
                                                            "L: "
                                                                ++ (List.Extra.find (\g -> g.id == loserId) games
                                                                        |> Maybe.map .name
                                                                        |> Maybe.withDefault "TBD"
                                                                   )

                                                        _ ->
                                                            "TBD"
                                            in
                                            div
                                                ([ style "padding-left" "3px"
                                                 , classList [ ( "font-weight-bold", side.result == Just SideResultWon ) ]
                                                 ]
                                                    ++ (if position == 0 then
                                                            [ style "border-bottom" "solid 1px rgba(0, 0, 0, 0.3)" ]

                                                        else
                                                            []
                                                       )
                                                )
                                                [ text label ]
                                    in
                                    div
                                        [ class "d-flex justify-content-center"
                                        , style "position" "absolute"
                                        , style "flex-direction" "column"
                                        , style "width" "180px"
                                        , style "height" "70px"
                                        , style "overflow" "hidden"
                                        , style "background-color" "rgba(0, 0, 0, 0.1)"
                                        , style "border" "solid 1px rgba(0, 0, 0, 0.3)"
                                        , style "left" (String.fromInt (coords.col * gridSize) ++ "px")
                                        , style "top" (String.fromInt (coords.row * gridSize) ++ "px")
                                        ]
                                        [ div
                                            [ class "d-flex w-100"
                                            , style "height" "20px"
                                            , style "background-color" "rgba(0, 0, 0, 0.3)"
                                            , style "z-index" "200"
                                            ]
                                            [ button
                                                [ class "d-block flex-fill btn btn-link p-0 m-0"
                                                , style "font-size" "12px"
                                                , style "padding-left" "3px"
                                                , style "color" "white"
                                                , style "overflow" "hidden"
                                                , onClick (NavigateTo (gameUrl event.id game))
                                                ]
                                                [ text game.name ]
                                            ]
                                        , div [] (List.indexedMap viewSide game.sides)
                                        ]

                                Nothing ->
                                    text ""

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
                            div [ class "group-lines" ]
                                [ viewSvgConnector ((colsForGames + 1) * gridSize) ((rowsForGroup - 1) * gridSize) lineConnectors
                                ]
                    in
                    div
                        [ class "mt-4" ]
                        [ h5 [ class "mb-3" ] [ text ("☷ " ++ group.name) ]
                        , div [ style "position" "relative" ]
                            [ viewSvgConnectors
                            , div [ class "games" ] (List.map viewGroupGame gamesForGroup)
                            ]
                        ]
            in
            case onStage.groups of
                Just groups ->
                    div [] (List.map viewGroup groups)

                Nothing ->
                    div [] [ text "No groups" ]
    in
    div []
        [ div [ class "nav nav-tabs mb-3" ] (List.map viewStageLink event.stages)
        , case onStage.stageType of
            RoundRobin ->
                viewRoundRobin

            Bracket ->
                viewBracket
        ]



-- REPORTS


viewReports : WebData (List Translation) -> Event -> Html Msg
viewReports translations event =
    let
        hasAttendance =
            (List.map .attendance event.draws |> List.sum) > 0

        attendanceLink =
            "/events/" ++ String.fromInt event.id ++ "/reports/attendance"

        scoringAnalysisLink =
            "/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"

        scoringAnalysisByHammerLink =
            "/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis_by_hammer"

        teamRostersLink =
            "/events/" ++ String.fromInt event.id ++ "/reports/team_rosters"

        competitionMatrixLink =
            "/events/" ++ String.fromInt event.id ++ "/reports/competition_matrix"
    in
    ul []
        [ if hasAttendance then
            li [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo attendanceLink) ] [ text (translate translations "attendance") ] ]

          else
            text ""
        , li [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo competitionMatrixLink) ] [ text (translate translations "competition_matrix") ] ]
        , if event.endScoresEnabled then
            li [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo scoringAnalysisLink) ] [ text (translate translations "scoring_analysis") ] ]

          else
            text ""
        , if event.endScoresEnabled then
            li [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo scoringAnalysisByHammerLink) ] [ text (translate translations "scoring_analysis_by_hammer") ] ]

          else
            text ""
        , li [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo teamRostersLink) ] [ text (translate translations "team_rosters") ] ]
        ]


viewDraw : WebData (List Translation) -> Maybe ScoringHilight -> Event -> Draw -> Html Msg
viewDraw translations scoringHilight event draw =
    let
        viewDrawSheet game =
            case game of
                Just game_ ->
                    let
                        sheetLabel =
                            sheetNameForGame event game_
                    in
                    viewGame translations scoringHilight event sheetLabel False draw game_

                Nothing ->
                    text ""
    in
    div []
        ([ nav
            [ attribute "aria-label" "breadcrumb" ]
            [ ol [ class "breadcrumb" ]
                [ li
                    [ class "breadcrumb-item active"
                    , attribute "aria-current" "page"
                    ]
                    [ text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt) ]
                ]
            ]
         ]
            ++ List.map viewDrawSheet draw.drawSheets
        )


viewGame : WebData (List Translation) -> Maybe ScoringHilight -> Event -> String -> Bool -> Draw -> Game -> Html Msg
viewGame translations scoringHilight event sheetLabel detailed draw game =
    let
        maxNumberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        viewEndHeader endNumber =
            th [ class "text-center", style "width" "50px" ] [ text (String.fromInt endNumber) ]

        teamName side =
            findTeamForSide event.teams side
                |> Maybe.map .name
                |> Maybe.withDefault "TBD"

        viewSide side =
            let
                wonOrTied =
                    (side.result == Just SideResultWon)
                        || (side.result == Just SideResultTied)

                viewEndScore endNumber =
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
                    td
                        [ classList
                            [ ( "text-center", True )
                            , ( "bg-light", hasHammer && not isHilighted )
                            , ( "bg-secondary", isHilighted )
                            , ( "text-light", isHilighted )
                            , ( "font-weight-bolder", isHilighted )
                            ]
                        ]
                        [ text endScoreStr ]

                rockStyles =
                    [ style "border-bottom"
                        ("solid 3px "
                            ++ colorNameToRGB
                                (if side.topRock then
                                    event.topRock

                                 else
                                    event.botRock
                                )
                        )
                    ]
            in
            tr []
                ([ td
                    [ classList
                        [ ( "font-weight-bold", wonOrTied )
                        , ( "text-primary", side.firstHammer && scoringHilight == Just HilightHammers )
                        ]
                    ]
                    [ span rockStyles
                        [ text
                            (teamName side
                                ++ (if side.firstHammer then
                                        " *"

                                    else
                                        ""
                                   )
                            )
                        ]
                    ]
                 ]
                    ++ List.map viewEndScore (List.range 1 maxNumberOfEnds)
                    ++ [ td
                            [ class "text-center" ]
                            [ if wonOrTied then
                                u [ class "font-weight-bold" ] [ text (String.fromInt (List.sum side.endScores)) ]

                              else
                                text (String.fromInt (List.sum side.endScores))
                            ]
                       ]
                )

        viewGameCaption =
            let
                label =
                    i []
                        [ case game.state of
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
                        ]
            in
            if detailed then
                label

            else
                button [ class "btn btn-link p-0 m-0", onClick (NavigateTo gamePath) ] [ label ]

        viewGameHilight =
            case scoringHilight of
                Just hilight ->
                    button
                        [ style "cursor" "pointer"
                        , class "btn btn-sm btn-secondary d-print-none"
                        , onClick (ToggleScoringHilight hilight)
                        ]
                        [ span []
                            [ text
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
                            ]
                        ]

                Nothing ->
                    text ""

        gamePath =
            gameUrl event.id game

        drawPath =
            drawUrl event.id draw
    in
    div []
        [ if detailed then
            nav [ attribute "aria-label" "breadcrumb" ]
                [ ol [ class "breadcrumb" ]
                    [ li [ class "breadcrumb-item" ]
                        [ button
                            [ class "btn btn-link p-0 m-0 d-inline align-baseline", onClick (NavigateTo drawPath) ]
                            [ text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt) ]
                        ]
                    , li
                        [ class "breadcrumb-item active d-none d-md-inline-block"
                        , attribute "aria-current" "page"
                        ]
                        [ text game.name ]
                    ]
                ]

          else
            text ""
        , div [ class "table-responsive mt-4" ]
            [ table [ class "table table-bordered mb-0" ]
                [ caption []
                    [ div [ class "d-flex justify-content-between" ]
                        [ viewGameCaption
                        , viewGameHilight
                        ]
                    ]
                , thead []
                    [ tr []
                        ([ th [ style "min-width" "160px" ]
                            [ span [] [ text sheetLabel ]
                            , if detailed then
                                text ""

                              else
                                button
                                    [ class "btn btn-link p-0 m-0 d-inline align-baseline"
                                    , onClick (NavigateTo gamePath)
                                    ]
                                    [ small [ class "ml-3" ] [ text game.name ]
                                    ]
                            ]
                         ]
                            ++ List.map viewEndHeader (List.range 1 maxNumberOfEnds)
                            ++ [ th [ class "text-center", style "width" "64px" ] [ text "Total" ] ]
                        )
                    ]
                , tbody [] (List.map viewSide game.sides)
                ]
            ]
        , if detailed then
            div [ class "mt-3" ]
                [ List.map (findTeamForSide event.teams) game.sides
                    |> List.filterMap identity
                    |> viewReportScoringAnalysis translations scoringHilight event
                ]

          else
            text ""
        ]


viewTeam : WebData (List Translation) -> Event -> Team -> Html Msg
viewTeam translations event team =
    let
        hasDraws =
            not (List.isEmpty event.draws)

        viewTeamLineup =
            let
                hasDelivery =
                    List.any (\c -> c.delivery /= Nothing) team.lineup

                hasClubName =
                    List.any (\c -> c.clubName /= Nothing) team.lineup

                hasClubCity =
                    List.any (\c -> c.clubCity /= Nothing) team.lineup

                hasPhotoUrl =
                    List.any (\c -> c.photoUrl /= Nothing) team.lineup

                viewTeamCurler curler =
                    div [ class "col-12 col-md-6 col-lg-4 col-xl-3 mb-4" ]
                        [ div [ class "card" ]
                            [ if hasPhotoUrl then
                                div [ class "d-flex justify-content-center d-print-none", style "height" "150px" ]
                                    [ case curler.photoUrl of
                                        Just photoUrl ->
                                            img
                                                [ class "card-img-top mt-2"
                                                , style "width" "auto"
                                                , style "max-height" "100%"
                                                , src photoUrl
                                                ]
                                                []

                                        Nothing ->
                                            div [ class "mt-3" ] [ svgNoImage ]
                                    ]

                              else
                                text ""
                            , div [ class "card-body" ]
                                [ h5 [ class "card-title mb-1" ] [ text curler.name ]
                                , p [ class "card-text" ]
                                    [ span [] [ text (teamPositionToString translations curler.position) ]
                                    , if curler.skip then
                                        sup [] [ text (" " ++ translate translations "skip") ]

                                      else
                                        text ""
                                    , if hasDelivery then
                                        small [ class "d-block" ] [ text (translate translations "delivery" ++ ": " ++ deliveryToString translations curler.delivery) ]

                                      else
                                        text ""
                                    , if hasClubName then
                                        small [ class "d-block" ] [ text (Maybe.withDefault "-" curler.clubName) ]

                                      else
                                        text ""
                                    , if hasClubCity then
                                        small [ class "d-block" ] [ text (Maybe.withDefault "-" curler.clubCity) ]

                                      else
                                        text ""
                                    ]
                                ]
                            ]
                        ]
            in
            div [ class "row" ] (List.map viewTeamCurler team.lineup)

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
            div [ class "row mb-4" ]
                [ if hasTeamContactInfo then
                    div [ class "col-12 col-md-6 table-responsive" ]
                        [ table [ class "table" ]
                            [ tr []
                                [ th [ class "w-25", style "min-width" "140px" ] [ text (translate translations "contact_name") ]
                                , td [ class "w-25", style "min-width" "160px" ] [ text (Maybe.withDefault "-" team.contactName) ]
                                ]
                            , tr []
                                [ th [] [ text (translate translations "contact_email") ]
                                , td [] [ text (Maybe.withDefault "-" team.contactEmail) ]
                                ]
                            , tr []
                                [ th [] [ text (translate translations "contact_phone") ]
                                , td [] [ text (Maybe.withDefault "-" team.contactPhone) ]
                                ]
                            ]
                        ]

                  else
                    text ""
                , if hasTeamOtherInfo then
                    div [ class "col-12 col-md-6 table-responsive" ]
                        [ table [ class "table" ]
                            [ tr []
                                [ th [ class "w-25", style "min-width" "100px" ] [ text (translate translations "coach") ]
                                , td [ class "w-25", style "min-width" "160px" ] [ text (Maybe.withDefault "-" team.coach) ]
                                ]
                            , tr []
                                [ th [] [ text (translate translations "affiliation") ]
                                , td [] [ text (Maybe.withDefault "-" team.affiliation) ]
                                ]
                            , tr []
                                [ th [] [ text (translate translations "location") ]
                                , td [] [ text (Maybe.withDefault "-" team.location) ]
                                ]
                            ]
                        ]

                  else
                    text ""
                ]

        viewTeamSchedule =
            let
                hasSideForTeam : Game -> Bool
                hasSideForTeam game =
                    List.any (\s -> s.teamId == Just team.id) game.sides

                draws =
                    let
                        hasGameForTeam draw =
                            List.filterMap identity draw.drawSheets
                                |> List.filter hasSideForTeam
                                |> List.isEmpty
                                |> not
                    in
                    List.filter hasGameForTeam event.draws

                viewTeamDraw : Draw -> List (Html Msg)
                viewTeamDraw draw =
                    let
                        games =
                            List.filterMap identity draw.drawSheets
                                |> List.filter hasSideForTeam

                        viewTeamGame : Game -> Html Msg
                        viewTeamGame game =
                            let
                                drawPath =
                                    drawUrl event.id draw

                                gamePath =
                                    gameUrl event.id game

                                resultLabel =
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
                                            button [ class "btn btn-link p-0 m-0", onClick (NavigateTo gamePath) ] [ text t ]

                                        Nothing ->
                                            text "-"

                                gameScoreLabel =
                                    case gameScore game of
                                        Just score ->
                                            button [ class "btn btn-link p-0 m-0", onClick (NavigateTo gamePath) ] [ text score ]

                                        Nothing ->
                                            text ""

                                opponentLabel =
                                    let
                                        opponent =
                                            List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                                                |> Maybe.andThen (findTeamForSide event.teams)
                                    in
                                    case opponent of
                                        Just oppo ->
                                            let
                                                oppoPath =
                                                    teamUrl event.id oppo
                                            in
                                            button [ class "btn btn-link m-0 p-0", onClick (NavigateTo oppoPath) ] [ text oppo.name ]

                                        Nothing ->
                                            text ""
                            in
                            tr []
                                [ td [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo drawPath) ] [ text draw.label ] ]
                                , td [] [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo drawPath) ] [ text draw.startsAt ] ]
                                , td [] [ resultLabel ]
                                , td [] [ gameScoreLabel ]
                                , td [] [ opponentLabel ]
                                ]
                    in
                    List.map viewTeamGame games
            in
            div [ class "table-responsive" ]
                [ table [ class "table" ]
                    [ thead []
                        [ tr []
                            [ th [ style "min-width" "100px" ] [ text "Draw" ]
                            , th [ style "min-width" "220px" ] []
                            , th [ style "min-width" "170px" ] [ text "Result" ]
                            , th [ style "min-width" "80px" ] [ text "Score" ]
                            , th [ style "min-width" "150px" ] [ text "Opponent" ]
                            ]
                        ]
                    , tbody [] (List.map viewTeamDraw draws |> List.concat)
                    ]
                ]
    in
    div []
        [ h4 [ class "mb-3" ] [ text team.name ]
        , viewTeamLineup
        , viewTeamInfo
        , if hasDraws then
            viewTeamSchedule

          else
            text ""
        ]


viewReportScoringAnalysis : WebData (List Translation) -> Maybe ScoringHilight -> Event -> List Team -> Html Msg
viewReportScoringAnalysis translations scoringHilight event teams =
    let
        isHilighted onHilight =
            scoringHilight == Just onHilight

        isForGame =
            List.length teams == 2

        fullReportUrl =
            "#/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"
    in
    div
        [ class "table-responsive" ]
        [ div
            [ class "d-flex justify-content-between" ]
            [ h4 [ class "mb-3" ] [ text (translate translations "scoring_analysis") ]
            , if isForGame then
                button
                    [ class "btn btn-link p-0 m-0"
                    , onClick (NavigateTo fullReportUrl)
                    ]
                    [ small [] [ text (translate translations "full_report" ++ " →") ]
                    ]

              else
                text ""
            ]
        , table [ class "table table-sm table-bordered small" ]
            ([ caption []
                [ ol [ class "pl-3" ]
                    [ li [] [ text (" LSFE - " ++ translate translations "last_shot_in_first_end") ]
                    , li [] [ text ("SE - " ++ translate translations "stolen_ends") ]
                    , li [] [ text ("BE - " ++ translate translations "blank_ends") ]
                    , li [] [ text ("SP - " ++ translate translations "stolen_points") ]
                    ]
                ]
             , thead [ class "thead-light" ]
                [ tr []
                    [ th [ style "min-width" "80px" ] [ text "Team" ]
                    , th [ class "text-center" ] [ text "Games" ]
                    , th [ class "text-center" ] [ text "Ends" ]
                    , th [] []
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightHammers) )
                            , ( "text-danger", isForGame && isHilighted HilightHammers )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightHammers)
                        ]
                        [ span [] [ text "LSFE" ], sup [] [ text "1" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightStolenEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightStolenEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightStolenEnds)
                        ]
                        [ span [] [ text "SE" ], sup [] [ text "2" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightBlankEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightBlankEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightBlankEnds)
                        ]
                        [ span [] [ text "BE" ], sup [] [ text "3" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight1PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight1PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight1PointEnds)
                        ]
                        [ text "1pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight2PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight2PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight2PointEnds)
                        ]
                        [ text "2pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight3PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight3PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight3PointEnds)
                        ]
                        [ text "3pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight4PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight4PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight4PointEnds)
                        ]
                        [ text "4pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight5PlusPointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight5PlusPointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight5PlusPointEnds)
                        ]
                        [ text "5pt+" ]
                    , th [ class "text-right" ] [ text "Tot" ]
                    , th [ class "text-right" ] [ text "Avg" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightStolenEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightStolenEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightStolenEnds)
                        ]
                        [ span [] [ text "SP" ], sup [] [ text "4" ] ]
                    ]
                ]
             ]
                ++ List.map (viewTeamScoringAnalysis event) teams
            )
        ]


viewTeamScoringAnalysis : Event -> Team -> Html Msg
viewTeamScoringAnalysis event team =
    let
        games =
            let
                participatedIn sides =
                    List.any (\s -> s.teamId == Just team.id) sides
            in
            List.filter (\g -> g.state /= GamePending) (gamesFromDraws event.draws)
                |> List.filter (\g -> participatedIn g.sides)

        sidesFor =
            List.map .sides games
                |> List.concat
                |> List.filter (\s -> s.teamId == Just team.id)

        sidesAgainst =
            List.map .sides games
                |> List.concat
                |> List.filter (\s -> s.teamId /= Just team.id)

        endsFor =
            List.map .endScores sidesFor
                |> List.concat

        endsAgainst =
            List.map .endScores sidesAgainst
                |> List.concat

        firstHammerCountFor =
            List.filter (\s -> s.firstHammer == True) sidesFor
                |> List.length

        firstHammerCountAgainst =
            List.filter (\s -> s.firstHammer == True) sidesAgainst
                |> List.length

        blankEndsFor =
            List.filter (\i -> i /= 0) endsFor
                |> List.length

        blankEndsAgainst =
            List.filter (\i -> i == 0) endsAgainst
                |> List.length

        onePointEndsFor =
            List.filter (\i -> i == 1) endsFor
                |> List.length

        onePointEndsAgainst =
            List.filter (\i -> i == 1) endsAgainst
                |> List.length

        twoPointEndsFor =
            List.filter (\i -> i == 2) endsFor
                |> List.length

        twoPointEndsAgainst =
            List.filter (\i -> i == 2) endsAgainst
                |> List.length

        threePointEndsFor =
            List.filter (\i -> i == 3) endsFor
                |> List.length

        threePointEndsAgainst =
            List.filter (\i -> i == 3) endsAgainst
                |> List.length

        fourPointEndsFor =
            List.filter (\i -> i == 4) endsFor
                |> List.length

        fourPointEndsAgainst =
            List.filter (\i -> i == 4) endsAgainst
                |> List.length

        moreThanFourPointEndsFor =
            List.filter (\i -> i > 4) endsFor
                |> List.length

        moreThanFourPointEndsAgainst =
            List.filter (\i -> i > 4) endsAgainst
                |> List.length

        totalPointsFor =
            List.sum endsFor

        totalPointsAgainst =
            List.sum endsAgainst

        averagePointsFor =
            -- total points divided by number of ends. We multiple by 100 then round then divide by 100 so that we get 2 decimal places.
            toFloat (round ((toFloat totalPointsFor / toFloat (List.length endsFor)) * 100)) / 100

        averagePointsAgainst =
            -- see averagePointsFor
            toFloat (round ((toFloat totalPointsAgainst / toFloat (List.length endsAgainst)) * 100)) / 100

        stolenEndsCount for =
            List.length (stolenEnds for games team)

        stolenPoints for =
            List.sum (stolenEnds for games team)
    in
    tbody []
        [ tr []
            [ td [ rowspan 2 ]
                [ button
                    [ class "d-block mt-3 btn btn-link mb-0 p-0"
                    , onClick (NavigateTo (teamUrl event.id team))
                    ]
                    [ span [ class "small" ] [ text team.name ] ]
                ]
            , td [ rowspan 2, class "text-center" ] [ div [ class "mt-3" ] [ text (String.fromInt (List.length games)) ] ]
            , td [ rowspan 2, class "text-center" ] [ div [ class "mt-3" ] [ text (String.fromInt (List.length endsFor)) ] ]
            , td [ class "pl-2" ] [ text "For" ]
            , td [ class "text-right" ] [ text (String.fromInt firstHammerCountFor) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenEndsCount True)) ]
            , td [ class "text-right" ] [ text (String.fromInt blankEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt onePointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt twoPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt threePointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt fourPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt moreThanFourPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt totalPointsFor) ]
            , td [ class "text-right" ] [ text (String.fromFloat averagePointsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenPoints True)) ]
            ]
        , tr []
            [ td [ class "pl-2" ] [ text "Against" ]
            , td [ class "text-right" ] [ text (String.fromInt firstHammerCountAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenEndsCount False)) ]
            , td [ class "text-right" ] [ text (String.fromInt blankEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt onePointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt twoPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt threePointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt fourPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt moreThanFourPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt totalPointsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromFloat averagePointsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenPoints False)) ]
            ]
        ]


viewReportScoringAnalysisByHammer : WebData (List Translation) -> Event -> Html Msg
viewReportScoringAnalysisByHammer translations event =
    let
        games : List Game
        games =
            -- Not pending
            List.filter (\g -> g.state /= GamePending) (gamesFromDraws event.draws)

        viewByHammer : Bool -> Html Msg
        viewByHammer withHammer =
            let
                teams =
                    teamsWithGames event.teams games

                viewTeamByHammer team =
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

                        blankEndsFor =
                            List.filter (\e -> e == 0) endsFor
                                |> List.length

                        blankEndsForPercent =
                            round ((toFloat blankEndsFor / toFloat (endsFor |> List.length)) * 100)

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
                    tr []
                        [ td []
                            [ button [ class "btn btn-link p-0 m-0", onClick (NavigateTo (teamUrl event.id team)) ] [ text team.name ] ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt gamesCount) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt endsCount) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt blankEndsFor) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt blankEndsForPercent) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt stolenEndsAgainst) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt stolenEndsAgainstPercent) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt singlePointsFor) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt singlePointsForPercent) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt multiPointsFor) ]
                        , td [ class "text-right" ]
                            [ text (String.fromInt multiPointsForPercent) ]
                        ]
            in
            div [ class "table-responsive" ]
                [ table [ class "table table-striped mt-3" ]
                    [ thead []
                        [ tr []
                            [ th [ colspan 3, style "border-top" "none", style "min-width" "250px" ]
                                [ text
                                    (translate translations
                                        (if withHammer then
                                            "with_hammer"

                                         else
                                            "without_hammer"
                                        )
                                    )
                                ]
                            , th [ colspan 2, class "text-right", style "border-top" "none", style "min-width" "140px" ] [ text (translate translations "blank_ends_for") ]
                            , th [ colspan 2, class "text-right", style "border-top" "none", style "min-width" "180px" ] [ text (translate translations "stolen_ends_against") ]
                            , th [ colspan 2, class "text-right", style "border-top" "none", style "min-width" "155px" ] [ text (translate translations "single_points_for") ]
                            , th [ colspan 2, class "text-right", style "border-top" "none", style "min-width" "145px" ] [ text (translate translations "multi_points_for") ]
                            ]
                        , tr []
                            [ th [ style "min-width" "150px" ] [ text (translate translations "team") ]
                            , th [ class "text-right", style "min-width" "60px" ] [ text (translate translations "games") ]
                            , th [ class "text-right", style "min-width" "60px" ] [ text (translate translations "ends") ]
                            , th [ class "text-right" ] [ text "#" ]
                            , th [ class "text-right" ] [ text "%" ]
                            , th [ class "text-right" ] [ text "#" ]
                            , th [ class "text-right" ] [ text "%" ]
                            , th [ class "text-right" ] [ text "#" ]
                            , th [ class "text-right" ] [ text "%" ]
                            , th [ class "text-right" ] [ text "#" ]
                            , th [ class "text-right" ] [ text "%" ]
                            ]
                        ]
                    , tbody [] (List.map viewTeamByHammer teams)
                    ]
                ]
    in
    div []
        [ h4 [] [ text (translate translations "scoring_analysis_by_hammer") ]
        , viewByHammer True
        , viewByHammer False
        ]


viewReportTeamRosters : WebData (List Translation) -> List Team -> Html Msg
viewReportTeamRosters translations teams =
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
            let
                viewTeamRosterCurler curler =
                    tr []
                        [ td [] [ text curler.name ]
                        , td []
                            [ text (teamPositionToString translations curler.position)
                            , if curler.skip then
                                sup [ class "ml-1" ] [ text (translate translations "skip") ]

                              else
                                text ""
                            ]
                        , if hasDelivery then
                            td [] [ text (deliveryToString translations curler.delivery) ]

                          else
                            text ""
                        ]
            in
            [ thead [ class "thead-light" ]
                [ tr [] [ th [ colspan 3 ] [ text team.name ] ]
                ]
            , tbody [] (List.map viewTeamRosterCurler team.lineup)
            ]
    in
    div []
        [ h4 [ class "mb-3" ] [ text (translate translations "team_rosters") ]
        , div [ class "table-responsive" ]
            [ table [ class "table" ] (List.map viewTeamRoster teams |> List.concat)
            ]
        ]


viewReportCompetitionMatrix : WebData (List Translation) -> Event -> Html Msg
viewReportCompetitionMatrix translations event =
    let
        viewStageMatrix stage =
            let
                -- Only the games participating in this stage.
                games =
                    List.filter (\g -> g.stageId == stage.id) (gamesFromDraws event.draws)

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
                            List.any inGame games
                    in
                    -- Filter the event.teams to include only those teams involved in games that are in this stage.
                    List.filter teamIncluded event.teams

                viewTeamColumn team =
                    td [ class "text-center", style "min-width" "80px" ] [ text team.shortName ]

                viewTeamRow team =
                    tr []
                        ([ td [ class "text-center", style "min-width" "80px" ] [ text team.shortName ]
                         ]
                            ++ List.map (viewTeamCell team) teams
                        )

                viewTeamCell teamA teamB =
                    let
                        teamIdsForGame g =
                            List.map .teamId g.sides
                                |> List.filterMap identity

                        matchingGame =
                            List.Extra.find (\g -> [ teamA.id, teamB.id ] == teamIdsForGame g || [ teamB.id, teamA.id ] == teamIdsForGame g) games
                    in
                    case matchingGame of
                        Just game ->
                            td [ class "text-center" ]
                                [ case gameScore game of
                                    Just score ->
                                        let
                                            gamePath =
                                                gameUrl event.id game
                                        in
                                        button [ class "btn btn-link p-0 m-0", onClick (NavigateTo gamePath) ] [ text score ]

                                    Nothing ->
                                        text ""
                                ]

                        Nothing ->
                            td [ class "bg-light" ] []
            in
            div [ class "table-responsive mt-3" ]
                [ h5 [] [ text stage.name ]
                , table [ class "table table-bordered" ]
                    ([ tr []
                        ([ td [] [] ] ++ List.map viewTeamColumn teams)
                     ]
                        ++ List.map viewTeamRow teams
                    )
                ]
    in
    div []
        ([ h4 [] [ text (translate translations "competition_matrix") ] ]
            ++ (List.filter (\s -> s.stageType == RoundRobin) event.stages |> List.map viewStageMatrix)
        )


viewReportAttendance : WebData (List Translation) -> List Draw -> Html Msg
viewReportAttendance translations draws =
    let
        viewAttendanceRow idx draw =
            let
                sumToCurrent =
                    List.take (idx + 1) draws
                        |> List.map (\d -> d.attendance)
                        |> List.sum
            in
            tr []
                [ td [] [ text draw.label ]
                , td []
                    [ text (String.fromInt draw.attendance) ]
                , td []
                    [ text (String.fromInt sumToCurrent) ]
                ]
    in
    div []
        [ h3 [ class "mb-3" ] [ text (translate translations "attendance") ]
        , div [ class "table-responsive" ]
            [ table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        [ th [] [ text (translate translations "draw") ]
                        , th [] [ text (translate translations "attendance") ]
                        , th [] [ text (translate translations "total") ]
                        ]
                    ]
                , tbody [] (List.indexedMap viewAttendanceRow draws)
                ]
            ]
        ]


viewReport : WebData (List Translation) -> Maybe ScoringHilight -> Event -> String -> Html Msg
viewReport translations scoringHilight event report =
    case report of
        "attendance" ->
            viewReportAttendance translations event.draws

        "scoring_analysis" ->
            let
                games =
                    gamesFromDraws event.draws
            in
            viewReportScoringAnalysis translations scoringHilight event (teamsWithGames event.teams games)

        "scoring_analysis_by_hammer" ->
            viewReportScoringAnalysisByHammer translations event

        "team_rosters" ->
            viewReportTeamRosters translations event.teams

        "competition_matrix" ->
            viewReportCompetitionMatrix translations event

        _ ->
            viewNoDataForRoute translations



-- PORTS


port navigateTo : String -> Cmd msg


port hashChangeReceiver : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ hashChangeReceiver (HashChanged False)
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
