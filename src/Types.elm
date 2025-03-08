module Types exposing (..)

import Element as El exposing (Device)
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (WebData)
import Theme exposing (Theme, decodeTheme, defaultTheme)
import Translation exposing (Translation)


type alias Model =
    { flags : Flags
    , hash : String
    , translations : WebData (List Translation)
    , items : WebData ItemsResult
    , itemFilter : ItemFilter
    , product : WebData Product
    , event : WebData Event
    , eventConfig : EventConfig
    , errorMsg : Maybe String
    }


type alias Item =
    { id : Int
    , name : String
    , summary : Maybe String
    , occursOn : Maybe String
    , timeZoneShort : Maybe String
    , location : Maybe String
    , venue : Maybe String
    , noRegistrationMessage : Maybe String
    , price : Maybe String
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , publishResults : Bool
    }


type ItemsSection
    = LeaguesSection
    | CompetitionsSection
    | ProductsSection


type alias Flags =
    { host : Maybe String
    , hash : Maybe String
    , lang : String
    , apiKey : Maybe String
    , subdomain : Maybe String
    , fullScreenToggle : Bool
    , fullScreen : Bool
    , searchable : Bool
    , section : ItemsSection
    , registration : Bool
    , showWaiversForTeams : Bool
    , excludeEventSections : List String
    , defaultEventSection : Maybe String
    , eventId : Maybe Int
    , theme : Theme
    , loggedInCurlerIds : List Int
    , device : Device
    }


type alias Season =
    { display : String
    , delta : Int
    }


type alias ItemsResult =
    { seasons : List Season
    , items : List Item
    }


type alias ItemFilter =
    { page : Int
    , seasonDelta : Int
    , search : String
    , seasonSearchOpen : Bool
    }


type alias Sponsor =
    { logoUrl : String
    , name : Maybe String
    , url : Maybe String
    }


type alias Product =
    { id : Int
    , name : String
    , summary : Maybe String
    , description : Maybe String
    , sponsor : Maybe Sponsor
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , total : Maybe String
    , potentialDiscounts : List String
    }


type RockDelivery
    = RockDeliveryRight
    | RockDeliveryLeft


type alias TeamCurler =
    { curlerId : Int
    , position : Maybe Int
    , skip : Bool
    , name : String
    , delivery : Maybe RockDelivery
    , photoUrl : Maybe String
    , waiver : Bool
    }


type alias Team =
    { id : Int
    , name : String
    , shortName : String
    , coach : Maybe String
    , affiliation : Maybe String
    , location : Maybe String
    , contactName : Maybe String
    , email : Maybe String
    , phone : Maybe String
    , imageUrl : Maybe String
    , lineup : List TeamCurler
    }


type alias Lineup =
    { first : Maybe String
    , second : Maybe String
    , third : Maybe String
    , fourth : Maybe String
    , alternate : Maybe String
    }


type alias Registration =
    { curlerName : Maybe String
    , teamName : Maybe String
    , skipName : Maybe String
    , position : Maybe String
    , lineup : Maybe Lineup
    }


type EventType
    = League
    | Competition


type EventState
    = EventStatePending
    | EventStateActive
    | EventStateComplete


type ScoringHilight
    = HilightHammers
    | HilightStolenEnds
    | HilightStolenPoints
    | HilightBlankEnds
    | Hilight1PointEnds
    | Hilight2PointEnds
    | Hilight3PointEnds
    | Hilight4PointEnds
    | Hilight5PlusPointEnds


type alias EventConfig =
    { scoringHilight : Maybe ScoringHilight
    , drawSelected : Maybe Int
    , drawSelectionOpen : Bool
    , teamSelected : Maybe Int
    , teamSelectionOpen : Bool
    }


type alias Spare =
    { name : String
    , positions : List String
    }


type StageType
    = RoundRobin
    | Bracket


type alias Group =
    { id : Int
    , name : String
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


type SideResult
    = SideResultWon
    | SideResultLost
    | SideResultTied
    | SideResultUnnecessary
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
    , timeRemaining : Maybe String
    , lsd : Maybe Float
    }


type alias Game =
    { id : String
    , name : String
    , state : GameState
    , videoUrl : Maybe String
    , coords : Maybe GameCoords
    , sides : List Side
    }


type alias Standing =
    { teamId : Int
    , rank : Int
    , played : Int
    , wins : Int
    , losses : Int
    , ties : Int
    , points : Float
    , lsd : Maybe Float
    , lsdRank : Maybe Int
    }


type alias Stage =
    { id : Int
    , stageType : StageType
    , name : String
    , groups : Maybe (List Group)
    , games : List Game
    , standings : List Standing
    }


type alias Draw =
    { id : Int
    , epoch : Int
    , startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe String)
    }


type alias Event =
    { id : Int
    , eventType : EventType
    , name : String
    , summary : Maybe String
    , description : Maybe String
    , note : Maybe String
    , teamRestriction : String
    , mixedDoubles : Bool
    , ageRange : String
    , sponsor : Maybe Sponsor
    , startsOn : String
    , endsOn : String
    , state : EventState
    , timeZone : Maybe String
    , timeZoneShort : Maybe String
    , location : Maybe String
    , venue : Maybe String
    , videoUrl : Maybe String
    , noRegistrationMessage : Maybe String
    , registrationOpensAt : Maybe String
    , registrationClosesAt : Maybe String
    , spotsAvailable : Maybe Int
    , spotsRemaining : Maybe Int
    , addToCartUrl : Maybe String
    , addToCartText : Maybe String
    , total : Maybe String
    , potentialDiscounts : List String
    , endScoresEnabled : Bool
    , shotByShotEnabled : Bool
    , lastStoneDrawEnabled : Bool
    , numberOfEnds : Int
    , topRock : String
    , botRock : String
    , sheetNames : List String
    , teams : List Team
    , registrations : List Registration
    , spares : List Spare
    , stages : List Stage
    , draws : List Draw
    , currentDrawId : Maybe Int
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
        |> optional "host" (nullable string) Nothing
        |> required "hash" (nullable string)
        |> optional "lang" string "en"
        |> optional "apiKey" (nullable string) Nothing
        |> optional "subdomain" (nullable string) Nothing
        |> optional "fullScreenToggle" bool False
        |> hardcoded False
        |> optional "searchable" bool True
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "showWaiversForTeams" bool False
        |> optional "excludeEventSections" (list string) []
        |> optional "defaultEventSection" (nullable string) Nothing
        |> optional "eventId" (nullable int) Nothing
        |> optional "theme" decodeTheme defaultTheme
        |> optional "loggedInCurlerIds" (list int) []
        |> hardcoded (El.classifyDevice { width = 800, height = 600 })


decodeItemsResult : Decoder ItemsResult
decodeItemsResult =
    let
        decodeSeason : Decoder Season
        decodeSeason =
            Decode.succeed Season
                |> required "display" string
                |> required "delta" int

        decodeItem : Decoder Item
        decodeItem =
            Decode.succeed Item
                |> required "id" int
                |> required "name" string
                |> optional "summary" (nullable string) Nothing
                |> optional "occurs_on" (nullable string) Nothing
                |> optional "time_zone_short" (nullable string) Nothing
                |> optional "location" (nullable string) Nothing
                |> optional "venue" (nullable string) Nothing
                |> optional "no_registration_message" (nullable string) Nothing
                |> optional "price" (nullable string) Nothing
                |> optional "add_to_cart_url" (nullable string) Nothing
                |> optional "add_to_cart_text" (nullable string) Nothing
                |> optional "publish_results" bool False
    in
    Decode.succeed ItemsResult
        |> optional "seasons" (list decodeSeason) []
        |> required "items" (list decodeItem)


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
        |> optional "total" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []


decodeEvent : Decoder Event
decodeEvent =
    let
        decodeEventType : Decoder EventType
        decodeEventType =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "competition" ->
                                Decode.succeed Competition

                            _ ->
                                Decode.succeed League
                    )

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
        |> required "event_type" decodeEventType
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing
        |> optional "note" (nullable string) Nothing
        |> required "team_restriction" string
        |> optional "mixed_doubles" bool False
        |> required "age_range" string
        |> optional "sponsor" (nullable decodeSponsor) Nothing
        |> required "starts_on" string
        |> required "ends_on" string
        |> optional "state" decodeEventState EventStateComplete
        |> optional "time_zone" (nullable string) Nothing
        |> optional "time_zone_short" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> optional "venue" (nullable string) Nothing
        |> optional "video_url" (nullable string) Nothing
        |> optional "no_registration_message" (nullable string) Nothing
        |> optional "registration_opens_at" (nullable string) Nothing
        |> optional "registration_closes_at" (nullable string) Nothing
        |> optional "spots_available" (nullable int) Nothing
        |> optional "spots_remaining" (nullable int) Nothing
        |> optional "add_to_cart_url" (nullable string) Nothing
        |> optional "add_to_cart_text" (nullable string) Nothing
        |> optional "total" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []
        |> optional "end_scores_enabled" bool False
        |> optional "shot_by_shot_enabled" bool False
        |> optional "last_stone_draw_enabled" bool False
        |> optional "number_of_ends" int 10
        |> optional "top_rock" string "red"
        |> optional "bot_rock" string "yellow"
        |> optional "sheet_names" (list string) []
        |> optional "teams" (list decodeTeam) []
        |> optional "registrations" (list decodeRegistration) []
        |> optional "spares" (list decodeSpare) []
        |> optional "stages" (list decodeStage) []
        |> optional "draws" (list decodeDraw) []
        |> optional "current_draw_id" (nullable int) Nothing


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
        |> optional "email" (nullable string) Nothing
        |> optional "phone" (nullable string) Nothing
        |> optional "image_url" (nullable string) Nothing
        |> optional "lineup" (list decodeTeamCurler) []


decodeTeamCurler : Decoder TeamCurler
decodeTeamCurler =
    let
        decodeTeamPosition : Decoder Int
        decodeTeamPosition =
            -- This can be made obsolute, if we pass position integers instead of strings.
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "fourth" ->
                                Decode.succeed 4

                            "third" ->
                                Decode.succeed 3

                            "second" ->
                                Decode.succeed 2

                            "first" ->
                                Decode.succeed 1

                            "alternate1" ->
                                Decode.succeed 5

                            "alternate2" ->
                                Decode.succeed 6

                            "alternate3" ->
                                Decode.succeed 7

                            "alternate4" ->
                                Decode.succeed 8

                            "alternate5" ->
                                Decode.succeed 9

                            "alternate6" ->
                                Decode.succeed 10

                            _ ->
                                -- Sketch
                                Decode.succeed 5
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
                |> optional "lsd" (nullable float) Nothing
                |> optional "lsd_rank" (nullable int) Nothing
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
        |> required "epoch" int
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

                                    "unnecessary" ->
                                        Decode.succeed (Just SideResultUnnecessary)

                                    "conceded" ->
                                        Decode.succeed (Just SideResultForfeited)

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
                |> optional "time_remaining" (nullable string) Nothing
                |> optional "lsd" (nullable float) Nothing
    in
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "state" decodeGameState
        |> optional "video_url" (nullable string) Nothing
        |> optional "coords" (nullable decodeGameCoords) Nothing
        |> required "game_positions" (list decodeSide)
