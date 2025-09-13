module Results.Types exposing (..)

import Browser.Dom
import Element exposing (Device)
import RemoteData exposing (WebData)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation)
import Time


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
    | SideResultForfeited
    | SideResultTimePenalized


type GameResult
    = Winner
    | Loser


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
    , winnerToGameId : Maybe String
    , winnerToSide : Maybe Int
    , loserToGameId : Maybe String
    , loserToSide : Maybe Int
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


type Msg
    = InitDevice Browser.Dom.Viewport
    | Tick Time.Posix
    | SetDevice Int Int
    | NavigateTo String
    | ToggleFullScreen
    | HashChanged Bool String
    | Reload
    | GotTranslations (WebData (List Translation))
    | GotItems (WebData ItemsResult)
    | IncrementPageBy Int
    | ToggleSeasonSearch
    | UpdateSearch String
    | UpdateSeasonDelta Int
    | NavigateOut String
    | GotEvent (WebData Event)
    | ReloadedEvent (WebData Event)
    | GotProduct (WebData Product)
    | ToggleScoringHilight ScoringHilight
    | ToggleDrawSelection
    | UpdateDrawSelected Int
    | ToggleTeamSelection
    | UpdateTeamSelected Int


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
