module Results.View exposing (view)

import Array
import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, style)
import Html.Events
import List.Extra
import Markdown
import RemoteData exposing (RemoteData(..))
import Results.CustomSvg exposing (..)
import Results.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme, defaultTheme)
import Shared.Translation exposing (Translation, decodeTranslations, translate)



-- VIEW MODELS AND HELPERS


type DrawState
    = DrawPending
    | DrawActive
    | DrawComplete


type alias EndStat =
    { teamId : Int
    , hammer : Bool
    , firstHammerFor : Bool
    , firstHammerAgainst : Bool
    , scoreFor : Int
    , scoreAgainst : Int
    , blankFor : Bool
    , blankAgainst : Bool
    , stolenFor : Bool
    , stolenAgainst : Bool
    , onePointFor : Bool
    , onePointAgainst : Bool
    , multiPointFor : Bool
    , multiPointAgainst : Bool
    }


type alias ShotExpanded =
    { gameId : String
    , drawId : Int
    , drawEpoch : Int
    , drawLabel : String
    , stageId : Int
    , sideNumber : Int
    , teamId : Int
    , teamShortName : String
    , curlerId : Int
    , curlerName : String
    , endNumber : Int
    , lineupPosition : Int
    , position : Int
    , turn : Maybe String
    , throw : Maybe String
    , rating : Maybe String
    }


type alias ShotSummaryByPosition =
    { position : Int
    , lineupPosition : Int
    , gameId : String
    , drawId : Int
    , drawEpoch : Int
    , sideNumber : Int
    , teamId : Int
    , teamName : String
    , curlerId : Int
    , curlerName : String
    , numberOfShots : Int
    , totalRatings : Int
    , percentage : Float
    , overUnder : Float
    , plusMinus : Int
    }


type alias PositionalPercentage =
    { position : Int
    , drawEpoch : Int
    , curlerId : Int
    , curlerName : String
    , teamId : Int
    , teamName : String
    , alternate : Bool
    , percentage : Float
    , oppositePercentage : Float
    }


type alias TeamShot =
    { curlerId : Int
    , curlerName : String
    , throw : String
    , turn : String
    , rating : String
    }


type alias Throws =
    { throw : String
    , name : String
    , inTurn : String
    , inTurnPercentage : String
    , outTurn : String
    , outTurnPercentage : String
    , total : String
    , totalPercentage : String
    }


type alias LineConnector =
    { fromCoords : ( Int, Int )
    , toCoords : ( Int, Int )
    }


expandShotsForGame : Event -> Game -> List ShotExpanded
expandShotsForGame { mixedDoubles, teams, draws, stages } game =
    let
        shotNumberToPosition : Int -> Int
        shotNumberToPosition shotNumber =
            if mixedDoubles then
                case shotNumber of
                    1 ->
                        1

                    5 ->
                        -- In mixed doubles, the lead throws 1 and 5.
                        1

                    _ ->
                        2

            else
                case shotNumber of
                    1 ->
                        1

                    2 ->
                        1

                    3 ->
                        2

                    4 ->
                        2

                    5 ->
                        3

                    6 ->
                        3

                    _ ->
                        4

        stageWithGame =
            let
                hasGame stage =
                    List.any (\g -> g.id == game.id) stage.games
            in
            List.Extra.find hasGame stages

        drawWithGame =
            drawWithGameId draws game.id
    in
    case ( stageWithGame, drawWithGame ) of
        ( Just stage, Just draw ) ->
            game.sides
                |> List.indexedMap
                    (\sideNumber side ->
                        case teamForSide teams side of
                            Just team ->
                                side.shots
                                    |> List.map
                                        (\shot ->
                                            case shot.curlerId of
                                                Just curlerId ->
                                                    case List.Extra.find (\c -> c.curlerId == curlerId) team.lineup of
                                                        Just curler ->
                                                            Just
                                                                { gameId = game.id
                                                                , drawId = draw.id
                                                                , drawEpoch = draw.epoch
                                                                , drawLabel = draw.label
                                                                , stageId = stage.id
                                                                , sideNumber = sideNumber
                                                                , teamId = team.id
                                                                , teamShortName = team.shortName
                                                                , curlerId = curlerId
                                                                , curlerName = curler.name
                                                                , endNumber = shot.endNumber
                                                                , lineupPosition = List.Extra.find (\l -> l.curlerId == curlerId) team.lineup |> Maybe.map .position |> Maybe.withDefault (Just 5) |> Maybe.withDefault 5
                                                                , position = shotNumberToPosition shot.shotNumber
                                                                , turn = shot.turn
                                                                , throw = shot.throw
                                                                , rating = shot.rating
                                                                }

                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    Nothing
                                        )
                                    |> List.filterMap identity

                            Nothing ->
                                []
                    )
                -- Lift the shots up to the sides
                |> List.concat

        _ ->
            []


summarizeShotsByPositionForGame : Event -> Game -> List ShotSummaryByPosition
summarizeShotsByPositionForGame event game =
    let
        toSummary : ( ShotExpanded, List ShotExpanded ) -> ShotSummaryByPosition
        toSummary ( shotsHead, shotsTail ) =
            let
                positionForCurler : Int
                positionForCurler =
                    -- The position is determine by where they throw the most rocks, or if the same, the
                    (shotsHead :: shotsTail)
                        |> List.map .position
                        |> List.Extra.group
                        |> List.map (\tu -> ( Tuple.first tu, List.length (Tuple.second tu) ))
                        |> List.sortBy (\tu -> Tuple.second tu)
                        |> List.reverse
                        |> List.map (\tu -> Tuple.first tu)
                        |> List.head
                        |> Maybe.withDefault 0

                totalRatings : Int
                totalRatings =
                    List.map .rating (shotsHead :: shotsTail)
                        |> List.filterMap identity
                        |> List.map String.toInt
                        |> List.filterMap identity
                        |> List.sum

                numberOfShots =
                    List.length shotsTail + 1
            in
            { position = positionForCurler
            , lineupPosition = shotsHead.lineupPosition
            , gameId = shotsHead.gameId
            , drawId = shotsHead.drawId
            , drawEpoch = shotsHead.drawEpoch
            , sideNumber = shotsHead.sideNumber
            , teamId = shotsHead.teamId
            , teamName = shotsHead.teamShortName
            , curlerId = shotsHead.curlerId
            , curlerName = shotsHead.curlerName
            , numberOfShots = numberOfShots
            , totalRatings = totalRatings
            , percentage = toFloat totalRatings / toFloat numberOfShots * 100 / 4
            , overUnder = 0
            , plusMinus = 0
            }

        addPlusMinuses : List ShotSummaryByPosition -> List ShotSummaryByPosition
        addPlusMinuses shotSummaries =
            let
                addPlusMinus : ShotSummaryByPosition -> ShotSummaryByPosition
                addPlusMinus shotSummary =
                    let
                        opponent =
                            shotSummaries
                                |> List.filter (\ss -> ss.position == shotSummary.position && ss.gameId == shotSummary.gameId && ss.teamId /= shotSummary.teamId)
                                |> List.head
                    in
                    -- TODO
                    case opponent of
                        Just opp ->
                            let
                                overUnder =
                                    shotSummary.percentage - opp.percentage

                                plusMinus =
                                    if overUnder >= 5 then
                                        1

                                    else if overUnder <= -5 then
                                        -1

                                    else
                                        0
                            in
                            { shotSummary | overUnder = overUnder, plusMinus = plusMinus }

                        Nothing ->
                            shotSummary
            in
            List.map addPlusMinus shotSummaries
    in
    expandShotsForGame event game
        -- Remove throw throughs (X in throw)
        |> List.filter (\s -> s.throw /= Nothing && s.throw /= Just "X")
        -- We need to sort by side number and curler since the groupWhile only examines adjacent items. (annoying!)
        |> List.sortBy .curlerId
        -- Then sort by side so we keep the two sides separate
        |> List.sortBy .sideNumber
        -- Group by side and curler
        |> List.Extra.groupWhile (\a b -> a.sideNumber == b.sideNumber && a.curlerId == b.curlerId)
        -- Build out a shot summary for each
        |> List.map toSummary
        -- Sort by position within each side.
        |> List.sortBy .position
        |> List.sortBy .sideNumber
        |> addPlusMinuses



-- VIEWS


viewButtonPrimary theme content msg =
    button
        [ Background.color theme.primary

        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
        , El.htmlAttribute (attribute "style" "color: white !important;")
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
        { device, fullScreenToggle, fullScreen } =
            model.flags

        viewMain =
            el
                [ El.htmlAttribute (class "cio__main")
                , El.width
                    (if fullScreen then
                        El.fill

                     else
                        El.fill
                            |> El.maximum
                                (case device.class of
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
                , El.scrollbarX
                , El.clipY
                , El.centerX
                ]
                (case model.errorMsg of
                    Just errorMsg ->
                        viewNotReady fullScreen errorMsg

                    Nothing ->
                        case model.translations of
                            Success translations ->
                                viewRoute translations model

                            Failure error ->
                                Lazy.lazy2 viewFetchError model.flags.theme (errorMessage error)

                            _ ->
                                viewNotReady fullScreen "Loading..."
                )

        theme =
            model.flags.theme
    in
    El.layout
        [ Font.size 16
        , Font.color theme.defaultText
        , El.width El.fill
        , El.padding 10
        , if fullScreen then
            El.htmlAttribute (style "z-index" "2001")

          else
            attrNone
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
        , if fullScreen then
            El.inFront
                (el
                    [ El.width El.fill
                    , El.height El.fill
                    , El.padding 10
                    , El.scrollbarY
                    , Background.color theme.white
                    ]
                    viewMain
                )

          else
            attrNone
        ]
    <|
        if fullScreen then
            El.none

        else
            viewMain


viewRoute : List Translation -> Model -> Element Msg
viewRoute translations { flags, hash, itemFilter, eventConfig, items, product, event } =
    let
        { device, fullScreen } =
            flags

        viewLoading =
            Lazy.lazy2 viewNotReady fullScreen "Loading..."
    in
    case toRoute flags.defaultEventSection hash of
        ItemsRoute ->
            case items of
                Success items_ ->
                    Lazy.lazy5 viewItems flags device translations itemFilter items_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading

        ProductRoute id ->
            case product of
                Success product_ ->
                    Lazy.lazy4 viewProduct flags translations fullScreen product_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading

        EventRoute id nestedRoute ->
            case event of
                Success event_ ->
                    Lazy.lazy5 viewEvent flags translations eventConfig nestedRoute event_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading


viewReloadStatus : Flags -> List Translation -> NestedEventRoute -> Event -> Element Msg
viewReloadStatus flags translations nestedRoute event =
    let
        { device, theme } =
            flags

        shouldReload =
            (event.state == EventStateActive)
                && event.endScoresEnabled
                && not (List.member nestedRoute [ DetailsRoute, RegistrationsRoute, SparesRoute, TeamsRoute ])
    in
    if shouldReload && device.class /= El.Phone then
        el
            [ El.paddingXY 10 15
            , El.alignTop
            , Font.size 12
            , Font.color theme.secondary
            , El.htmlAttribute (class "cio__reload_button")
            ]
            (text (translate translations "refreshes_in" ++ " 30s"))

    else
        El.none


viewFullScreenButton : Theme -> List Translation -> Bool -> Element Msg
viewFullScreenButton theme translations fullScreen =
    button
        [ El.paddingXY 5 4
        , Border.rounded 4
        , Font.size 12
        , Font.color theme.white
        , Background.color theme.secondary
        , El.focused [ Background.color theme.secondary ]
        ]
        { onPress = Just ToggleFullScreen
        , label =
            if fullScreen then
                -- El.html svgExitFullScreen
                text (translate translations "minimize" ++ " ↙")

            else
                -- El.html svgFullScreen
                text (translate translations "fullscreen" ++ " ↗")
        }


viewNotReady : Bool -> String -> Element Msg
viewNotReady fullScreen message =
    el [ El.htmlAttribute (class "cio__not_ready") ] (text message)


viewFetchError : Theme -> String -> Element Msg
viewFetchError theme message =
    row
        [ El.htmlAttribute (class "cio__fetch_error") ]
        [ column [ El.spacing 10 ]
            [ el [] (text message)
            , viewButtonPrimary theme "Reload" Reload
            ]
        ]


viewItems : Flags -> Device -> List Translation -> ItemFilter -> ItemsResult -> Element Msg
viewItems flags device translations itemFilter items =
    let
        { theme, fullScreen, section, registration } =
            flags

        viewPaging =
            let
                viewPageButton content msg =
                    button
                        [ Font.size 14
                        , El.padding 8
                        , Border.rounded 3

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important;")
                        , Background.color theme.primary
                        , El.focused [ Background.color theme.primary ]
                        ]
                        { onPress = Just msg
                        , label = text content
                        }
            in
            row [ El.htmlAttribute (class "cio__paging"), El.spacing 10 ]
                [ if itemFilter.page > 1 then
                    viewPageButton "< Previous" (IncrementPageBy -1)

                  else
                    El.none
                , if List.length filteredItems > (itemFilter.page * 10) then
                    viewPageButton "Next >" (IncrementPageBy 1)

                  else
                    El.none
                ]

        viewSeasonDropDown =
            let
                seasonOption { display, delta } =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateSeasonDelta delta)
                        , if delta == itemFilter.seasonDelta then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations display))

                seasonOptions =
                    if itemFilter.seasonSearchOpen then
                        let
                            scrolling =
                                if List.length items.seasons > 5 then
                                    [ El.height (El.fill |> El.minimum 210), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map seasonOption items.seasons)

                    else
                        El.none

                seasonSelected =
                    List.filter (\season -> season.delta == itemFilter.seasonDelta) items.seasons
                        |> List.map .display
                        |> List.head
                        |> Maybe.withDefault "-"
            in
            row
                [ El.width (El.px 150)
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
                    items.items

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
                    List.filter matches items.items

        pagedItems =
            filteredItems
                |> Array.fromList
                |> Array.slice ((itemFilter.page - 1) * 10) (itemFilter.page * 10)
                |> Array.toList
    in
    column [ El.spacing 10, El.width El.fill ]
        [ row [ El.spacing 20, El.htmlAttribute (class "cio__filter_container") ]
            [ if flags.searchable then
                El.html
                    (Html.input
                        [ Html.Attributes.placeholder (translate translations "search")
                        , style "box-sizing" "border-box"
                        , style "height" "38px"
                        , style "padding" "3px 10px"
                        , style "margin-right" "10px"
                        , style "border" "solid lightgray 1px"
                        , style "width" "170px"
                        , Html.Events.onInput UpdateSearch
                        ]
                        []
                    )

              else
                El.none
            , if List.length items.seasons > 1 then
                viewSeasonDropDown

              else
                El.none
            ]
        , if List.isEmpty filteredItems then
            el [ El.padding 10 ] (text "No results found.")

          else
            let
                viewItemName item =
                    let
                        newPath =
                            case section of
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

                viewItemLocation item =
                    column
                        [ El.spacingXY 0 5
                        , El.paddingXY 10 15
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_location")
                        ]
                        [ el [] (text (Maybe.withDefault " " item.location))
                        , el [ Font.size 13 ] (text (Maybe.withDefault " " item.venue))
                        ]

                viewItemPrice item =
                    if registration then
                        viewItemCell (el [ El.alignRight, El.htmlAttribute (class "cio__item_price") ] (text (Maybe.withDefault " " item.price)))

                    else
                        viewItemCell (text " ")

                viewItemRegister item =
                    if registration then
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
                                                , El.htmlAttribute (attribute "style" "color: white !important;")
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
                    { data = pagedItems
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
                          , view = viewItemLocation
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


viewProduct : Flags -> List Translation -> Bool -> Product -> Element Msg
viewProduct { theme } translations fullScreen product =
    row
        [ El.width El.fill
        , El.height El.fill
        , El.paddingXY 0 20
        , El.spacing 20
        , El.htmlAttribute (class "cio__product")
        ]
        [ column
            [ El.spacing 20
            , El.width El.fill
            , El.height El.fill
            , El.alignTop
            ]
            [ el [ Font.size 28 ] (text product.name)
            , case product.summary of
                Just summary ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_summary") ] [ text summary ]

                Nothing ->
                    El.none
            , case product.description of
                Just description ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_description") ] [ text description ]

                Nothing ->
                    El.none
            , case product.total of
                Just total ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__product_total") ]
                        [ el [ Font.bold ] (text (translate translations "total"))
                        , el [] (text total)
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


viewEvent : Flags -> List Translation -> EventConfig -> NestedEventRoute -> Event -> Element Msg
viewEvent flags translations eventConfig nestedRoute event =
    let
        { device, theme, fullScreenToggle, fullScreen } =
            flags

        eventSections : List String
        eventSections =
            let
                -- Check if a section is included (not in the explicitly excluded sections list).
                included section =
                    List.map String.toLower flags.excludeEventSections
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
                            List.any (\g -> g.state == GameComplete) (gamesInEvent event)

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
                        , El.htmlAttribute (attribute "style" "color: white !important;")
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
        [ El.row [ El.spacing 10 ]
            [ el
                [ Font.size
                    (case device.class of
                        El.Phone ->
                            22

                        _ ->
                            28
                    )
                , El.width El.fill
                , Font.medium
                , El.htmlAttribute (class "cio__event_name")
                ]
                (text event.name)
            , el [ El.alignTop ] (viewReloadStatus flags translations nestedRoute event)
            , if fullScreenToggle then
                el [] (viewFullScreenButton theme translations fullScreen)

              else
                El.none
            ]
        , El.row [ El.width El.fill, El.htmlAttribute (class "cio__event_nav") ]
            (List.map viewNavItem eventSections
                ++ (case event.videoUrl of
                        Just videoUrl ->
                            [ el [ El.padding 8 ] (text "")
                            , El.newTabLink
                                [ El.padding 8
                                , Border.rounded 4
                                , Font.color theme.white
                                , Background.color theme.secondary
                                ]
                                { url = videoUrl
                                , label = text (translate translations "video" ++ " ▶")
                                }
                            ]

                        Nothing ->
                            []
                   )
            )
        , case nestedRoute of
            DetailsRoute ->
                Lazy.lazy4 viewDetails theme device translations event

            RegistrationsRoute ->
                Lazy.lazy3 viewRegistrations theme translations event.registrations

            SparesRoute ->
                Lazy.lazy3 viewSpares flags translations event

            DrawsRoute ->
                Lazy.lazy4 viewDraws theme translations eventConfig event

            DrawRoute drawId ->
                case List.Extra.find (\d -> d.id == drawId) event.draws of
                    Just draw ->
                        Lazy.lazy5 viewDraw theme translations eventConfig event draw

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            GameRoute gameId ->
                let
                    drawWithGame =
                        drawWithGameId event.draws gameId
                in
                case ( drawWithGame, findGameById event gameId ) of
                    ( Just draw, Just game ) ->
                        let
                            sheetLabel =
                                sheetNameForGame event game
                        in
                        viewGame theme translations eventConfig event sheetLabel True draw game

                    _ ->
                        Lazy.lazy viewNoDataForRoute translations

            StagesRoute ->
                case List.head event.stages of
                    Just stage ->
                        Lazy.lazy5 viewStages theme device translations event stage

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            StageRoute id ->
                case List.Extra.find (\s -> s.id == id) event.stages of
                    Just stage ->
                        Lazy.lazy5 viewStages theme device translations event stage

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            TeamsRoute ->
                Lazy.lazy3 viewTeams theme translations event

            TeamRoute id ->
                case List.Extra.find (\t -> t.id == id) event.teams of
                    Just team ->
                        Lazy.lazy5 viewTeam theme translations flags event team

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            ReportsRoute ->
                Lazy.lazy3 viewReports theme translations event

            ReportRoute report ->
                Lazy.lazy5 viewReport theme translations eventConfig event report
        ]


viewDetails : Theme -> Device -> List Translation -> Event -> Element Msg
viewDetails theme device translations event =
    row [ El.spacing 20, El.htmlAttribute (class "cio__event_details") ]
        [ column [ El.spacing 20, El.width El.fill, El.alignTop ]
            [ case event.summary of
                Just summary ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_summary") ] [ text summary ]

                Nothing ->
                    El.none
            , case event.description of
                Just description ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_description") ] [ El.html (Markdown.toHtml [] description) ]

                Nothing ->
                    El.none
            , case event.total of
                Just total ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__event_total") ]
                        [ el [ Font.bold ] (text (translate translations "total"))
                        , el [] (text total)
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
            , case ( event.registrationOpensAt, event.registrationClosesAt ) of
                ( Just registrationOpensAt, Just registrationClosesAt ) ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_opens_at") ]
                            [ el [ Font.bold ] (text (translate translations "registration_opens_at"))
                            , el [] (text registrationOpensAt)
                            ]
                        , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_closes_at") ]
                            [ el [ Font.bold ] (text (translate translations "registration_closes_at"))
                            , el [] (text registrationClosesAt)
                            ]
                        ]

                _ ->
                    El.none
            , case event.timeZone of
                Just timeZone ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_time_zone") ]
                            [ el [ Font.bold ] (text (translate translations "time_zone"))
                            , el [] (text timeZone)
                            ]
                        ]

                Nothing ->
                    El.none
            , case ( event.location, event.venue ) of
                ( Just location, Just venue ) ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_location") ]
                            [ el [ Font.bold ] (text (translate translations "location"))
                            , el [] (text location)
                            , el [ Font.size 13 ] (text venue)
                            ]
                        ]

                _ ->
                    El.none
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


viewSpares : Flags -> List Translation -> Event -> Element Msg
viewSpares flags translations event =
    let
        theme =
            flags.theme

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
        [ if List.isEmpty event.spares then
            El.paragraph [] [ text (translate translations "no_spares") ]

          else
            El.indexedTable [ El.htmlAttribute (class "cio__event_spares_table") ]
                { data = event.spares
                , columns = [ nameColumn, positionsColumn ]
                }
        , button
            [ Font.color theme.primary, El.focused [ Background.color theme.white ] ]
            { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/events/" ++ String.fromInt event.id ++ "/spares"))
            , label = text (translate translations "members_login_to_see_contact_info")
            }
        ]


viewDraws : Theme -> List Translation -> EventConfig -> Event -> Element Msg
viewDraws theme translations eventConfig event =
    let
        currentDraw =
            currentDrawForEvent event

        drawLink : Draw -> String -> DrawState -> Element Msg
        drawLink draw label drawState_ =
            if event.endScoresEnabled then
                button
                    [ case drawState_ of
                        DrawPending ->
                            Font.color theme.secondary

                        _ ->
                            Font.color theme.primary
                    , El.focused [ Background.color theme.white ]
                    ]
                    { onPress = Just (NavigateTo (drawUrl event.id draw.id))
                    , label = text label
                    }

            else
                text label

        gameLink : Game -> DrawState -> Element Msg
        gameLink game drawState_ =
            let
                teamNameForSide side =
                    teamForSide event.teams side
                        |> Maybe.map .shortName

                teamNames =
                    List.map teamNameForSide game.sides
                        |> List.filterMap identity

                hasScores =
                    List.any (\s -> s.score /= Nothing) game.sides

                teamNameForSideWithScore side =
                    teamForSide event.teams side
                        |> Maybe.map
                            (\team ->
                                case side.score of
                                    Just score ->
                                        -- Bold the score
                                        row [ El.spacing 2 ] [ text team.shortName, el [ Font.bold ] (text (String.fromInt score)) ]

                                    Nothing ->
                                        text team.shortName
                            )

                teamNamesWithScores =
                    List.map teamNameForSideWithScore game.sides
                        |> List.filterMap identity

                gameNameWithScores =
                    -- Show the teams that are playing once a game is active.
                    case game.state of
                        GameComplete ->
                            -- Show the scores for the teams that played once a game is complete.
                            if hasScores then
                                row [ El.spacing 6 ] teamNamesWithScores

                            else
                                text (String.join " v " teamNames)

                        _ ->
                            if List.length teamNames == 2 then
                                text (String.join " v " teamNames)

                            else
                                text game.name
            in
            if event.endScoresEnabled then
                button
                    [ case drawState_ of
                        DrawPending ->
                            Font.color theme.secondary

                        _ ->
                            Font.color theme.primary
                    , El.focused [ Background.color theme.white ]
                    ]
                    { onPress = Just (NavigateTo (gameUrl event.id game.id))
                    , label = gameNameWithScores
                    }

            else
                el [] gameNameWithScores

        tableColumns onActive =
            let
                drawState : Draw -> DrawState
                drawState draw =
                    if drawHasCompletedGame event draw then
                        DrawComplete

                    else if (Maybe.map .id currentDraw == Just draw.id) || drawHasActiveGame event draw then
                        -- Highlight the active (closest) draw if there are no active games, or the current
                        -- draw is it has an active game.
                        DrawActive

                    else
                        DrawPending

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
                        [ el [ align ] (text (translate translations content))
                        ]

                tableCell align drawState_ content =
                    row
                        ([ El.paddingXY 12 16
                         , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                         , Border.color theme.grey
                         , El.htmlAttribute (class "cio__event_draws_cell")
                         ]
                            ++ (case drawState_ of
                                    DrawActive ->
                                        [ Background.color theme.greyLight
                                        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 1 }
                                        ]

                                    _ ->
                                        []
                               )
                        )
                        [ el [ align ] content ]

                labelColumn =
                    Just
                        { header = tableHeader El.alignLeft " "
                        , width = El.px 35
                        , view = \draw -> tableCell El.alignLeft (drawState draw) (drawLink draw draw.label (drawState draw))
                        }

                startsAtColumn =
                    Just
                        { header =
                            tableHeader El.alignLeft
                                (if onActive then
                                    translate translations "current_draw"

                                 else
                                    translate translations "all_draws"
                                )
                        , width = El.px 180
                        , view = \draw -> tableCell El.alignLeft (drawState draw) (drawLink draw draw.startsAt (drawState draw))
                        }

                attendanceColumn =
                    if hasAttendance then
                        Just
                            { header =
                                tableHeader El.alignLeft
                                    (if onActive then
                                        "Att"

                                     else
                                        "Att"
                                    )
                            , width = El.px 65
                            , view =
                                \draw ->
                                    tableCell El.alignLeft (drawState draw) (text (String.fromInt draw.attendance))
                            }

                    else
                        Nothing

                sheetColumn columnIndex sheetName =
                    Just
                        { header =
                            tableHeader El.centerX
                                (if sheetName == "" then
                                    " "

                                 else
                                    sheetName
                                )
                        , width = El.fill
                        , view =
                            \draw ->
                                tableCell El.centerX
                                    (drawState draw)
                                    (case List.Extra.getAt columnIndex draw.drawSheets of
                                        Just (Just gameId) ->
                                            case List.Extra.find (\g -> g.id == gameId) (gamesInEvent event) of
                                                Just game ->
                                                    gameLink game (drawState draw)

                                                Nothing ->
                                                    text "-"

                                        _ ->
                                            text "-"
                                    )
                        }

                sheetColumns =
                    List.indexedMap sheetColumn event.sheetNames
            in
            ([ labelColumn, startsAtColumn ] ++ sheetColumns ++ [ attendanceColumn ])
                |> List.filterMap identity
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_draws") ]
        (if List.isEmpty event.draws then
            El.paragraph [] [ text (translate translations "no_draws") ]

         else
            column [ El.width El.fill, El.spacing 10 ]
                [ if event.eventType == Competition then
                    case currentDraw of
                        Just draw ->
                            El.table [ El.htmlAttribute (class "cio__event_draws_table") ]
                                { data = [ draw ]
                                , columns = tableColumns True
                                }

                        Nothing ->
                            El.none

                  else
                    El.none
                , El.table [ El.paddingXY 0 10, El.htmlAttribute (class "cio__event_draws_table") ]
                    { data = event.draws
                    , columns = tableColumns False
                    }
                , case event.timeZone of
                    Just timeZone ->
                        el [ Font.italic, Font.color theme.greyDark, El.paddingXY 10 10 ] (text ("* " ++ timeZone))

                    Nothing ->
                        El.none
                , case event.note of
                    Just note ->
                        El.paragraph [ El.paddingXY 10 10 ]
                            [ el
                                [ Font.italic
                                , Font.color theme.greyDark
                                ]
                                (text note)
                            ]

                    Nothing ->
                        El.none
                ]
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

                hasLsdCumulative =
                    List.any (\standing -> standing.lsd /= Nothing) onStage.standings

                hasLsdRank =
                    List.any (\standing -> standing.lsdRank /= Nothing) onStage.standings

                tableHeader align content =
                    el
                        [ Font.bold
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        , El.paddingXY 12 16
                        ]
                        (el [ align ] (text (translate translations content)))

                tableCell i align content =
                    el
                        [ El.paddingXY 12 16
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] content)

                teamColumn =
                    Just
                        { header = tableHeader El.alignLeft " "
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
                                            El.alignLeft
                                            (button
                                                [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateTo (teamUrl event.id standing.teamId))
                                                , label = text teamName
                                                }
                                            )

                                    Nothing ->
                                        text " "
                        }

                lsdCumulativeColumn =
                    if hasLsdCumulative then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "LSD"

                                     else
                                        -- "lsd_cumulative"
                                        "LSD Rank Distance"
                                    )
                            , width = El.fill
                            , view =
                                \i standing ->
                                    tableCell i
                                        El.alignRight
                                        (text
                                            (case standing.lsd of
                                                Just lsd ->
                                                    String.fromFloat lsd

                                                Nothing ->
                                                    "-"
                                            )
                                        )
                            }

                    else
                        Nothing

                lsdRankColumn =
                    if hasLsdRank then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "LSD #"

                                     else
                                        -- "lsd_standings"
                                        "LSD Standings"
                                    )
                            , width = El.fill
                            , view =
                                \i standing ->
                                    tableCell i
                                        El.alignRight
                                        (text
                                            (case standing.lsdRank of
                                                Just lsdRank ->
                                                    String.fromInt lsdRank

                                                Nothing ->
                                                    "-"
                                            )
                                        )
                            }

                    else
                        Nothing

                gamesColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "G"

                                 else
                                    "games"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.played))
                        }

                winsColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "W"

                                 else
                                    "wins"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.wins))
                        }

                lossesColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "L"

                                 else
                                    "losses"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.losses))
                        }

                tiesColumn =
                    if hasTies then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "T"

                                     else
                                        "ties"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.ties))
                            }

                    else
                        Nothing

                pointsColumn =
                    if hasPoints then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "P"

                                     else
                                        "points"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i El.alignRight (text (String.fromFloat standing.points))
                            }

                    else
                        Nothing

                tableColumns =
                    List.filterMap identity [ teamColumn, gamesColumn, winsColumn, lossesColumn, tiesColumn, pointsColumn, lsdCumulativeColumn, lsdRankColumn ]
            in
            El.indexedTable [ El.htmlAttribute (class "cio__event_round_robin_table") ]
                { data = onStage.standings
                , columns = tableColumns
                }

        viewBracket =
            let
                gridSize : Int
                gridSize =
                    50

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
                                                ([ El.width El.fill
                                                 , El.height (El.px 25)
                                                 , El.clip
                                                 , El.paddingEach { left = 3, right = 0, top = 6, bottom = 0 }
                                                 , if position == 0 then
                                                    Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }

                                                   else
                                                    Border.width 0
                                                 , Border.color theme.grey
                                                 ]
                                                    ++ (if side.result == Just SideResultWon then
                                                            [ Font.bold, Font.color theme.primary ]

                                                        else
                                                            [ Font.regular ]
                                                       )
                                                )
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
                                                Just (NavigateTo (gameUrl event.id game.id))

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


viewDraw : Theme -> List Translation -> EventConfig -> Event -> Draw -> Element Msg
viewDraw theme translations eventConfig event draw =
    let
        viewDrawSheet gameId =
            let
                maybeGame =
                    gamesInEvent event
                        |> List.Extra.find (\g -> Just g.id == gameId)

                sheetLabel game =
                    sheetNameForGame event game
            in
            case maybeGame of
                Just game ->
                    viewGame theme translations eventConfig event (sheetLabel game) False draw game

                Nothing ->
                    El.none
    in
    column [ El.width El.fill, El.spacing 20 ]
        [ el
            [ El.width El.fill, El.padding 16, Font.color theme.greyDark, Background.color theme.greyLight ]
            (text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt))
        , column [ El.width El.fill, El.spacing 30 ] (List.map viewDrawSheet draw.drawSheets)
        , case event.timeZone of
            Just timeZone ->
                el [ Font.italic, Font.color theme.greyDark, El.paddingXY 0 10 ] (text ("* " ++ timeZone))

            Nothing ->
                El.none
        ]


viewGame : Theme -> List Translation -> EventConfig -> Event -> String -> Bool -> Draw -> Game -> Element Msg
viewGame theme translations eventConfig event sheetLabel detailed draw game =
    let
        { scoringHilight } =
            eventConfig

        maxNumberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        teamName side =
            teamForSide event.teams side
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
                            if not detailed && event.endScoresEnabled then
                                text (translate translations "final_game_statistics")

                            else if List.any (\s -> s.result == Just SideResultTied) game.sides then
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
            if List.any (\s -> s.result == Just SideResultUnnecessary) game.sides then
                -- Unnecessary games are never linked.
                el [ Font.italic, Font.color theme.greyDark, El.padding 8 ] (text (translate translations "unnecessary"))

            else if detailed then
                el [ Font.italic, Font.color theme.greyDark, El.padding 8 ] label

            else
                button
                    [ Font.color theme.primary
                    , Font.italic
                    , El.padding 8
                    , El.focused [ Background.color theme.transparent ]
                    ]
                    { onPress = Just (NavigateTo gamePath), label = label }

        viewGameHilight =
            case eventConfig.scoringHilight of
                Just hilight ->
                    button
                        [ El.alignRight
                        , El.padding 8

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important;")
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

                                        HilightStolenPoints ->
                                            "stolen_points"

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
            gameUrl event.id game.id

        drawPath =
            drawUrl event.id draw.id

        viewLsd : Side -> Element Msg
        viewLsd side =
            el
                [ El.paddingXY 10 15
                , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                , Border.color theme.grey
                ]
                (el [ El.centerX ]
                    (text
                        (case side.lsd of
                            Just lsd ->
                                String.fromFloat lsd

                            Nothing ->
                                " "
                        )
                    )
                )

        viewEndScore : Int -> Side -> Element Msg
        viewEndScore endNumber sideFor =
            let
                sideAgainstMaybe =
                    List.Extra.find (\s -> s.firstHammer /= sideFor.firstHammer) game.sides

                hasHammer =
                    case sideAgainstMaybe of
                        Just sideAgainst ->
                            hasHammerInEnd event.mixedDoubles sideFor sideAgainst (endNumber - 1)

                        Nothing ->
                            False

                endScoreStr =
                    case List.Extra.getAt (endNumber - 1) sideFor.endScores of
                        Just es ->
                            String.fromInt es

                        Nothing ->
                            if game.state == GameComplete then
                                "X"

                            else
                                " "

                endScoreInt =
                    List.Extra.getAt (endNumber - 1) sideFor.endScores
                        |> Maybe.withDefault 0

                stolenEnd =
                    (endScoreInt > 0) && not hasHammer

                blankEnd =
                    isBlankEnd (Just sideFor) sideAgainstMaybe (endNumber - 1)

                isHilighted =
                    (scoringHilight == Just HilightHammers && hasHammer)
                        || (scoringHilight == Just HilightStolenEnds && stolenEnd)
                        || (scoringHilight == Just HilightStolenPoints && stolenEnd)
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
                                (rockColorNameToRGB
                                    (if side.topRock then
                                        event.topRock

                                     else
                                        event.botRock
                                    )
                                )
                            ]
                            (row []
                                [ case teamForSide event.teams side of
                                    Just team ->
                                        button [] { onPress = Just (NavigateTo (teamUrl event.id team.id)), label = text team.name }

                                    Nothing ->
                                        text "TDB"
                                , text
                                    (if side.firstHammer then
                                        " *"

                                     else
                                        ""
                                    )
                                ]
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

        lsdColumn =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text "LSD"))
            , width = El.px 60
            , view = viewLsd
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
            let
                hasLsd =
                    event.lastStoneDrawEnabled
                        && List.any (\side -> side.lsd /= Nothing) game.sides
            in
            [ teamColumn ]
                ++ (if hasLsd then
                        [ lsdColumn ]

                    else
                        []
                   )
                ++ List.map endColumn (List.range 1 maxNumberOfEnds)
                ++ [ totalColumn ]
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
                    , case game.videoUrl of
                        Just videoUrl ->
                            El.newTabLink
                                [ El.alignRight
                                , El.padding 8
                                , Border.rounded 4
                                , Font.color theme.white
                                , Background.color theme.secondary
                                ]
                                { url = videoUrl
                                , label = text (translate translations "video" ++ " ▶")
                                }

                        Nothing ->
                            El.none
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
        , if detailed then
            column [ El.width El.fill, El.paddingXY 0 10, El.spacing 10 ]
                [ if event.shotByShotEnabled then
                    -- Shot Percentage
                    el [ El.width El.fill, El.paddingXY 0 20 ]
                        (viewScoringAndPercentagesForGame theme translations event game)

                  else
                    El.none
                , button [ El.alignRight, Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                    { onPress = Just (NavigateTo ("/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"))
                    , label = el [ Font.size 12 ] (text (translate translations "full_report" ++ " →"))
                    }
                , el [ El.width El.fill, El.spacing 10 ]
                    (List.map (teamForSide event.teams) game.sides
                        |> List.filterMap identity
                        |> Just
                        |> viewReportScoringAnalysis theme translations eventConfig event
                    )
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
                        , El.padding 15
                        , El.spacing 15
                        , El.width (El.px 250)
                        ]
                        [ if hasPhotoUrl then
                            el [ El.width El.fill ]
                                (case curler.photoUrl of
                                    Just photoUrl ->
                                        El.image [ El.height (El.px 150), El.centerX ]
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
                                            text (positionNumberToString translations curler.position)

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
                                        el [ Font.size 12 ] (text " ")

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
                    [ team.contactName, team.email, team.phone ]
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
                            , { label = "email", data = team.email }
                            , { label = "phone", data = team.phone }
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
                                |> List.map (findGameById event)
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
                        |> Maybe.andThen (teamForSide event.teams)

                drawAndSheetName draw game =
                    draw.label ++ " - " ++ sheetNameForGame event game

                viewTeamDrawLabel { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw.id))
                                , label = text (drawAndSheetName draw game)
                                }
                            )

                    else
                        tableCell (text (drawAndSheetName draw game))

                viewTeamDrawStartsAt { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw.id))
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
                                        { onPress = Just (NavigateTo (gameUrl event.id game.id))
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
                                                { onPress = Just (NavigateTo (gameUrl event.id game.id))
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
        hasCompetitionMatrix =
            List.any (\stage -> stage.stageType == RoundRobin) event.stages

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
        ([ if hasCompetitionMatrix then
            reportButton "competition_matrix"

           else
            El.none
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
                    [ reportButton "hog_line_violation"
                    , reportButton "scoring_and_percentages"
                    , reportButton "positional_percentage_comparison"
                    , reportButton "positional_plus_minus_comparison"
                    , reportButton "statistics_by_team"
                    , reportButton "cumulative_statistics_by_team"
                    ]

                else
                    []
               )
        )


viewReport : Theme -> List Translation -> EventConfig -> Event -> String -> Element Msg
viewReport theme translations eventConfig event report =
    case report of
        "competition_matrix" ->
            Lazy.lazy3 viewReportCompetitionMatrix theme translations event

        "team_rosters" ->
            Lazy.lazy3 viewReportTeamRosters theme translations event.teams

        "attendance" ->
            Lazy.lazy3 viewReportAttendance theme translations event.draws

        "scoring_analysis" ->
            column [ El.width El.fill, El.paddingXY 0 20, El.spacing 20, Font.size 24 ]
                [ text (translate translations "scoring_analysis")
                , Lazy.lazy5 viewReportScoringAnalysis theme translations eventConfig event Nothing
                ]

        "scoring_analysis_by_hammer" ->
            Lazy.lazy3 viewReportScoringAnalysisByHammer theme translations event

        "hog_line_violation" ->
            Lazy.lazy3 viewReportHogLineViolation theme translations event

        "scoring_and_percentages" ->
            Lazy.lazy5 viewReportScoringAndPercentagesForDraw theme translations eventConfig event eventConfig.drawSelected

        "positional_percentage_comparison" ->
            -- Tie in to routing to get the currently selected stage?
            Lazy.lazy3 viewReportPositionalPercentageComparison theme translations event

        "positional_plus_minus_comparison" ->
            -- Tie in to routing to get the currently selected stage?
            Lazy.lazy3 viewReportPositionalPlusMinusComparison theme translations event

        "statistics_by_team" ->
            Lazy.lazy5 viewReportStatisticsByTeam theme translations eventConfig event False

        "cumulative_statistics_by_team" ->
            Lazy.lazy5 viewReportStatisticsByTeam theme translations eventConfig event True

        _ ->
            Lazy.lazy viewNoDataForRoute translations


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
                                                        gameUrl event.id game.id
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
                                    [ el [] (text (positionNumberToString translations curler.position))
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


viewReportScoringAnalysis : Theme -> List Translation -> EventConfig -> Event -> Maybe (List Team) -> Element Msg
viewReportScoringAnalysis theme translations eventConfig event restrictToTeams =
    let
        teams =
            case restrictToTeams of
                Just teams_ ->
                    teams_

                Nothing ->
                    let
                        teamHasGames team =
                            not (List.isEmpty (gamesWithTeam event team))
                    in
                    List.filter teamHasGames event.teams

        rows =
            -- TODO: Structure the data so that for and against are just rows, but when rendering we know due to missing data or a flag which is which.
            List.Extra.interweave teams teams

        isHilighted onHilight =
            eventConfig.scoringHilight == Just onHilight

        isForGame =
            List.length teams == 2

        games team =
            let
                participatedIn sides =
                    List.any (\s -> s.teamId == Just team.id) sides
            in
            List.filter (\g -> g.state /= GamePending) (gamesInEvent event)
                |> List.filter (\g -> participatedIn g.sides)

        sidesFor team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId == Just team.id)

        sidesAgainst team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId /= Just team.id)

        statsFor team =
            List.map .endScores (sidesFor team)
                |> List.concat

        statsAgainst team =
            List.map .endScores (sidesAgainst team)
                |> List.concat

        firstHammerCountFor team =
            List.filter (\s -> s.firstHammer == True) (sidesFor team)
                |> List.length

        firstHammerCountAgainst team =
            List.filter (\s -> s.firstHammer == True) (sidesAgainst team)
                |> List.length

        blankEndsFor : Bool -> Team -> Int
        blankEndsFor for team =
            -- For is when you get the hammer after the blank end, which depends on whether it's a mixed doubles.
            -- Against is when you lose the hammer after the blank end, which depends on whether it's a mixed doubles.
            let
                -- A blank end is when neither teams gets a point.
                blankEndsForGame game =
                    let
                        -- We don't know if the team side is the first or second, so build a tuple so we know.
                        ( sideFor, sideAgainst ) =
                            ( List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                            , List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                            )

                        -- We don't care about the event's number of ends, just the max of either side.
                        numberOfEnds =
                            List.map (\side -> List.length side.endScores) game.sides
                                |> List.maximum
                                |> Maybe.withDefault 0

                        isBlankEndFor : Int -> Bool
                        isBlankEndFor endIndex =
                            case ( sideFor, sideAgainst ) of
                                ( Just sideFor_, Just sideAgainst_ ) ->
                                    let
                                        hasHammer =
                                            hasHammerInEnd event.mixedDoubles sideFor_ sideAgainst_ endIndex
                                    in
                                    -- This is ridiculous (but correct) and needs to be refactored.
                                    if for then
                                        isBlankEnd sideFor sideAgainst endIndex
                                            && ((not event.mixedDoubles && hasHammer)
                                                    || (event.mixedDoubles && not hasHammer)
                                               )

                                    else
                                        isBlankEnd sideFor sideAgainst endIndex
                                            && ((not event.mixedDoubles && not hasHammer)
                                                    || (event.mixedDoubles && hasHammer)
                                               )

                                _ ->
                                    False
                    in
                    List.range 0 numberOfEnds
                        |> List.filter isBlankEndFor
            in
            List.map (\game -> blankEndsForGame game) (games team)
                |> List.concat
                |> List.length

        onePointEndsFor team =
            List.filter (\i -> i == 1) (statsFor team)
                |> List.length

        onePointEndsAgainst team =
            List.filter (\i -> i == 1) (statsAgainst team)
                |> List.length

        twoPointEndsFor team =
            List.filter (\i -> i == 2) (statsFor team)
                |> List.length

        twoPointEndsAgainst team =
            List.filter (\i -> i == 2) (statsAgainst team)
                |> List.length

        threePointEndsFor team =
            List.filter (\i -> i == 3) (statsFor team)
                |> List.length

        threePointEndsAgainst team =
            List.filter (\i -> i == 3) (statsAgainst team)
                |> List.length

        fourPointEndsFor team =
            List.filter (\i -> i == 4) (statsFor team)
                |> List.length

        fourPointEndsAgainst team =
            List.filter (\i -> i == 4) (statsAgainst team)
                |> List.length

        fivePlusPointEndsFor team =
            List.filter (\i -> i > 4) (statsFor team)
                |> List.length

        fivePlusPointEndsAgainst team =
            List.filter (\i -> i > 4) (statsAgainst team)
                |> List.length

        totalPointsFor team =
            List.sum (statsFor team)

        totalPointsAgainst team =
            List.sum (statsAgainst team)

        averagePointsFor team =
            -- total points divided by number of ends. We multiple by 100 then round then divide by 100 so that we get 2 decimal places.
            if totalPointsFor team == 0 then
                0.0

            else
                toFloat (round ((toFloat (totalPointsFor team) / toFloat (List.length (statsFor team))) * 100)) / 100

        averagePointsAgainst team =
            -- see averagePointsFor
            toFloat (round ((toFloat (totalPointsAgainst team) / toFloat (List.length (statsAgainst team))) * 100)) / 100

        stolenEnds : Team -> Bool -> List Int
        stolenEnds team for =
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
                            if hasHammerInEnd event.mixedDoubles sideFor sideAgainst endIndex then
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
            List.map (\game -> stolenEndsForGame game) (games team)
                |> List.concat

        stolenEndsCount team for =
            stolenEnds team for
                |> List.length

        stolenPoints team for =
            stolenEnds team for
                |> List.sum

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
        [ El.indexedTable [ El.width El.fill, Font.size 13 ]
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
                                -- It doesn't matter if we use statsFor or statsAgainst, the count is the same since these the ends they participated in, just whether it's the top or bot.
                                statsFor team
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
                                tableCell El.centerX 1 (text (translate translations "for"))

                            else
                                tableCell El.centerX 1 (text (translate translations "against"))
                  }

                -- LSFE / First Hammer
                , { header =
                        tableHeader (translate translations "lsfe") El.centerX (Just (ToggleScoringHilight HilightHammers)) (Just "1")
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
                        tableHeader (translate translations "se") El.centerX (Just (ToggleScoringHilight HilightStolenEnds)) (Just "2")
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
                , { header = tableHeader (translate translations "be") El.centerX (Just (ToggleScoringHilight HilightBlankEnds)) (Just "3")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsFor True team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsFor False team)))
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
                , { header = tableHeader (translate translations "avg") El.centerX Nothing Nothing
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
                , { header = tableHeader (translate translations "sp") El.centerX (Just (ToggleScoringHilight HilightStolenPoints)) (Just "4")
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
            [ el [] (text ("1 - " ++ translate translations "lsfe" ++ ": " ++ translate translations "last_shot_first_end"))
            , el [] (text ("2 - " ++ translate translations "se" ++ ": " ++ translate translations "stolen_ends"))
            , el [] (text ("3 - " ++ translate translations "be" ++ ": " ++ translate translations "blank_ends"))
            , el [] (text ("4 - " ++ translate translations "sp" ++ ": " ++ translate translations "stolen_points"))
            ]
        ]


viewReportScoringAnalysisByHammer : Theme -> List Translation -> Event -> Element Msg
viewReportScoringAnalysisByHammer theme translations event =
    let
        viewByHammer withHammer =
            let
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
                        , El.clipX
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        ]
                        (el [ align ] content)

                viewTeamByHammer rowNumber team =
                    let
                        endStatsForGames : List (List EndStat)
                        endStatsForGames =
                            let
                                { mixedDoubles } =
                                    event

                                endStatsForGame : Game -> List EndStat
                                endStatsForGame game =
                                    let
                                        sideForMaybe =
                                            -- Get the side we're on
                                            List.Extra.find (\side -> side.teamId == Just team.id) game.sides

                                        sideAgainstMaybe =
                                            -- Get the side we aren't on
                                            List.Extra.find (\side -> side.teamId /= Just team.id) game.sides

                                        endStat : Side -> Side -> Int -> Int -> EndStat
                                        endStat sideFor sideAgainst endIndex scoreFor =
                                            let
                                                scoreAgainst =
                                                    List.Extra.getAt endIndex sideAgainst.endScores
                                                        |> Maybe.withDefault 0

                                                hammer =
                                                    hasHammerInEnd mixedDoubles sideFor sideAgainst endIndex

                                                blank =
                                                    scoreFor == 0 && scoreAgainst == 0

                                                blankFor =
                                                    -- You keep the hammer on a blank end, so you can only have a "for" when you already have the hammer.
                                                    -- Except in mixed doubles, where you lose the hammer on a blank end, so you can only have a "for" when you don't already have the hammer.
                                                    blank
                                                        && ((hammer && not mixedDoubles) || (not hammer && mixedDoubles))

                                                blankAgainst =
                                                    -- You keep the hammer on a blank end, so you can only have an "against" when you don't already have the hammer.
                                                    -- Except in mixed doubles, where you lose the hammer on a blank end, so you can only have a "against" when you already have the hammer.
                                                    blank
                                                        && ((not hammer && not mixedDoubles) || (hammer && mixedDoubles))
                                            in
                                            { teamId = team.id
                                            , hammer = hammer
                                            , firstHammerFor = sideFor.firstHammer
                                            , firstHammerAgainst = sideAgainst.firstHammer
                                            , scoreFor = scoreFor
                                            , scoreAgainst = scoreAgainst
                                            , blankFor = blankFor
                                            , blankAgainst = blankAgainst
                                            , stolenFor = not hammer && scoreFor > 0
                                            , stolenAgainst = hammer && scoreAgainst > 0
                                            , onePointFor = scoreFor == 1
                                            , onePointAgainst = scoreAgainst == 1
                                            , multiPointFor = scoreFor > 1
                                            , multiPointAgainst = scoreAgainst > 1
                                            }
                                    in
                                    -- Get the end scores for the side we're on
                                    case ( sideForMaybe, sideAgainstMaybe ) of
                                        ( Just sideFor, Just sideAgainst ) ->
                                            List.indexedMap (endStat sideFor sideAgainst) sideFor.endScores

                                        _ ->
                                            []
                            in
                            List.map endStatsForGame games

                        games : List Game
                        games =
                            gamesWithTeam event team
                                |> List.filter (\game -> game.state /= GamePending)

                        gamesCount : Int
                        gamesCount =
                            endStatsForGames
                                |> List.length

                        endStats : List EndStat
                        endStats =
                            endStatsForGames
                                |> List.concat

                        endsByHammer : List EndStat
                        endsByHammer =
                            endStats
                                |> List.filter (\es -> es.hammer == withHammer)

                        endsCountByHammer : Int
                        endsCountByHammer =
                            List.length endsByHammer

                        endsCountByHammerAndFilter : Bool -> (EndStat -> Bool) -> Int
                        endsCountByHammerAndFilter for statFilter =
                            endsByHammer
                                |> List.filter statFilter
                                |> List.length

                        blankEnds : Bool -> Int
                        blankEnds for =
                            let
                                statFilter : EndStat -> Bool
                                statFilter es =
                                    if for then
                                        es.blankFor

                                    else
                                        es.blankAgainst
                            in
                            endsCountByHammerAndFilter for statFilter

                        blankEndsPercent : Bool -> Int
                        blankEndsPercent for =
                            if blankEnds for == 0 then
                                0

                            else
                                round ((toFloat (blankEnds for) / toFloat endsCountByHammer) * 100)

                        stolenEnds : Bool -> Int
                        stolenEnds for =
                            let
                                statFilter es =
                                    if for then
                                        es.stolenFor

                                    else
                                        es.stolenAgainst
                            in
                            endsCountByHammerAndFilter for statFilter

                        stolenEndsPercent : Bool -> Int
                        stolenEndsPercent for =
                            if stolenEnds for == 0 then
                                0

                            else
                                round ((toFloat (stolenEnds for) / toFloat endsCountByHammer) * 100)

                        singlePoints : Bool -> Int
                        singlePoints for =
                            let
                                statFilter es =
                                    if for then
                                        es.onePointFor

                                    else
                                        es.onePointAgainst
                            in
                            endsCountByHammerAndFilter for statFilter

                        singlePointsPercent : Bool -> Int
                        singlePointsPercent for =
                            if singlePoints for == 0 then
                                0

                            else
                                round ((toFloat (singlePoints for) / toFloat endsCountByHammer) * 100)

                        multiPoints : Bool -> Int
                        multiPoints for =
                            let
                                statFilter es =
                                    if for then
                                        es.multiPointFor

                                    else
                                        es.multiPointAgainst
                            in
                            endsCountByHammerAndFilter for statFilter

                        multiPointsPercent : Bool -> Int
                        multiPointsPercent for =
                            if multiPoints for == 0 then
                                0

                            else
                                round ((toFloat (multiPoints for) / toFloat endsCountByHammer) * 100)
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
                        , viewCell { portion = 1, align = El.alignRight, content = text (String.fromInt endsCountByHammer) }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (blankEnds withHammer))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (blankEndsPercent withHammer))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (stolenEnds (not withHammer)))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (stolenEndsPercent (not withHammer)))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (singlePoints withHammer))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (singlePointsPercent withHammer))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (multiPoints withHammer))
                            }
                        , viewCell
                            { portion = 1
                            , align = El.alignRight
                            , content = text (String.fromInt (multiPointsPercent withHammer))
                            }
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
                    , viewHeader
                        { portion = 2
                        , align = El.alignRight
                        , content =
                            if withHammer then
                                "blanks_for"

                            else
                                "blanks_against"
                        }
                    , viewHeader
                        { portion = 2
                        , align = El.alignRight
                        , content =
                            if withHammer then
                                "steals_against"

                            else
                                "steals_for"
                        }
                    , viewHeader
                        { portion = 2
                        , align = El.alignRight
                        , content =
                            if withHammer then
                                "single_points_for"

                            else
                                "single_points_against"
                        }
                    , viewHeader
                        { portion = 2
                        , align = El.alignRight
                        , content =
                            if withHammer then
                                "multi_points_for"

                            else
                                "multi_points_against"
                        }
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
                    ++ List.indexedMap viewTeamByHammer event.teams
                )
    in
    column [ El.spacing 30, El.width El.fill, El.htmlAttribute (class "cio__event_reports_scoring_analysis_by_hammer") ]
        [ el [ Font.size 24 ] (text (translate translations "scoring_analysis_by_hammer"))
        , column [ El.spacing 80, El.width El.fill ]
            [ viewByHammer True
            , viewByHammer False
            ]
        ]


viewReportHogLineViolation : Theme -> List Translation -> Event -> Element Msg
viewReportHogLineViolation theme translations event =
    let
        hogLineViolations : List ShotExpanded
        hogLineViolations =
            let
                expandShotsForEvent : List ShotExpanded
                expandShotsForEvent =
                    let
                        expandShotsForStage : Stage -> List ShotExpanded
                        expandShotsForStage stage =
                            -- Kind of annoying, but we need to dig down to shots while picking up the draw label, and side's team id.
                            -- Ignore it if we're missing a draw (unscheduled), team (no point in reporting), curler (no point in reporting).
                            List.map (expandShotsForGame event) stage.games
                                -- Lift the games up to the root
                                |> List.concat
                    in
                    -- Kind of annoying, but we need to dig down to shots while picking up the draw label, and side's team id.
                    -- Ignore it if we're missing a draw (unscheduled), team (no point in reporting), curler (no point in reporting).
                    event.stages
                        |> List.map expandShotsForStage
                        -- Lift the stages up to the root
                        |> List.concat
            in
            expandShotsForEvent
                |> List.filter (\s -> s.rating == Just "V")

        viewHeader align label =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Font.semiBold
                , Background.color theme.greyLight
                ]
                (el [ align ] (text (translate translations label)))

        viewCell align content =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                ]
                (el [ align ] content)
    in
    -- We are looking for any shots with a rating value of "V" (for violation)
    -- Then we want to report the athlete name, the team name, the draw label, and the end number.
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "hog_line_violation"))
        , El.table []
            { data = hogLineViolations
            , columns =
                [ { header = viewHeader El.alignLeft "curler"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignLeft (text hogLineViolation.curlerName)
                  }
                , { header = viewHeader El.alignLeft "team"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignLeft
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (teamUrl event.id hogLineViolation.teamId))
                                    , label = text hogLineViolation.teamShortName
                                    }
                                )
                  }
                , { header = viewHeader El.alignRight "draw"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignRight
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (drawUrl event.id hogLineViolation.drawId))
                                    , label = text hogLineViolation.drawLabel
                                    }
                                )
                  }
                , { header = viewHeader El.alignRight "end_number"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignRight
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (gameUrl event.id hogLineViolation.gameId))
                                    , label = text (String.fromInt hogLineViolation.endNumber)
                                    }
                                )
                  }
                ]
            }
        ]


viewScoringAndPercentagesForGame : Theme -> List Translation -> Event -> Game -> Element Msg
viewScoringAndPercentagesForGame theme translations event game =
    let
        shotsGroupedByCurler : List (List ShotSummaryByPosition)
        shotsGroupedByCurler =
            let
                groupSummarizedShotsByTeam : List (List ShotSummaryByPosition)
                groupSummarizedShotsByTeam =
                    summarizeShotsByPositionForGame event game
                        -- |> List.map addPlusToSummary
                        |> List.Extra.groupWhile (\a b -> a.sideNumber == b.sideNumber)
                        |> List.map fromNonempty

                appendTotal : List ShotSummaryByPosition -> List ShotSummaryByPosition
                appendTotal group =
                    group
                        ++ [ { position = 5
                             , lineupPosition = 5
                             , gameId = ""
                             , drawId = 0
                             , drawEpoch = 0
                             , sideNumber = 0
                             , teamId = 0
                             , teamName = ""
                             , curlerId = 0
                             , curlerName = translate translations "total"
                             , numberOfShots = List.map .numberOfShots group |> List.sum
                             , totalRatings = List.map .totalRatings group |> List.sum
                             , percentage = 0
                             , overUnder = 0
                             , plusMinus = 0
                             }
                           ]
            in
            groupSummarizedShotsByTeam
                |> List.map appendTotal

        viewShotsByTeam shotsGrouped =
            let
                teamName =
                    case List.head shotsGrouped of
                        Just s ->
                            s.teamName

                        Nothing ->
                            ""

                tableHeader content =
                    el
                        [ Font.bold
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.greyDark
                        , El.paddingXY 0 12
                        ]
                        content

                tableCell content =
                    el
                        [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        , El.paddingXY 0 8
                        ]
                        content
            in
            column [ El.width El.fill, El.height El.fill, Font.size 14 ]
                [ El.table [ El.width El.fill, El.spacingXY 0 5, El.htmlAttribute (class "cio__items_table") ]
                    { data = shotsGrouped
                    , columns =
                        [ { header = tableHeader (text " ")
                          , width = El.px 20
                          , view =
                                \s ->
                                    tableCell
                                        (text
                                            (if s.position > 4 then
                                                " "

                                             else
                                                String.fromInt s.position
                                            )
                                        )
                          }
                        , { header = tableHeader (text teamName)
                          , width = El.fill
                          , view = \s -> tableCell (text s.curlerName)
                          }
                        , { header = tableHeader (el [ El.alignRight ] (text "#SH"))
                          , width = El.px 55
                          , view = \s -> tableCell (el [ El.alignRight ] (text (String.fromInt s.numberOfShots)))
                          }
                        , { header = tableHeader (el [ El.alignRight ] (text "PTS"))
                          , width = El.px 55
                          , view = \s -> tableCell (el [ El.alignRight ] (text (String.fromInt s.totalRatings)))
                          }
                        , { header = tableHeader (el [ El.alignRight ] (text "PCT"))
                          , width = El.px 55
                          , view = \s -> tableCell (el [ El.alignRight ] (text (String.fromInt (((toFloat s.totalRatings / toFloat (s.numberOfShots * 4)) * 100) |> round) ++ "%")))
                          }
                        ]
                    }
                ]
    in
    row [ El.width El.fill, El.spacing 30 ]
        (shotsGroupedByCurler
            |> List.map viewShotsByTeam
        )


viewReportScoringAndPercentagesForDraw : Theme -> List Translation -> EventConfig -> Event -> Maybe Int -> Element Msg
viewReportScoringAndPercentagesForDraw theme translations eventConfig event onDrawId =
    let
        onDraw =
            case onDrawId of
                Just id ->
                    List.Extra.find (\d -> d.id == id) event.draws

                Nothing ->
                    case event.state of
                        EventStateComplete ->
                            List.Extra.last event.draws

                        _ ->
                            List.head event.draws

        viewDraw_ draw =
            let
                viewDrawSheet drawSheet =
                    let
                        gameForDrawSheet =
                            case drawSheet of
                                Just id ->
                                    findGameById event id

                                Nothing ->
                                    Nothing
                    in
                    case gameForDrawSheet of
                        Just game ->
                            viewScoringAndPercentagesForGame theme translations event game

                        Nothing ->
                            El.none
            in
            column [ El.width El.fill, El.spacing 20 ]
                ([ el [ El.width El.fill, Font.size 20 ] (text (translate translations "draw" ++ " " ++ draw.label)) ]
                    ++ List.map viewDrawSheet draw.drawSheets
                )

        viewDrawSelector =
            let
                drawOption draw =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateDrawSelected draw.id)
                        , if Just draw.id == eventConfig.drawSelected then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations "draw" ++ " " ++ draw.label))

                drawOptions =
                    if eventConfig.drawSelectionOpen then
                        let
                            scrolling =
                                if List.length event.draws > 5 then
                                    [ El.height (El.fill |> El.minimum 210), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map drawOption event.draws)

                    else
                        El.none
            in
            row
                [ El.width (El.px 150)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleDrawSelection
                , El.below drawOptions
                , El.htmlAttribute (class "cio__draw_dropdown")
                ]
                [ el []
                    (text
                        (case onDraw of
                            Just draw ->
                                translate translations "draw" ++ " " ++ draw.label

                            Nothing ->
                                "-"
                        )
                    )
                , el [ El.alignRight ]
                    (text
                        (if eventConfig.drawSelectionOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]
    in
    column [ El.width El.fill, El.spacing 20 ]
        [ row [ El.width El.fill ]
            [ el [ Font.size 24 ] (text (translate translations "scoring_and_percentages"))
            , el [ El.alignRight ] viewDrawSelector
            ]
        , case onDraw of
            Just draw ->
                viewDraw_ draw

            Nothing ->
                el [ El.width El.fill ] (text "Coming Soon!")
        ]


viewReportPositionalPercentageComparison : Theme -> List Translation -> Event -> Element Msg
viewReportPositionalPercentageComparison theme translations event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        shotSummariesByPosition : Int -> List ShotSummaryByPosition
        shotSummariesByPosition position =
            List.map (gamesInDraw event) draws
                |> List.concat
                |> List.map (summarizeShotsByPositionForGame event)
                |> List.concat
                |> List.filter
                    (\ss ->
                        if position <= 4 then
                            ss.position == position && ss.lineupPosition <= 4

                        else
                            ss.lineupPosition == position
                    )

        viewPosition position =
            let
                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    shotSummariesByPosition position
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .percentage g) / toFloat (List.length g))
                        |> List.reverse

                data : List ShotSummaryByPosition
                data =
                    []

                viewHeader align label =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 2, left = 0 }
                        , Border.color theme.grey
                        , Font.semiBold
                        ]
                        (el [ align ] (text (translate translations label)))

                viewCell i align content =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] (text content))

                viewDrawCell draw =
                    { header = viewHeader El.centerX draw.label
                    , width = El.fill
                    , view =
                        \i shotSummary ->
                            viewCell i
                                El.centerX
                                (case List.Extra.find (\ss -> ss.drawId == draw.id) shotSummary of
                                    Just ss ->
                                        String.fromInt (round ss.percentage)

                                    _ ->
                                        " "
                                )

                    -- (String.fromInt shotSummary.totalRatings)
                    }
            in
            El.indexedTable []
                { data = groupSummarizedShotsByCurler
                , columns =
                    [ { header = viewHeader El.alignLeft (positionNumberToString translations (Just position))
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    (Maybe.map .curlerName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    , { header = viewHeader El.alignLeft (translate translations "team")
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    (Maybe.map .teamName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    ]
                        ++ List.map viewDrawCell draws
                        ++ [ { header = viewHeader El.centerX (translate translations "total")
                             , width = El.fill
                             , view =
                                \i shotSummaries ->
                                    viewCell i
                                        El.centerX
                                        (String.fromFloat
                                            (toFloat (round (List.sum (List.map .percentage shotSummaries) / toFloat (List.length shotSummaries))))
                                        )
                             }
                           ]
                }
    in
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "positional_percentage_comparison"))
        , column [ El.spacing 60 ] (List.map viewPosition [ 4, 3, 2, 1, 5 ])
        , el [] (text "")
        ]


viewReportPositionalPlusMinusComparison : Theme -> List Translation -> Event -> Element Msg
viewReportPositionalPlusMinusComparison theme translations event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        shotSummariesByPosition : Int -> List ShotSummaryByPosition
        shotSummariesByPosition position =
            List.map (gamesInDraw event) draws
                |> List.concat
                |> List.map (summarizeShotsByPositionForGame event)
                |> List.concat
                |> List.filter
                    (\ss ->
                        if position <= 4 then
                            ss.position == position && ss.lineupPosition <= 4

                        else
                            ss.lineupPosition == position
                    )

        viewPosition position =
            let
                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    shotSummariesByPosition position
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .overUnder g))
                        |> List.sortBy (\g -> List.sum (List.map .plusMinus g))
                        |> List.reverse

                data : List ShotSummaryByPosition
                data =
                    []

                viewHeader align label =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 2, left = 0 }
                        , Border.color theme.grey
                        , Font.semiBold
                        ]
                        (el [ align ] (text (translate translations label)))

                viewCell i align fontWeight fontColor content =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        , Maybe.withDefault Font.regular fontWeight
                        , Font.color (Maybe.withDefault (El.rgb 0 0 0) fontColor)
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] (text content))

                viewDrawCell draw =
                    { header = viewHeader El.centerX draw.label
                    , width = El.fill
                    , view =
                        \i shotSummary ->
                            case List.Extra.find (\ss -> ss.drawId == draw.id) shotSummary of
                                Just ss ->
                                    if ss.plusMinus > 0 then
                                        viewCell i El.centerX (Just Font.semiBold) Nothing "+"

                                    else if ss.plusMinus < 0 then
                                        viewCell i El.centerX (Just Font.bold) Nothing "-"

                                    else
                                        viewCell i El.centerX Nothing Nothing "0"

                                _ ->
                                    viewCell i El.centerX Nothing (Just (El.rgb 0.8 0.8 0.8)) "*"
                    }
            in
            El.indexedTable []
                { data = groupSummarizedShotsByCurler
                , columns =
                    [ { header = viewHeader El.alignLeft (positionNumberToString translations (Just position))
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    Nothing
                                    Nothing
                                    (Maybe.map .curlerName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    , { header = viewHeader El.alignLeft (translate translations "team")
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    Nothing
                                    Nothing
                                    (Maybe.map .teamName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    ]
                        ++ List.map viewDrawCell draws
                        ++ [ { header = viewHeader El.alignRight (translate translations "total")
                             , width = El.fill
                             , view =
                                \i shotSummaries ->
                                    let
                                        tally =
                                            let
                                                plusMinus ss =
                                                    if ss.overUnder >= 5 then
                                                        1

                                                    else if ss.overUnder <= -5 then
                                                        -1

                                                    else
                                                        0
                                            in
                                            List.map plusMinus shotSummaries |> List.sum
                                    in
                                    viewCell i
                                        El.alignRight
                                        Nothing
                                        Nothing
                                        ((if tally > 0 then
                                            "+"

                                          else
                                            ""
                                         )
                                            ++ String.fromInt tally
                                        )
                             }
                           ]
                }
    in
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "positional_plus_minus_comparison"))
        , column [ El.spacing 60 ] (List.map viewPosition [ 4, 3, 2, 1, 5 ])
        , el [] (text "")
        ]


viewReportStatisticsByTeam : Theme -> List Translation -> EventConfig -> Event -> Bool -> Element Msg
viewReportStatisticsByTeam theme translations eventConfig event cumulative =
    let
        draw =
            if cumulative then
                Nothing

            else
                case eventConfig.drawSelected of
                    Just drawId ->
                        List.Extra.find (\draw_ -> draw_.id == drawId) event.draws

                    Nothing ->
                        List.head event.draws

        viewDrawSelector =
            let
                drawOption draw_ =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateDrawSelected draw_.id)
                        , if Just draw_.id == eventConfig.drawSelected then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations "draw" ++ " " ++ draw_.label))

                drawOptions =
                    if eventConfig.drawSelectionOpen then
                        let
                            scrolling =
                                if List.length event.draws > 5 then
                                    [ El.height (El.fill |> El.minimum 210), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map drawOption event.draws)

                    else
                        El.none
            in
            row
                [ El.width (El.px 150)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleDrawSelection
                , El.below drawOptions
                , El.htmlAttribute (class "cio__draw_dropdown")
                ]
                [ el []
                    (text
                        (case draw of
                            Just draw_ ->
                                translate translations "draw" ++ " " ++ draw_.label

                            Nothing ->
                                "-"
                        )
                    )
                , el [ El.alignRight ]
                    (text
                        (if eventConfig.drawSelectionOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        selectableTeams =
            case draw of
                Just draw_ ->
                    draw_.drawSheets
                        |> List.filterMap identity
                        |> List.map
                            (\drawSheet ->
                                findGameById event drawSheet
                                    |> Maybe.map
                                        (\game ->
                                            game.sides
                                                |> List.map
                                                    (\side ->
                                                        side.teamId
                                                            |> Maybe.andThen (\teamId -> List.Extra.find (\t -> t.id == teamId) event.teams)
                                                    )
                                        )
                                    |> Maybe.withDefault []
                            )
                        |> List.concat
                        |> List.filterMap identity

                Nothing ->
                    event.teams

        team =
            case eventConfig.teamSelected of
                Just teamId ->
                    case List.Extra.find (\team_ -> team_.id == teamId) selectableTeams of
                        Just team_ ->
                            Just team_

                        Nothing ->
                            List.head selectableTeams

                Nothing ->
                    List.head selectableTeams

        viewTeamSelector =
            let
                teamOption team_ =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateTeamSelected team_.id)
                        , if Just team_.id == eventConfig.teamSelected then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text team_.name)

                teamOptions =
                    if eventConfig.teamSelectionOpen then
                        let
                            scrolling =
                                if List.length selectableTeams >= 8 then
                                    [ El.height (El.fill |> El.minimum 260), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map teamOption selectableTeams)

                    else
                        El.none
            in
            row
                [ El.width (El.px 260)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleTeamSelection
                , El.below teamOptions
                , El.htmlAttribute (class "cio__team_dropdown")
                ]
                [ el []
                    (text
                        (case team of
                            Just team_ ->
                                team_.name

                            Nothing ->
                                "-"
                        )
                    )
                , el [ El.alignRight ]
                    (text
                        (if eventConfig.teamSelectionOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        throws : Team -> Maybe TeamCurler -> List Throws
        throws team_ curler =
            let
                shots : List TeamShot
                shots =
                    let
                        teamShots : List TeamShot
                        teamShots =
                            let
                                teamShot : Shot -> Maybe TeamShot
                                teamShot { curlerId, throw, turn, rating } =
                                    case curlerId of
                                        Just curlerId_ ->
                                            case List.Extra.find (\c -> c.curlerId == curlerId_) team_.lineup of
                                                Just curler_ ->
                                                    case [ throw, turn, rating ] of
                                                        [ Just throw_, Just turn_, Just rating_ ] ->
                                                            Just
                                                                { curlerId = curlerId_
                                                                , curlerName = curler_.name
                                                                , throw = throw_
                                                                , turn = turn_
                                                                , rating = rating_
                                                                }

                                                        _ ->
                                                            Nothing

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                            in
                            List.map
                                (\stage ->
                                    List.map
                                        (\game ->
                                            if game.state /= GamePending then
                                                let
                                                    hasGame =
                                                        case draw of
                                                            Just draw_ ->
                                                                let
                                                                    matching gameId =
                                                                        gameId == Just game.id
                                                                in
                                                                List.any matching draw_.drawSheets

                                                            Nothing ->
                                                                False
                                                in
                                                if draw == Nothing || hasGame then
                                                    List.map
                                                        (\side ->
                                                            if side.teamId == Just team_.id then
                                                                List.map
                                                                    (\shot ->
                                                                        teamShot shot
                                                                    )
                                                                    side.shots

                                                            else
                                                                []
                                                        )
                                                        game.sides

                                                else
                                                    []

                                            else
                                                []
                                        )
                                        stage.games
                                )
                                event.stages
                                |> List.concat
                                |> List.concat
                                |> List.concat
                                |> List.filterMap identity
                    in
                    (case curler of
                        Just curler_ ->
                            teamShots
                                |> List.filter (\teamShot -> teamShot.curlerId == curler_.curlerId)

                        Nothing ->
                            teamShots
                    )
                        -- |> List.filter (\teamShot -> teamShot.turn == "I" || teamShot.turn == "O")
                        |> List.filter (\teamShot -> teamShot.throw /= "X")

                shotsByThrow : String -> List TeamShot -> List TeamShot
                shotsByThrow throw shots_ =
                    shots_
                        |> List.filter (\shot -> shot.throw == throw)

                shotsByTurn : String -> List TeamShot -> List TeamShot
                shotsByTurn turn shots_ =
                    shots_
                        |> List.filter (\shot -> shot.turn == turn)

                shotsByAllDraws : List TeamShot -> List TeamShot
                shotsByAllDraws shots_ =
                    shots_
                        |> List.filter (\shot -> String.contains shot.throw "EFGHJ")

                shotsByAllTakeouts : List TeamShot -> List TeamShot
                shotsByAllTakeouts shots_ =
                    shots_
                        |> List.filter (\shot -> String.contains shot.throw "ABCD")

                shotsToPoints shots_ =
                    shots_ |> List.map .rating |> List.map String.toInt |> List.filterMap identity |> List.sum

                throwLabels : List ( String, String )
                throwLabels =
                    [ ( "A", "Takeout" )
                    , ( "B", "Hit and roll" )
                    , ( "C", "Clear front" )
                    , ( "D", "Raise take out" )
                    , ( "E", "Draw" )
                    , ( "F", "Front stone" )
                    , ( "G", "Guard" )
                    , ( "H", "Freeze" )
                    , ( "J", "Tap Back" )
                    ]
            in
            (throwLabels
                |> List.map
                    (\throwLabel ->
                        let
                            throw =
                                Tuple.first throwLabel

                            inTurn =
                                shots |> shotsByThrow throw |> shotsByTurn "I" |> List.length

                            inTurnPoints =
                                shots |> shotsByThrow throw |> shotsByTurn "I" |> shotsToPoints

                            outTurn =
                                shots |> shotsByThrow throw |> shotsByTurn "O" |> List.length

                            outTurnPoints =
                                shots |> shotsByThrow throw |> shotsByTurn "O" |> shotsToPoints

                            total =
                                shots |> shotsByThrow throw |> List.length

                            totalPoints =
                                shots |> shotsByThrow throw |> shotsToPoints
                        in
                        { throw = throw
                        , name = Tuple.second throwLabel
                        , inTurn = inTurn |> String.fromInt
                        , inTurnPercentage = round ((toFloat inTurnPoints / toFloat (inTurn * 4)) * 100) |> String.fromInt
                        , outTurn = outTurn |> String.fromInt
                        , outTurnPercentage = round ((toFloat outTurnPoints / toFloat (outTurn * 4)) * 100) |> String.fromInt
                        , total = total |> String.fromInt
                        , totalPercentage = round ((toFloat totalPoints / toFloat (total * 4)) * 100) |> String.fromInt
                        }
                    )
            )
                ++ [ { throw = ""
                     , name = "All Draws"
                     , inTurn = shots |> shotsByAllDraws |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByAllDraws |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> shotsByAllDraws |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   , { throw = ""
                     , name = "All Takeouts"
                     , inTurn = shots |> shotsByAllTakeouts |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByAllTakeouts |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> shotsByAllTakeouts |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   , { throw = ""
                     , name = "Total"
                     , inTurn = shots |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsToPoints) / toFloat ((shots |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   ]

        viewHeader align label =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Font.semiBold
                , Background.color theme.greyLight
                ]
                (el [ align ] (text (translate translations label)))

        viewCell align content onType =
            el
                [ El.padding 15
                , Border.widthEach
                    { top =
                        if onType == "" then
                            1

                        else
                            0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.color theme.grey
                , if onType == "" then
                    Font.semiBold

                  else
                    Font.regular
                ]
                (el [ align ]
                    (text
                        (if content == "0" || content == "NaN" then
                            "-"

                         else
                            content
                        )
                    )
                )

        viewThrowsTable throws_ =
            El.table []
                { data = throws_
                , columns =
                    [ { header = viewHeader El.alignLeft "Type"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignLeft throw.name throw.throw
                      }
                    , { header = viewHeader El.alignRight "Inturn"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.inTurn throw.throw
                      }
                    , { header = viewHeader El.alignRight "Inturn %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.inTurnPercentage throw.throw
                      }
                    , { header = viewHeader El.alignRight "Outturn"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.outTurn throw.throw
                      }
                    , { header = viewHeader El.alignRight "Outturn %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.outTurnPercentage throw.throw
                      }
                    , { header = viewHeader El.alignRight "Total"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.total throw.throw
                      }
                    , { header = viewHeader El.alignRight "Total %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.totalPercentage throw.throw
                      }
                    ]
                }
    in
    column [ El.width El.fill, El.spacing 30, El.paddingEach { top = 0, right = 0, bottom = 140, left = 0 } ]
        [ row [ El.width El.fill ]
            [ el [ Font.size 24, El.width El.fill ]
                (text
                    (translate translations
                        ((if cumulative then
                            "cumulative_"

                          else
                            ""
                         )
                            ++ "statistics_by_team"
                        )
                    )
                )
            , column [ El.spacing 10, El.alignRight ]
                [ if cumulative then
                    El.none

                  else
                    el [ El.alignRight ] viewDrawSelector
                , el [ El.alignRight ] viewTeamSelector
                ]
            ]
        , column [ El.width El.fill, El.spacing 20 ]
            (case team of
                Just team_ ->
                    [ el [ Font.size 20 ] (text team_.name)
                    , viewThrowsTable (throws team_ Nothing)
                    , el [] (text "")
                    ]
                        ++ List.map
                            (\curler ->
                                column [ El.spacing 20 ]
                                    [ el [ Font.size 20, El.paddingEach { top = 10, right = 0, bottom = 0, left = 0 } ] (text curler.name)
                                    , viewThrowsTable (throws team_ (Just curler))
                                    ]
                            )
                            team_.lineup

                Nothing ->
                    []
            )
        ]
