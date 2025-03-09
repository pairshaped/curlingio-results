module Results.Reports.ScoringAnalysisByHammer exposing (view)

import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes exposing (class)
import List.Extra
import Results.Helpers exposing (..)
import Results.Reports.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


view : Theme -> List Translation -> Event -> Element Msg
view theme translations event =
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
