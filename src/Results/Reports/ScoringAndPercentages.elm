module Results.Reports.ScoringAndPercentages exposing (viewForDraw, viewForGame)

import Element as El exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes exposing (class)
import List.Extra
import Results.Helpers exposing (..)
import Results.Reports.Helpers exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


viewForGame : Theme -> List Translation -> Event -> Game -> Element Msg
viewForGame theme translations event game =
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
                     , gender = Unknown
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


viewForDraw : Theme -> List Translation -> EventConfig -> Event -> Maybe Int -> Element Msg
viewForDraw theme translations eventConfig event onDrawId =
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
                            viewForGame theme translations event game

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
