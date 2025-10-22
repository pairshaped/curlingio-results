module Results.Reports.View exposing (viewReport, viewReports)

import Element as El exposing (Element, column, el, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Element.Lazy as Lazy
import Html.Attributes exposing (class)
import Results.Reports.Attendance
import Results.Reports.CompetitionMatrix
import Results.Reports.HogLineViolation
import Results.Reports.PositionalComparison
import Results.Reports.ScoringAnalysis
import Results.Reports.ScoringAnalysisByHammer
import Results.Reports.ScoringAndPercentages
import Results.Reports.StatisticsByTeam
import Results.Reports.TeamRosters
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


viewNoDataForRoute : List Translation -> Element Msg
viewNoDataForRoute translations =
    el [] (text (translate translations "no_data_for_route"))


viewReports : Theme -> List Translation -> Event -> Element Msg
viewReports theme translations event =
    let
        hasCompetitionMatrix =
            List.any (\stage -> stage.stageType == RoundRobin) event.stages

        hasAttendance =
            (List.map .attendance event.draws |> List.sum) > 0

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
            Lazy.lazy3 Results.Reports.CompetitionMatrix.view theme translations event

        "team_rosters" ->
            Lazy.lazy3 Results.Reports.TeamRosters.view theme translations event.teams

        "attendance" ->
            Lazy.lazy3 Results.Reports.Attendance.view theme translations event.draws

        "scoring_analysis" ->
            column [ El.width El.fill, El.paddingXY 0 20, El.spacing 20, Font.size 24 ]
                [ text (translate translations "scoring_analysis")
                , Lazy.lazy4 Results.Reports.ScoringAnalysis.view theme translations event Nothing
                ]

        "scoring_analysis_by_hammer" ->
            Lazy.lazy3 Results.Reports.ScoringAnalysisByHammer.view theme translations event

        "hog_line_violation" ->
            Lazy.lazy3 Results.Reports.HogLineViolation.view theme translations event

        "scoring_and_percentages" ->
            Lazy.lazy5 Results.Reports.ScoringAndPercentages.viewForDraw theme translations eventConfig event eventConfig.drawSelected

        "positional_percentage_comparison" ->
            -- Tie in to routing to get the currently selected stage?
            Lazy.lazy4 Results.Reports.PositionalComparison.viewPercentage theme translations eventConfig event

        "positional_plus_minus_comparison" ->
            -- Tie in to routing to get the currently selected stage?
            Lazy.lazy4 Results.Reports.PositionalComparison.viewPlusMinus theme translations eventConfig event

        "statistics_by_team" ->
            Lazy.lazy5 Results.Reports.StatisticsByTeam.view theme translations eventConfig event False

        "cumulative_statistics_by_team" ->
            Lazy.lazy5 Results.Reports.StatisticsByTeam.view theme translations eventConfig event True

        _ ->
            Lazy.lazy viewNoDataForRoute translations
