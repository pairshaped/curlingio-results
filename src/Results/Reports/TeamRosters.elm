module Results.Reports.TeamRosters exposing (view)

import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes exposing (class)
import List.Extra
import Results.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


view : Theme -> List Translation -> List Team -> Element Msg
view theme translations teams =
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
