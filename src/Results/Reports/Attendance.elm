module Results.Reports.Attendance exposing (view)

import Element as El exposing (Element, column, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes exposing (class)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


view : Theme -> List Translation -> List Draw -> Element Msg
view theme translations draws =
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
                        \idx _ ->
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
