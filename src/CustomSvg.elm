module CustomSvg exposing (..)

import Svg exposing (path, polyline, svg)
import Svg.Attributes exposing (d, fill, height, points, stroke, strokeDasharray, strokeOpacity, viewBox, width)


svgScreenResize forPath =
    svg
        [ width "32"
        , height "32"
        , viewBox "0 0 24 24"
        ]
        [ path [ d forPath ] [] ]


svgExitFullScreen =
    svgScreenResize "M.172 15.828a.5.5 0 0 0 .707 0l4.096-4.096V14.5a.5.5 0 1 0 1 0v-3.975a.5.5 0 0 0-.5-.5H1.5a.5.5 0 0 0 0 1h2.768L.172 15.121a.5.5 0 0 0 0 .707zM15.828.172a.5.5 0 0 0-.707 0l-4.096 4.096V1.5a.5.5 0 1 0-1 0v3.975a.5.5 0 0 0 .5.5H14.5a.5.5 0 0 0 0-1h-2.768L15.828.879a.5.5 0 0 0 0-.707z"


svgFullScreen =
    svgScreenResize "M5.828 10.172a.5.5 0 0 0-.707 0l-4.096 4.096V11.5a.5.5 0 0 0-1 0v3.975a.5.5 0 0 0 .5.5H4.5a.5.5 0 0 0 0-1H1.732l4.096-4.096a.5.5 0 0 0 0-.707zm4.344-4.344a.5.5 0 0 0 .707 0l4.096-4.096V4.5a.5.5 0 1 0 1 0V.525a.5.5 0 0 0-.5-.5H11.5a.5.5 0 0 0 0 1h2.768l-4.096 4.096a.5.5 0 0 0 0 .707z"


svgNoImage =
    svg
        [ width "130"
        , height "130"
        , viewBox "0 0 32 32"
        ]
        [ path [ stroke "#e8e8e8", fill "#eeeeee", d "M30,3.4141,28.5859,2,2,28.5859,3.4141,30l2-2H26a2.0027,2.0027,0,0,0,2-2V5.4141ZM26,26H7.4141l7.7929-7.793,2.3788,2.3787a2,2,0,0,0,2.8284,0L22,19l4,3.9973Zm0-5.8318-2.5858-2.5859a2,2,0,0,0-2.8284,0L19,19.1682l-2.377-2.3771L26,7.4141Z" ] []
        , path [ stroke "#e8e8e8", fill "#eeeeee", d "M6,22V19l5-4.9966,1.3733,1.3733,1.4159-1.416-1.375-1.375a2,2,0,0,0-2.8284,0L6,16.1716V6H22V4H6A2.002,2.002,0,0,0,4,6V22Z" ] []
        ]


viewSvgConnector width_ height_ connectors =
    let
        viewSvgConnectorLine { fromCoords, toCoords } =
            let
                strPoint coords =
                    String.fromInt (Tuple.first coords) ++ "," ++ String.fromInt (Tuple.second coords) ++ " "
            in
            polyline
                [ fill "none"
                , strokeOpacity "0.5"
                , strokeDasharray "3"
                , stroke "#333"
                , points
                    (strPoint ( Tuple.first fromCoords + 6, Tuple.second fromCoords )
                        ++ strPoint ( Tuple.first fromCoords + 11, Tuple.second fromCoords )
                        ++ strPoint ( Tuple.first toCoords - 8, Tuple.second toCoords )
                        ++ strPoint ( Tuple.first toCoords - 3, Tuple.second toCoords )
                    )
                ]
                []
    in
    svg
        [ width (String.fromInt width_)
        , height (String.fromInt height_)
        ]
        (List.map viewSvgConnectorLine connectors)
