module Results exposing (main)

import Browser
import Json.Decode
import Results.Rest
import Results.Types exposing (Model, Msg)
import Results.Update
import Results.View



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { init = Results.Rest.init
        , update = Results.Update.update
        , subscriptions = Results.Update.subscriptions
        , view = Results.View.view
        }
