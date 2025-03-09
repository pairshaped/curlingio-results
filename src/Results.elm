module Results exposing (main)

import Browser
import Results.Rest
import Results.Update
import Results.View



-- MAIN


main =
    Browser.element
        { init = Results.Rest.init
        , update = Results.Update.update
        , subscriptions = Results.Update.subscriptions
        , view = Results.View.view
        }
