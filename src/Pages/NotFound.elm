module Pages.NotFound exposing (view)

import Element exposing (..)
import Element.Background
import Element.Font
import Gen.Route
import UI
import View exposing (View)


view : View msg
view =
    { title = "Home"
    , element =
        column
            [ padding 20
            , width fill
            , height fill
            , Element.Background.color UI.white
            ]
            [ link []
                { url = Gen.Route.toHref Gen.Route.HarumiMurakami__Home_
                , label = text "Home"
                }
            , el
                [ Element.Font.size 244
                , Element.Font.bold
                , centerX
                , centerY
                , Element.Font.color UI.black
                ]
                (text "404")
            ]
    }
