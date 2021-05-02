module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (..)
import Element.Background
import Element.Font as Font
import UI


type alias View msg =
    { title : String
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , element = Element.text str
    }


none : View msg
none =
    { title = ""
    , element =
        el
            [ Element.Background.color UI.red
            , width fill
            , height fill
            ]
            Element.none
    }


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , element = Element.map fn view.element
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Element.layoutWith
            { options =
                [ focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ UI.robotoSlab
            , Font.light
            , height fill
            ]
            view.element
        ]
    }
