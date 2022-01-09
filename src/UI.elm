module UI exposing
    ( black
    , cream
    , layout
    , lessLightBlue
    , lightBlue
    , raleway
    , red
    , robotoSlab
    , rubik
    , sidebarHeight
    , white
    , withAlpha
    )

import Element exposing (..)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Gen.Route exposing (Route(..))
import Html.Attributes


sidebarHeight =
    120



-- layout : Route -> Element msg -> Element msg


layout currentRoute child onRoutePageAnimator =
    column
        [ htmlAttribute (Html.Attributes.style "transition" "color 200ms, background-color 200ms")
        , height fill
        , scrollbarY
        , width fill
        , Font.color
            (withAlpha 0.8 black)
        , Element.Background.color
            white
        ]
        [ el
            [ inFront (navbar currentRoute onRoutePageAnimator)
            , width fill
            , height fill
            ]
            child
        ]


navbar currentRoute onRoutePageAnimator =
    let
        icon =
            if currentRoute == HarukiMurakami__Home_ then
                FeatherIcons.menu

            else
                FeatherIcons.arrowLeft

        navigationTitle =
            if currentRoute == HarukiMurakami__Home_ then
                "Haruki Murakami"

            else
                "Back"
    in
    el
        [ width fill
        , height (px sidebarHeight)
        ]
    <|
        row
            [ paddingEach
                { bottom = 20
                , left = 60
                , right = 60
                , top = 20
                }
            , spacing 25
            , width fill
            , centerY
            ]
            [ icon
                |> FeatherIcons.withSize 28
                |> FeatherIcons.withStrokeWidth 2.4
                |> FeatherIcons.toHtml []
                |> Element.html
                |> el [ Font.color black, alpha 0.8 ]
            , el [ Font.size 28, Font.regular ] (text navigationTitle)
            , el [ alignRight ] (renderSearchBar currentRoute)
            ]


renderSearchBar currentRoute =
    let
        isExpanded =
            currentRoute == HarukiMurakami__Home_

        searchIcon =
            FeatherIcons.search
                |> FeatherIcons.withSize 28
                |> FeatherIcons.withStrokeWidth 2.4
                |> FeatherIcons.toHtml []
                |> Element.html
                |> el [ Font.color black, alpha 0.8 ]
    in
    row
        [ spacing 8
        , paddingXY 0 8
        , Border.color (withAlpha 0.2 black)
        , Border.widthEach
            { bottom = 2
            , left = 0
            , right = 0
            , top = 0
            }
        ]
        [ el [] searchIcon
        , el
            [ height fill
            , width
                (px
                    (if isExpanded then
                        200

                     else
                        1
                    )
                )
            ]
            none
        ]



-- FONTS


robotoSlab =
    Font.family
        [ Font.typeface "Roboto Slab"
        , Font.serif
        ]


rubik =
    Font.family
        [ Font.typeface "Rubik"
        , Font.sansSerif
        ]


raleway =
    Font.family
        [ Font.typeface "Raleway"
        , Font.sansSerif
        ]



-- COLORS


red =
    rgb255 246 52 32


black =
    rgb255 25 25 27


white =
    rgb255 249 249 251


lightBlue =
    rgb255 232 244 250


lessLightBlue =
    rgb255 146 208 216


cream =
    rgb255 244 233 223


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgbColor =
            toRgb color
    in
    rgba rgbColor.red rgbColor.green rgbColor.blue alpha
