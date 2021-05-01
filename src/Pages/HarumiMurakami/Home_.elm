module Pages.HarumiMurakami.Home_ exposing (Model, Msg, page)

import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Gen.Params.Example exposing (Params)
import Gen.Route exposing (Route)
import Html.Attributes
import Html.Events
import Models.Book exposing (Book)
import Page
import Request
import Shared
import Svg exposing (Svg, svg)
import Svg.Attributes
import Task
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.pageOptions
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { req : Request.With Params
    , isDark : Bool
    , text : String
    }


init : Request.With Params -> Shared.PageOptions -> ( Model, Cmd Msg )
init req sharedOptions =
    ( Model req sharedOptions.isDark "", Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | UpdateText String
    | ScrollToBook Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateText string ->
            ( { model | text = string }, Cmd.none )

        ScrollToBook index ->
            let
                bookIndex =
                    "book-" ++ String.fromInt index
            in
            ( model
            , Browser.Dom.getElement bookIndex
                |> Task.andThen
                    (\info ->
                        Browser.Dom.getViewportOf "book-container"
                            |> Task.andThen
                                (\info2 ->
                                    Browser.Dom.setViewportOf "book-container" (info.element.x - 148 + info2.viewport.x) 0
                                )
                    )
                |> Task.attempt (\_ -> NoOp)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home"
    , element =
        UI.layout model.req.route
            (row
                [ height fill
                , width fill
                ]
                [ --- Width is 148
                  sidebar
                , column
                    [ alignTop
                    , moveDown UI.sidebarHeight
                    , htmlAttribute (Html.Attributes.style "width" "calc(100vw - 148px)")
                    ]
                    [ el [ height (px 60) ] none
                    , renderHeader
                    , el [ height (px 60) ] none
                    , renderBookList Models.Book.allBooks
                    , el [ height (px 160) ] none
                    , row []
                        [ el [ width (px 65) ] none
                        , el [ height (px 80), width (px 80), Background.color UI.cream ] none
                        , el [ width (px 20) ] none
                        , FeatherIcons.playCircle
                            |> FeatherIcons.withSize 36
                            |> FeatherIcons.withStrokeWidth 1
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> el [ Font.color UI.black, centerX, centerY, moveUp 20 ]
                        , el [ width (px 20) ] none
                        , column [ width (px 944), height fill ]
                            [ el [ Font.medium, UI.rubik, alignTop ] (text "What I Talk About When I Talk About Running")
                            , el [ height (px 4) ] none
                            , row [ width fill ]
                                [ el [ UI.rubik, alignTop, Font.size 16 ] (text "Chapter 1")
                                , el [ UI.rubik, alignTop, alignRight, Font.size 16 ] (text "7:01 / 18:54")
                                ]
                            , row [ width fill, alignBottom ]
                                [ el [ alpha 0.4, Background.color UI.black, width (px 2), height (px 16) ] none
                                , el [ alpha 0.4, Background.color UI.black, width fill, height (px 2) ] none
                                , el [ alpha 0.4, Background.color UI.black, width (px 2), height (px 16) ] none
                                ]
                            ]

                        -- ,
                        ]
                    ]
                ]
            )
    }


sidebar =
    column [ paddingXY 60 50, spacing 40, height fill ]
        [ renderIcon [ centerY ] FeatherIcons.music
        , renderIcon [ centerY ] FeatherIcons.book
        , renderIcon [ centerY ] FeatherIcons.bookmark
        , renderIcon [ alignBottom ] FeatherIcons.home
        ]


renderIcon attr icon =
    icon
        |> FeatherIcons.withSize 28
        |> FeatherIcons.withStrokeWidth 1.6
        |> FeatherIcons.toHtml []
        |> Element.html
        |> el (Font.color UI.black :: attr)


renderHeader =
    row
        [ width (fill |> maximum 1165)
        , behindContent
            (el
                [ height (px 540)
                , width (px 1100)
                , moveRight 65
                , moveUp 60
                , Background.color UI.cream
                ]
                none
            )
        ]
        [ el
            [ Font.light
            , Font.size 56
            , width fill
            ]
            (text "Most Popular Picks")
        , el [ width (px 40) ] none
        , renderIcon [ centerY, alignRight ] FeatherIcons.menu
        , el [ width (px 65) ] none
        ]


renderBookList books =
    el
        [ height (px 270)
        , htmlAttribute (Html.Attributes.class "keep-scrolling")
        , width fill
        , htmlAttribute (Html.Attributes.id "book-container")
        , scrollbarX
        , htmlAttribute (Html.Attributes.style "scroll-behavior" " smooth")
        ]
    <|
        row
            [ spacing 70
            ]
            (List.indexedMap renderBook books
                ++ [ el [ htmlAttribute (Html.Attributes.style "width" "calc(100vw - 148px - 65px - 65px - 6px - 440px)") ] none ]
            )


renderBook : Int -> Book -> Element Msg
renderBook index book =
    row
        [ UI.rubik
        , htmlAttribute (Html.Attributes.id ("book-" ++ String.fromInt index))
        , Element.Events.onClick (ScrollToBook index)
        ]
        [ image
            [ width (px 180)
            , height (px 270)
            ]
            { description = "Cover for " ++ book.title
            , src = book.imageUrl
            }
        , el [ width (px 40) ] none
        , column []
            [ el
                [ width (px 220)
                , Font.size 24
                , Font.regular
                ]
                (paragraph [] [ text book.title ])
            , el [ height (px 24) ] none
            , row [ spacing 4 ]
                [ renderFilledStar []
                , renderStar []
                , renderStar []
                , renderStar []
                , renderStar []
                ]
            , el [ height (px 8) ] none
            , paragraph [ Font.size 18 ]
                [ el [ Font.regular ] (text "See reviews ")
                , el [ Font.light ] (text ("(" ++ String.fromInt book.reviews ++ ")"))
                ]
            ]
        ]


svgFeatherIcon className fillColor =
    svg
        [ Svg.Attributes.class <| "feather feather-" ++ className
        , Svg.Attributes.fill fillColor
        , Svg.Attributes.height "16"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width "16"
        ]


renderFilledStar attr =
    svgFeatherIcon "star"
        "#f9af1e"
        [ Svg.polygon [ Svg.Attributes.points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2" ] []
        ]
        |> Element.html
        |> el (Font.color UI.black :: attr)


renderStar attr =
    svgFeatherIcon "star"
        "#00000033"
        [ Svg.polygon [ Svg.Attributes.points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2" ] []
        ]
        |> Element.html
        |> el (Font.color UI.black :: attr)
