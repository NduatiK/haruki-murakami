module Pages.HarumiMurakami.Home_ exposing (Model, Msg, page)

import Animator
import Animator.Inline
import Browser.Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Gen.Params.Example exposing (Params)
import Gen.Route exposing (Route(..))
import Html.Attributes
import Models.Book exposing (Book)
import Page
import Request
import Shared
import Svg exposing (Svg, svg)
import Svg.Attributes
import Task
import Time
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req shared.pageOptions
        , update = update
        , view = view
        , subscriptions =
            \model ->
                animator
                    |> Animator.toSubscription Tick model
        }



-- INIT


type alias Model =
    { req : Request.With Params
    , books : List Book
    , state : Animator.Timeline State
    , lastBook : Maybe Book
    , animationData : Maybe { pageWidth : Float, bookOffset : Float }
    , fast : Bool
    }


type State
    = Default
    | BookOpen Int Book


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching
            .state
            (\newState model ->
                { model | state = newState }
            )


init : Request.With Params -> Shared.PageOptions -> ( Model, Cmd Msg )
init req _ =
    ( { req = req
      , books = Models.Book.allBooks
      , state = Animator.init Default
      , lastBook = Nothing
      , animationData = Nothing
      , fast = True
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | SelectBook Int Book
    | UpdateSelectBookLocations ( Float, Float )
    | CloseBook
    | ToggleSpeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleSpeed ->
            ( { model | fast = not model.fast }
            , Cmd.none
            )

        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        SelectBook index book ->
            let
                pageId =
                    "page"

                selectedBookId =
                    "book-" ++ String.fromInt index
            in
            ( { model
                | state =
                    if model.fast then
                        model.state
                            |> Animator.go Animator.verySlowly (BookOpen index book)

                    else
                        model.state
                            |> Animator.go (Animator.millis 4000) (BookOpen index book)
                , lastBook = Just book
              }
            , Browser.Dom.getElement pageId
                |> Task.andThen
                    (\pageInfo ->
                        Browser.Dom.getElement selectedBookId
                            |> Task.andThen
                                (\bookInfo ->
                                    Task.succeed ( pageInfo.scene.width, bookInfo.element.x )
                                )
                    )
                |> Task.attempt
                    (\result ->
                        case result of
                            Err _ ->
                                NoOp

                            Ok ( pageWidth, bookOffset ) ->
                                UpdateSelectBookLocations ( pageWidth, bookOffset )
                    )
            )

        UpdateSelectBookLocations ( pageWidth, bookOffset ) ->
            ( { model
                | animationData =
                    Just { pageWidth = pageWidth, bookOffset = bookOffset }
              }
            , Cmd.none
            )

        CloseBook ->
            ( { model
                | state =
                    if model.fast then
                        model.state
                            |> Animator.go Animator.verySlowly Default

                    else
                        model.state
                            |> Animator.go (Animator.millis 4000) Default
              }
            , Cmd.none
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
        column
            [ htmlAttribute (Html.Attributes.style "transition" "color 200ms, background-color 200ms")
            , htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
            , height fill
            , scrollbarY
            , width fill
            , Font.color (UI.withAlpha 0.8 UI.black)
            , Background.color UI.white
            , inFront (bookDetailPage model)
            ]
            [ homePage model
            ]
    }


homePage model =
    row
        [ height fill
        , htmlAttribute (Html.Attributes.style "width" "100vw")
        , htmlAttribute (Html.Attributes.id "page")
        ]
        [ sidebar model
        , el
            [ inFront (navbar True model)
            , width fill
            , height fill
            ]
          <|
            column
                [ alignTop
                , moveDown UI.sidebarHeight
                , htmlAttribute (Html.Attributes.style "width" ([ "calc(100vw - ", String.fromInt sidebarWidth, "px)" ] |> String.join ""))
                ]
                [ el [ height (px 60) ] none
                , renderHeader model.state
                , el [ height (px 60) ] none
                , renderBookList model.state model.animationData model.books
                , el [ height (px 160) ] none
                , row
                    [ onOpenAnimateX model.state (always -800)
                    ]
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
                    , column
                        [ width (px 944)
                        , height fill
                        ]
                        [ el [ Font.medium, UI.rubik, alignTop ] (text "What I Talk About When I Talk About Running")
                        , el [ height (px 4) ] none
                        , row [ width fill ]
                            [ el [ UI.rubik, alignTop, Font.size 16 ] (text "Chapter 1")
                            , el [ UI.rubik, alignTop, alignRight, Font.size 16 ] (text "14:01 / 18:54")
                            ]
                        , row
                            [ width fill
                            , alignBottom
                            , inFront
                                (row [ width fill, centerY ]
                                    [ el [ width (fillPortion 13) ] none
                                    , el
                                        [ Background.color (rgb255 127 194 205)
                                        , width (px 12)
                                        , height (px 12)
                                        , Border.rounded 8
                                        ]
                                        none
                                    , el [ width (fillPortion 5) ] none
                                    ]
                                )
                            ]
                            [ el [ alpha 0.4, Background.color UI.black, width (px 2), height (px 16) ] none
                            , el [ alpha 0.4, Background.color UI.black, width fill, height (px 2) ] none
                            , el [ alpha 0.4, Background.color UI.black, width (px 2), height (px 16) ] none
                            ]
                        ]
                    ]
                ]
        ]


bookDetailCardWidth =
    840


bookDetailRightPadding =
    80


bookDetailPage model =
    let
        animateOffset =
            htmlAttribute <|
                Animator.Inline.xy model.state <|
                    \state ->
                        case ( state, model.animationData ) of
                            ( Default, Just { pageWidth, bookOffset } ) ->
                                { x =
                                    Animator.at
                                        -(pageWidth - bookDetailCardWidth - bookDetailRightPadding - 148 - bookOffset + 110 + 40 + 8)
                                , y = Animator.at 160
                                }

                            _ ->
                                { x = Animator.at -110
                                , y = Animator.at 160
                                }

        scale =
            Animator.linear model.state <|
                \state ->
                    case state of
                        Default ->
                            Animator.at 1

                        BookOpen bookIndex _ ->
                            Animator.at (220 / 180)
    in
    el
        [ clipX
        , Background.color (rgb255 225 214 205)

        , htmlAttribute
            (Html.Attributes.style "width"
                ((String.fromFloat <|
                    Animator.linear model.state <|
                        \state ->
                            if state == Default then
                                Animator.at 0

                            else
                                Animator.at 100
                 )
                    ++ "vw"
                )
            )
        , height fill
        ]
    <|
        case model.lastBook of
            Nothing ->
                none

            Just book ->
                row
                    [ inFront (navbar False model)
                    , height fill
                    , width fill
                    , behindContent
                        (el
                            [ Background.tiled "light-noise.svg"
                            , alpha 0.1
                            , height fill
                            , width fill
                            , moveUp 20
                            , moveLeft 20
                            ]
                            none
                        )
                    , behindContent
                        (el
                            [ Background.tiled "dark-noise.svg"
                            , alpha 0.05
                            , height fill
                            , width fill
                            ]
                            none
                        )
                    , htmlAttribute
                        (Html.Attributes.style "width" "100vw")
                    ]
                    [ column [ UI.raleway, Font.color (rgb 1 1 1), paddingXY 60 0 ]
                        [ el [ Font.size 100 ] (text "❝")
                        , paragraph [ UI.robotoSlab, Font.size 40 ]
                            [ text """
                            Dreamlike and
                            compelling...
                            Murakami is a
                            genius
                            """
                            ]
                        , el [ height (px 40) ] none
                        , el
                            [ width (px 30)
                            , height (px 2)
                            , Background.color UI.white
                            ]
                            none
                        , el [ height (px 40) ] none
                        , el [ Font.size 24, Font.extraBold, Font.italic, Font.letterSpacing 0.1 ] (text "Chicago Tribune")
                        ]
                    , el
                        [ height (px 610)
                        , width (px bookDetailCardWidth)
                        , alignRight
                        , alignTop
                        , moveDown UI.sidebarHeight
                        , Background.color UI.white
                        , inFront
                            (image
                                [ width (px (round (180 * scale)))
                                , height (px (round (270 * scale)))
                                , alignTop
                                , moveDown 140
                                , htmlAttribute (Html.Attributes.style "z-index" "200")
                                , animateOffset
                                , Border.shadow
                                    { blur = 15
                                    , color = UI.withAlpha 0.2 UI.black
                                    , offset = ( 0, 10 )
                                    , size = 0
                                    }
                                ]
                                { description = "Cover for " ++ book.title
                                , src = book.imageUrl
                                }
                            )

                        ]
                        (row [ width fill, height fill ]
                            [ el [ width (px 190) ] none
                            , column
                                [ UI.raleway, Font.color UI.black, width fill ]
                                [ paragraph [ UI.robotoSlab, Font.size 40, width fill ]
                                    [ text book.title ]
                                , el [ height (px 40) ] none
                                , el [ Font.medium ] (text "Haruki Murakami")
                                , el [ height (px 40) ] none
                                , row [ width fill ]
                                    [ renderMetadata "Originally Published" "July 1, 1960"
                                    , renderMetadata "Publisher" "Shin Publishing"
                                    ]
                                , el [ height (px 40) ] none
                                , renderMetadata "Categories" "Fictional, Magical Realism, Asian Literature, Domestic Fiction"
                                ]
                            , el [ width (px 40), alignRight ] none
                            , el
                                [ height fill
                                , width (px sidebarWidth)
                                , Border.widthEach
                                    { bottom = 0
                                    , left = 1
                                    , right = 0
                                    , top = 0
                                    }
                                , Border.color (UI.withAlpha 0.2 UI.black)
                                , alignRight
                                ]
                              <|
                                column [ centerY, centerX, spacing 80 ]
                                    [ renderBookAction "Save" FeatherIcons.bookmark
                                    , renderBookAction "Preview" FeatherIcons.playCircle
                                    , renderBookAction "Get Book" FeatherIcons.shoppingBag
                                    ]
                            ]
                        )
                    , el
                        [ height (px 540)
                        , width (px bookDetailRightPadding)
                        , alignRight
                        ]
                        none
                    ]


renderMetadata title subtitle =
    column [ spacing 8, width fill, Font.regular ]
        [ el [ Font.color UI.lessLightBlue, width fill ] (text title)
        , paragraph [ Font.color (UI.withAlpha 0.8 UI.black), width fill, Font.size 18 ] [ text subtitle ]
        ]


renderBookAction action icon =
    column [ spacing 8, centerX, Font.center, Font.regular, UI.rubik ]
        [ icon
            |> FeatherIcons.withSize 36
            |> FeatherIcons.withStrokeWidth 1
            |> FeatherIcons.toHtml []
            |> Element.html
            |> el [ Font.color UI.black, centerX ]
        , text action
        ]


sidebarWidth =
    148


sidebar model =
    column
        [ paddingXY 60 50
        , spacing 40
        , height fill
        , Background.color UI.white
        , width (px sidebarWidth)

        ]
        [ renderIcon [ centerY ] FeatherIcons.music
        , renderIcon [ centerY ] FeatherIcons.book
        , renderIcon [ centerY ] FeatherIcons.bookmark
        , renderIcon [ alignBottom, Element.Events.onClick ToggleSpeed ]
            (if model.fast then
                FeatherIcons.zap

             else
                FeatherIcons.zapOff
            )
        , renderIcon [ alignBottom ] FeatherIcons.home
        ]


renderIcon attr icon =
    icon
        |> FeatherIcons.withSize 28
        |> FeatherIcons.withStrokeWidth 1.6
        |> FeatherIcons.toHtml []
        |> Element.html
        |> el (Font.color UI.black :: attr)


renderHeader bookState =
    row
        [ width fill
        , height (px 15)
        , behindContent
            (el
                [ moveRight 65
                , moveUp 60
                ]
             <|
                el
                    [ height (px 540)
                    , width (px 1100)
                    , Background.color UI.cream
                    , onOpenAnimateX bookState (always -800)
                    ]
                    none
            )
        , behindContent
            (el
                [ moveRight 1165
                , moveUp 60
                , alignRight
                ]
             <|
                el
                    [ height (px 540)
                    , width (px 1100)
                    , Background.color UI.lightBlue
                    , onOpenAnimateX bookState (always -1400)
                    ]
                    none
            )
        , height (px 40)
        ]
        [ el
            [ Font.light
            , Font.size 56
            , width (fill |> maximum (1000 + 25))
            , onOpenAnimateX bookState (always 400)
            ]
            (text "Most Popular Picks")
        , el [ width (px 40) ] none
        , renderIcon
            [ alignLeft
            , centerY
            , onOpenAnimateX bookState (always -800)
            ]
            FeatherIcons.menu
        , el [ width (px 65) ] none
        ]


renderBookList state animationData books =
    el
        [ height (px 330)
        , htmlAttribute (Html.Attributes.class "keep-scrolling")
        , width fill
        , htmlAttribute (Html.Attributes.id "book-container")
        , htmlAttribute (Html.Attributes.style "scroll-behavior" " smooth")
        , htmlAttribute (Html.Attributes.style "overflow-y" "visible")
        , scrollbarX
        ]
    <|
        row
            [ spacing 70
            ]
            (List.indexedMap (renderBook state animationData) books
                ++ [ el
                        [ htmlAttribute
                            (Html.Attributes.style "width"
                                ([ "calc(100vw - ", String.fromInt sidebarWidth, "px - 65px - 65px - 6px - 440px)" ]
                                    |> String.join ""
                                )
                            )
                        ]
                        none
                   ]
            )



-- renderBook : Int -> Book -> Element Msg


renderBook bookState animationData index book =
    let
        noMovement =
            { x = Animator.at 0
            , y = Animator.at 0
            }

        animateOffset =
            htmlAttribute <|
                Animator.Inline.xy bookState <|
                    \state ->
                        case ( state, animationData ) of
                            ( BookOpen bookIndex _, Just { pageWidth, bookOffset } ) ->
                                if index == bookIndex then
                                    { x =
                                        Animator.at
                                            (pageWidth - bookDetailCardWidth - bookDetailRightPadding - bookOffset - 110)
                                    , y = Animator.at 0
                                    }

                                else
                                    noMovement

                            _ ->
                                noMovement

        scale =
            Animator.linear bookState <|
                \state ->
                    case state of
                        Default ->
                            Animator.at 1

                        BookOpen bookIndex _ ->
                            if index == bookIndex then
                                Animator.at (220 / 180)

                            else
                                Animator.at 1
    in
    row
        [ UI.rubik
        , htmlAttribute (Html.Attributes.id ("book-" ++ String.fromInt index))
        , Element.Events.onClick
            (if Animator.current bookState == Default then
                SelectBook index book

             else
                CloseBook
            )

         , onOpenAnimateX bookState <|
            \bookIndex ->
                if index > bookIndex then
                    1000

                else if index == bookIndex then
                    0

                else
                    -1000
        , height (px 270)
        ]
        [ image
            [ width (px (round (180 * scale)))
            , height (px (round (270 * scale)))
            , animateOffset

            , alignTop
            , if Animator.current bookState /= Default then
                htmlAttribute (Html.Attributes.style "z-index" "100")

              else
                moveUp 0
            ]
            { description = "Cover for " ++ book.title
            , src = book.imageUrl
            }
        , el [ width (px 40) ] none
        , column
            [ onOpenAnimateX bookState <|
                \bookIndex ->
                    if index == bookIndex then
                        1000

                    else
                        0
            ]
            [ el
                [ width (px 220)
                , Font.size 24
                , Font.regular
                ]
                (paragraph [] [ text book.title ])
            , el [ height (px 24) ] none
            , row [ spacing 4 ]
                (List.repeat book.stars (renderFilledStar [])
                    ++ List.repeat (5 - book.stars) (renderStar [])
                )
            , el [ height (px 8) ] none
            , paragraph [ Font.size 18 ]
                [ el [ Font.regular ] (text "See reviews ")
                , el [ Font.light ] (text ("(" ++ commaSeparatedInt book.reviews ++ ")"))
                ]
            ]
        ]


navbar forHome model =
    let
        icon =
            if forHome then
                FeatherIcons.menu

            else
                FeatherIcons.arrowLeft

        navigationTitle =
            if forHome then
                "Harumi Murakami"

            else
                "Back"
    in
    el
        [ width fill
        , height (px UI.sidebarHeight)
        ]
    <|
        row
            [ paddingEach
                { bottom = 20
                , left =
                    if forHome then
                        60

                    else
                        60 + sidebarWidth
                , right = 60
                , top = 20
                }
            , spacing 25
            , width fill
            , centerY
            ]
            [ Input.button []
                { onPress = Just CloseBook
                , label =
                    icon
                        |> FeatherIcons.withSize 28
                        |> FeatherIcons.withStrokeWidth 2.4
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> el [ Font.color UI.black, alpha 0.8 ]
                }
            , el [ Font.size 28, Font.regular ] (text navigationTitle)
            , el [ alignRight ] (renderSearchBar model)
            ]


renderSearchBar model =
    let
        searchIcon =
            FeatherIcons.search
                |> FeatherIcons.withSize 28
                |> FeatherIcons.withStrokeWidth 2.4
                |> FeatherIcons.toHtml []
                |> Element.html
                |> el [ Font.color UI.black, alpha 0.8 ]
    in
    row
        [ spacing 8
        , paddingXY 0 8
        , Border.color (UI.withAlpha 0.2 UI.black)
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
                (px <|
                    round <|
                        Animator.linear model.state <|
                            \state ->
                                if state == Default then
                                    Animator.at 140

                                else
                                    Animator.at 1
                )
            ]
            none
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


commaSeparatedInt int =
    int
        |> String.fromInt
        |> String.foldr
            (\x acc ->
                case acc of
                    [ a, b, c ] :: tail ->
                        [ [ x ], [ a, b, c ] ] ++ tail

                    head :: tail ->
                        (x :: head) :: tail

                    a ->
                        [ x ] :: a
            )
            []
        |> List.map String.fromList
        |> String.join ","


onOpenAnimateX bookState fn =
    htmlAttribute <|
        Animator.Inline.xy bookState <|
            \state ->
                case state of
                    Default ->
                        { x = Animator.at 0, y = Animator.at 0 }

                    BookOpen bookIndex _ ->
                        { x = Animator.at (fn bookIndex), y = Animator.at 0 }


onOpenScale bookState fn =
    htmlAttribute <|
        Animator.Inline.scale bookState <|
            \state ->
                case state of
                    Default ->
                        Animator.at 1

                    BookOpen bookIndex _ ->
                        Animator.at (fn bookIndex)
