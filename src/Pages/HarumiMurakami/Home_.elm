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
    , animationData : Maybe AnimationData
    , fast : Bool
    }


type alias AnimationData =
    { pageWidth : Float
    , bookOffset : Float
    }


type State
    = Default
    | BookOpen Int Book Bool


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
    | OpenPreview


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
                            |> Animator.go Animator.verySlowly (BookOpen index book False)

                    else
                        model.state
                            |> Animator.go (Animator.millis 4000) (BookOpen index book False)
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

        OpenPreview ->
            ( { model
                | state =
                    case Animator.current model.state of
                        Default ->
                            model.state

                        BookOpen index book bool ->
                            if model.fast then
                                model.state
                                    |> Animator.go Animator.verySlowly (BookOpen index book (not bool))

                            else
                                model.state
                                    |> Animator.go (Animator.millis 10000) (BookOpen index book (not bool))
              }
            , Cmd.none
            )



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


homePage : Model -> Element Msg
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


bookDetailCardWidth : number
bookDetailCardWidth =
    840


bookDetailRightPadding : number
bookDetailRightPadding =
    80


bookDetailPage : Model -> Element Msg
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

                        BookOpen _ _ _ ->
                            Animator.at (220 / 180)

        customPercent modifier =
            Animator.move model.state <|
                \state ->
                    modifier <|
                        case state of
                            Default ->
                                Animator.at 0

                            BookOpen _ _ False ->
                                Animator.at 0

                            BookOpen _ _ True ->
                                Animator.at 1

        openPercent =
            customPercent
                (Animator.leaveSmoothly 0.2
                    >> Animator.arriveSmoothly 0.2
                )

        delayedOpenPercent =
            customPercent
                (Animator.leaveSmoothly 0.8
                    >> Animator.arriveSmoothly 0.4
                )

        darkGrey =
            rgb255 45 45 47

        page2 =
            inFront
                (el
                    [ width (px (round (180 * scale)))
                    , height (px (round (270 * scale)))
                    , alignLeft
                    , animateOffset
                    , Background.color UI.white
                    , Element.Events.onClick OpenPreview
                    , behindContent
                        (column [ paddingXY 14 38, width fill, Font.regular, spacing 8, height fill ]
                            [ el [ Font.size 8, centerX, UI.robotoSlab ] (text "CHAPTER I")
                            , paragraph
                                [ Font.size 18, Font.center, width fill, UI.rubik, squishFont, spacing 8 ]
                                [ text
                                    """
Six Fingers and
Four Breasts
"""
                                ]
                            , paragraph
                                [ spacing 4
                                , htmlAttribute (Html.Attributes.class "tiny_text")
                                , clipY
                                , width fill
                                , height fill
                                ]
                                [ el [ Font.size 18, alignLeft, UI.robotoSlab ]
                                    (text "A")
                                , el
                                    [ Font.size 4
                                    , UI.robotoSlab
                                    , htmlAttribute (Html.Attributes.style "line-height" "4px !important")
                                    , spacing 4
                                    ]
                                  <|
                                    text """
orem ipsum dolor sit amet, sea pertinax pertinacia appellantur in, est ad esse assentior mediocritatem, magna populo menandri cum te. Vel augue menandri eu, at integre appareat splendide duo. Est ne tollit ullamcorper, eu pro falli diceret perpetua, sea ferri numquam legendos ut. Diceret suscipiantur at nec, his ei nulla mentitum efficiantur. Errem saepe ei vis. Per melius aperiri eu. Et interesset philosophia vim, graece denique intellegam duo at, te vix quot apeirian dignissim.

Per melius aperiri eu. Et interesset philosophia vim, graece denique intellegam duo at, te vix quot apeirian dignissim. Ei essent percipitur nam, natum possit interpretaris sea ea. Cum assum adipisci cotidieque ut, ut veri tollit duo. Erat idque volutpat mea ut, mel nominati splendide vulputate ea. Ei essent percipitur nam, natum possit interpretaris sea ea. Cum assum adipisci cotidieque ut, ut veri tollit duo. Erat idque volutpat mea ut, mel nominati splendide vulputate ea.

Per melius aperiri eu. Et interesset philosophia vim, graece denique intellegam duo at, te vix quot apeirian dignissim. Ei essent percipitur nam, natum possit interpretaris sea ea. Cum assum adipisci cotidieque ut, ut veri tollit duo. Erat idque volutpat mea ut, mel nominati splendide vulputate ea. Ei essent percipitur nam, natum possit interpretaris sea ea. Cum assum adipisci cotidieque ut, ut veri tollit duo. Erat idque volutpat mea ut, mel nominati splendide vulputate ea.
"""
                                ]
                            ]
                        )
                    ]
                 <|
                    el
                        [ width (px (round (180 * scale)))
                        , height (px (round (270 * scale)))
                        , alignTop
                        , moveDown 180
                        , Element.Events.onClick OpenPreview
                        , Background.color UI.lessLightBlue
                        , htmlAttribute (Html.Attributes.style "transform-origin" "left")
                        , if isOpening then
                            calcRotation delayedOpenPercent

                          else
                            calcRotation openPercent
                        ]
                        none
                )

        calcRotation percent =
            let
                minAsMaximum =
                    if percent > 0.5 then
                        2 * (1 - percent)

                    else
                        percent * 2
            in
            htmlAttribute (Html.Attributes.style "transform" ("rotateY(" ++ String.fromFloat (180 * percent) ++ "deg) skewY(" ++ String.fromFloat (minAsMaximum * -10) ++ "deg)"))

        page1 =
            inFront
                (el
                    [ width (px (round (180 * scale)))
                    , height (px (round (270 * scale)))
                    , alignLeft
                    , animateOffset
                    , Element.Events.onClick OpenPreview
                    ]
                 <|
                    el
                        [ width (px (round (180 * scale)))
                        , height (px (round (270 * scale)))
                        , alignTop
                        , moveDown 180
                        , Element.Events.onClick OpenPreview
                        , Background.color darkGrey
                        , htmlAttribute (Html.Attributes.style "transform-origin" "left")
                        , page1Animation
                        ]
                        none
                )

        isOpening =
            case Animator.current model.state of
                BookOpen _ _ bool ->
                    bool

                _ ->
                    False

        page1Animation =
            if isOpening then
                calcRotation openPercent

            else
                calcRotation delayedOpenPercent
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
                        ]
                      <|
                        el
                            [ height fill
                            , Background.color UI.white
                            , inFront
                                (el
                                    [ htmlAttribute <|
                                        Animator.Inline.scale model.state <|
                                            \_ ->
                                                Animator.at (1 + openPercent * 0.1)
                                    , htmlAttribute <|
                                        Animator.Inline.xy model.state <|
                                            \state ->
                                                case state of
                                                    Default ->
                                                        { x = Animator.at 0, y = Animator.at 0 }

                                                    BookOpen _ _ False ->
                                                        { x = Animator.at 0, y = Animator.at 0 }

                                                    BookOpen _ _ True ->
                                                        { x = Animator.at 0, y = Animator.at -20 }
                                    , inFront
                                        (el
                                            [ width (px (round (180 * scale)))
                                            , height (px (round (270 * scale)))
                                            , alignLeft
                                            , animateOffset
                                            , Background.color UI.lightBlue
                                            , Element.Events.onClick OpenPreview
                                            , Border.shadow
                                                { blur = 15
                                                , color = UI.withAlpha 0.2 UI.black
                                                , offset = ( 0, 10 )
                                                , size = 0
                                                }
                                            ]
                                            none
                                        )
                                    , if delayedOpenPercent > 0.5 then
                                        moveUp 0

                                      else
                                        page2
                                    , if delayedOpenPercent > 0.5 then
                                        moveUp 0

                                      else
                                        page1
                                    , inFront
                                        (el
                                            [ width (px (round (180 * scale)))
                                            , height (px (round (270 * scale)))
                                            , alignLeft
                                            , animateOffset
                                            , Element.Events.onClick OpenPreview
                                            ]
                                         <|
                                            image
                                                [ width (px (round (180 * scale)))
                                                , height (px (round (270 * scale)))
                                                , alignTop
                                                , clip
                                                , moveDown 140
                                                , htmlAttribute (Html.Attributes.style "z-index" "200")
                                                , htmlAttribute (Html.Attributes.style "transform-origin" "left")
                                                , page1Animation

                                                -- , Background.color UI.lightBlue
                                                , case Animator.current model.state of
                                                    Default ->
                                                        moveUp 0

                                                    BookOpen _ _ False ->
                                                        htmlAttribute (Html.Attributes.class "sheen")

                                                    BookOpen _ _ True ->
                                                        moveUp 0
                                                , Element.Events.onClick OpenPreview
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
                                    , if (isOpening && openPercent > 0.5) || (not isOpening && openPercent > 0.3) then
                                        page1

                                      else
                                        moveUp 0
                                    , if delayedOpenPercent > 0.5 then
                                        page2

                                      else
                                        moveUp 0
                                    ]
                                    none
                                )
                            ]
                            (row [ width fill, height fill ]
                                [ el [ width (px 120) ] none
                                , el
                                    [ height fill
                                    , width (px (round ((bookDetailCardWidth - 120 - sidebarWidth) * (1 - openPercent))))
                                    , clipX
                                    ]
                                  <|
                                    column
                                        [ centerY
                                        , UI.raleway
                                        , Font.color UI.black
                                        , width (px (round (bookDetailCardWidth - 120 - sidebarWidth)))
                                        , paddingEach
                                            { bottom = 0
                                            , left = round 70
                                            , right = round (40 * (1 - openPercent))
                                            , top = 0
                                            }
                                        ]
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
                                , el
                                    [ height fill
                                    , width (px sidebarWidth)
                                    , Border.widthEach
                                        { bottom = 0
                                        , left = round (1 - openPercent)
                                        , right = round openPercent
                                        , top = 0
                                        }
                                    , Border.color (UI.withAlpha 0.2 UI.black)
                                    ]
                                  <|
                                    column [ centerY, centerX, spacing 80 ]
                                        [ renderBookAction "Save" FeatherIcons.bookmark
                                        , renderBookAction "Preview" FeatherIcons.playCircle
                                        , renderBookAction "Get Book" FeatherIcons.shoppingBag
                                        ]
                                , el
                                    [ height fill
                                    , width (px (round ((bookDetailCardWidth - 120 - sidebarWidth) * openPercent)))
                                    , clipX
                                    ]
                                  <|
                                    column
                                        [ centerY
                                        , Font.color UI.black
                                        , width (px (round (bookDetailCardWidth - 140 - sidebarWidth)))
                                        , paddingEach
                                            { bottom = 0
                                            , left = round 60
                                            , right = round 50
                                            , top = 0
                                            }
                                        , UI.rubik
                                        ]
                                        [ el [ Font.regular, UI.rubik, Font.size 24, squishFont ]
                                            (text "Excerpt")
                                        , el [ height (px 40) ] none
                                        , el [ Font.color UI.lessLightBlue, Font.size 20, squishFont ] (text "Book 1 - Chapter 1")
                                        , el [ height (px 40) ] none
                                        , paragraph [ UI.robotoSlab, Font.size 18, spacingXY 0 10 ] <|
                                            [ text
                                                """
                            I wanted to ignore the phone, not only 
                            because the spaghetti was nearly 
                            done, but because Claudio Abbado was
                            bringing the London Symphony to its
                            musical climax. Finally, though, I had
                            to give in. It could have been somebody
                            with news of a job opening. I lowered
                            the flame, went to the living room, and
                            picked up the receiver.
                            """
                                            ]
                                        , el [ height (px 60) ] none
                                        , el
                                            [ Font.color UI.black
                                            , Font.size 20
                                            , Font.regular
                                            , squishFont
                                            ]
                                            (text "See full excerpt")
                                        , el
                                            [ width (px 300)
                                            , height (px 8)
                                            , Border.widthEach
                                                { bottom = 2
                                                , left = 0
                                                , right = 0
                                                , top = 0
                                                }
                                            , Border.color (UI.withAlpha 0.3 UI.black)
                                            ]
                                            none
                                        ]
                                ]
                            )
                    , el
                        [ height (px 540)
                        , width (px (round (bookDetailRightPadding * (1 - openPercent))))
                        , alignRight
                        ]
                        none
                    ]


renderMetadata : String -> String -> Element msg
renderMetadata title subtitle =
    column [ spacing 8, width fill, Font.regular ]
        [ el [ Font.color UI.lessLightBlue, width fill ] (text title)
        , paragraph [ Font.color (UI.withAlpha 0.8 UI.black), width fill, Font.size 18 ] [ text subtitle ]
        ]


renderBookAction : String -> FeatherIcons.Icon -> Element Msg
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


sidebarWidth : number
sidebarWidth =
    148


sidebar : Model -> Element Msg
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


renderIcon : List (Attr () msg) -> FeatherIcons.Icon -> Element msg
renderIcon attr icon =
    icon
        |> FeatherIcons.withSize 28
        |> FeatherIcons.withStrokeWidth 1.6
        |> FeatherIcons.toHtml []
        |> Element.html
        |> el (Font.color UI.black :: attr)


renderHeader : Animator.Timeline State -> Element Msg
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
            , width (fill |> maximum (slideDistance + 25))
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


renderBookList : Animator.Timeline State -> Maybe AnimationData -> List Book -> Element Msg
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


slideDistance =
    1200


renderBook : Animator.Timeline State -> Maybe AnimationData -> Int -> Book -> Element Msg
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
                            ( BookOpen bookIndex _ _, Just { pageWidth, bookOffset } ) ->
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

                        BookOpen bookIndex _ _ ->
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
                    slideDistance

                else if index == bookIndex then
                    0

                else
                    -slideDistance
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
                        slideDistance

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


navbar : Bool -> Model -> Element Msg
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


renderSearchBar : Model -> Element msg
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


svgFeatherIcon : String -> List (Svg msg) -> Element msg
svgFeatherIcon fillColor svgData =
    svg
        [ Svg.Attributes.fill fillColor
        , Svg.Attributes.height "16"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.strokeLinejoin "round"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.viewBox "0 0 24 24"
        , Svg.Attributes.width "16"
        ]
        svgData
        |> Element.html


renderFilledStar : List (Attr () msg) -> Element msg
renderFilledStar attr =
    svgFeatherIcon "#f9af1e" starData
        |> el (Font.color UI.black :: attr)


renderStar : List (Attr () msg) -> Element msg
renderStar attr =
    svgFeatherIcon "#00000033" starData
        |> el (Font.color UI.black :: attr)


starData : List (Svg msg)
starData =
    [ Svg.polygon [ Svg.Attributes.points "12 2 15.09 8.26 22 9.27 17 14.14 18.18 21.02 12 17.77 5.82 21.02 7 14.14 2 9.27 8.91 8.26 12 2" ] []
    ]


commaSeparatedInt : Int -> String
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


onOpenAnimateX : Animator.Timeline State -> (Int -> Float) -> Attribute Msg
onOpenAnimateX bookState fn =
    htmlAttribute <|
        Animator.Inline.xy bookState <|
            \state ->
                case state of
                    Default ->
                        { x = Animator.at 0, y = Animator.at 0 }

                    BookOpen bookIndex _ _ ->
                        { x = Animator.at (fn bookIndex), y = Animator.at 0 }


squishFont =
    htmlAttribute <|
        Html.Attributes.style "transform" "scaleY(0.9)"
