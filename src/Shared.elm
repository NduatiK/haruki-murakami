module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , PageOptions
    , init
    , subscriptions
    , update
    )

import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { pageOptions : PageOptions }


type alias PageOptions =
    { isDark : Bool }


type Msg
    = NoOp
    | ToggleDarkMode


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { pageOptions = { isDark = False } }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg ({ pageOptions } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleDarkMode ->
            ( { model | pageOptions = { pageOptions | isDark = not model.pageOptions.isDark } }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
