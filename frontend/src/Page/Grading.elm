module Page.Grading exposing (Model, Msg(..), init, view, update)

import Element as E
import SharedState exposing (SharedState)


type alias Model =
    {}


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    E.text "grading module"


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedSate msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
