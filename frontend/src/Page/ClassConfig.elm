module Page.ClassConfig exposing (Model, Msg, init, update, view)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    E.text "config"


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
    ( model, Cmd.none )
