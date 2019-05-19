module Page.ClassConfig exposing (Model, Msg, init, update, view)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import Palette
import SharedState exposing (SharedState, Class)


type alias Model =
    {
        selectedClass : Class
    }


type Msg
    = NoOp


init : SharedState -> Model
init sharedState =
    { selectedClass = sharedState.chosenClass
    }


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    E.row [ E.width E.fill, E.height E.fill, E.paddingXY 0 30, E.spacingXY 30 0 ]
        [ E.column [ E.width <| E.fillPortion 1, E.alignTop, Border.widthEach { top = 1, left = 1, right = 1, bottom = 0 }, Border.rounded 2, Border.color Palette.tableBorder ] <| classChoosers sharedState
        , E.column [ E.width <| E.fillPortion 4, E.alignTop ] [ E.text "right" ]
        ]


classChoosers : SharedState -> List (E.Element Msg)
classChoosers sharedState =
    let
        lastIndex =
            List.length sharedState.allClasses - 1
    in
    List.indexedMap (\i class -> classChooser class (i == lastIndex)) sharedState.allClasses


classChooser : Class -> Bool -> E.Element Msg
classChooser class isLast =
    let
        attributes =
            [ E.width E.fill
            , E.height <| E.px 40
            , E.paddingXY 5 0
            , Border.solid
            , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
            , Border.color Palette.tableBorder
            ]

        allAttributes =
            if isLast then
                Border.rounded 2 :: attributes

            else
                attributes
    in
    Input.button allAttributes { onPress = Nothing, label = E.text class.name }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
    ( model, Cmd.none )
