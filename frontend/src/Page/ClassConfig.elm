module Page.ClassConfig exposing (Model, Msg, init, update, view)

import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import List.Extra as List
import Palette
import SharedState exposing (Class, SharedState)


type alias Model =
    { selectedClass : Class
    }


type Msg
    = NoOp
    | SelectedClass Class


init : SharedState -> Model
init sharedState =
    { selectedClass = sharedState.chosenClass
    }


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    E.row [ E.width E.fill, E.height E.fill, E.paddingXY 0 30, E.spacingXY 30 0 ]
        [ E.column [ E.width <| E.fillPortion 1, E.alignTop, Border.widthEach { top = 1, left = 1, right = 1, bottom = 0 }, Border.rounded 2, Border.color Palette.tableBorder ] <|
            classChoosers sharedState model.selectedClass
        , E.column [ E.width <| E.fillPortion 4, E.alignTop ] [ classSettings model.selectedClass ]
        ]


classChoosers : SharedState -> Class -> List (E.Element Msg)
classChoosers sharedState chosenClass =
    let
        lastIndex =
            List.length sharedState.allClasses - 1
    in
    List.indexedMap (\i class -> classChooser class (i == lastIndex) (chosenClass == class)) sharedState.allClasses


classChooser : Class -> Bool -> Bool -> E.Element Msg
classChooser class isLast isSelected =
    let
        attributes =
            [ Border.solid
            , Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
            , Border.color Palette.tableBorder
            , E.width E.fill
            , E.height <| E.px 40
            , E.focused [ Border.glow (E.rgba 0 0 0 0) 0 ]
            , E.centerY
            ]

        lastRoundedBorder =
            if isLast then
                [ Border.rounded 2 ]

            else
                []

        selectedBorder =
            if isSelected then
                [ Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }, Border.color Palette.primary ]

            else
                []
    in
    Input.button (lastRoundedBorder ++ attributes) 
        { onPress = Just <| SelectedClass class
        , label = E.el (selectedBorder ++ [ E.height E.fill ]) <| E.el ([ E.paddingXY 5 0, E.centerY ]) <| E.text class.name
        }


classSettings : Class -> E.Element Msg
classSettings selectedClass =
    E.column [E.spacing 20  ] <| List.map (\student -> E.row [E.width E.fill] [ E.column [] [E.text student ]]) selectedClass.students

update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectedClass newClass ->
            ( { model | selectedClass = newClass }
            , Cmd.none
            )
