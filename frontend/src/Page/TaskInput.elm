module Page.TaskInput exposing (Category, Model, Msg(..), Table, Task, bgColor, completedRow, createTask, headerRow, init, inputRow, leftSite, onEnter, rightSite, subscriptions, table, update, view)

import Browser
import Browser.Navigation
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elements
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import Maybe.Extra as Maybe
import Palette
import Routing.Helpers exposing (reverseRoute, Route(..))
import SharedState exposing (SharedState)


type alias Category =
    String


type alias Task =
    { category : Category
    , maxPoints : Int
    }


type alias Model =
    { tasks : List Task
    , categoryInput : String
    , maxPointsInput : String
    }


type alias Table a =
    { a
        | tasks : List Task
        , categoryInput : String
        , maxPointsInput : String
    }


type Msg
    = NoOp
    | ChangeCategoryInput String
    | ChangeMaxPointsInput String
    | EnterRow
    | ClickedStartGrading


init : ( Model, Cmd Msg )
init =
    let
        model =
            { tasks = []
            , categoryInput = ""
            , maxPointsInput = ""
            }
    in
    ( model, Cmd.none )


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    E.row [ E.alignBottom, E.width E.fill, E.height E.fill, E.paddingXY 0 30 ]
        [ leftSite model
        , rightSite
        ]


leftSite model =
    E.column [ E.width <| E.fillPortion 5, E.paddingXY 20 0, E.alignTop, E.spacing 20 ]
        [ E.el [ Font.bold, Font.size 18 ] <| E.text "Wprowadź kategorie zadań"
        , table model
        ]


table : Table a -> E.Element Msg
table model =
    let
        completedRows =
            model.tasks
                |> List.indexedMap (\index task -> ( index + 1, task ))
                |> List.map (\( index, task ) -> completedRow index task)
    in
    E.column [ E.width E.fill ] <|
        headerRow
            :: completedRows
            ++ [ inputRow model.categoryInput model.maxPointsInput ]


headerRow =
    E.row [ E.width E.fill, E.spacing 5, E.padding 5, Font.bold ]
        [ E.column [ E.width <| E.fillPortion 1 ] [ E.text "#" ]
        , E.column [ E.width <| E.fillPortion 5 ] [ E.text "Category" ]
        , E.column [ E.width <| E.fillPortion 1 ] [ E.text "Max points" ]
        ]


completedRow : Int -> Task -> E.Element msg
completedRow num task =
    E.row [ E.width E.fill, E.spacing 5, E.padding 5, Background.color <| bgColor num ]
        [ E.column [ E.width <| E.fillPortion 1 ] [ E.text <| String.fromInt num ]
        , E.column [ E.width <| E.fillPortion 5 ] [ E.text task.category ]
        , E.column [ E.width <| E.fillPortion 1 ] [ E.text <| String.fromInt task.maxPoints ]
        ]


bgColor num =
    if modBy 2 num == 0 then
        E.rgb255 255 255 255

    else
        Palette.grey


inputRow categoryInput maxPointsInput =
    E.row [ E.width E.fill, E.spacing 5, E.padding 5 ]
        [ E.column [ E.width <| E.fillPortion 1, Font.color Palette.lightOrange ] [ E.text "+" ]
        , E.column [ E.width <| E.fillPortion 5 ] [ Input.text [ E.padding 0, Border.width 0, onEnter EnterRow ] { text = categoryInput, onChange = ChangeCategoryInput, placeholder = Just <| Input.placeholder [] <| E.text "Category", label = Input.labelHidden "Category" } ]
        , E.column [ E.width <| E.fillPortion 1 ] [ Input.text [ E.padding 0, Border.width 0, onEnter EnterRow ] { text = maxPointsInput, onChange = ChangeMaxPointsInput, placeholder = Just <| Input.placeholder [] <| E.text "Max points", label = Input.labelHidden "Max points" } ]
        ]


onEnter : Msg -> E.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    E.htmlAttribute <| Html.Events.on "keydown" (Json.andThen isEnter Html.Events.keyCode)


rightSite =
    E.column [ E.width <| E.fillPortion 3, E.spacing 5, E.paddingXY 0 50, E.alignTop ]
        [ Elements.primaryButton "Rozpocznij wprowadzanie ocen" ClickedStartGrading
        , Elements.secondaryButton "Zapisz szkic" NoOp
        ]


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeCategoryInput newCategoryInput ->
            ( { model | categoryInput = newCategoryInput }, Cmd.none )

        ChangeMaxPointsInput newMaxPointsInput ->
            ( { model | maxPointsInput = newMaxPointsInput }, Cmd.none )

        EnterRow ->
            case createTask model.categoryInput model.maxPointsInput of
                Just task ->
                    ( { model | tasks = model.tasks ++ [ task ], categoryInput = "", maxPointsInput = "" }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
        
        ClickedStartGrading ->
            ( model, Browser.Navigation.pushUrl sharedState.navKey (reverseRoute Grading) )


createTask categoryInput maxPointsInput =
    String.toInt maxPointsInput
        |> Maybe.filter (\_ -> categoryInput /= "")
        |> Maybe.filter (\maxPoints -> maxPoints > 0)
        |> Maybe.map (\maxPoints -> { category = categoryInput, maxPoints = maxPoints })


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
