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
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, Student)


type alias Category =
    String


type alias Task =
    { category : Category
    , maxPoints : Int
    }


type Phase
    = TaskInput
    | Grading


type Score
    = NoInput
    | Score Int


type alias StudentScore =
    { student : String
    , scores : List ( Task, Score )
    }


type alias GradingModel =
    List StudentScore


type alias Model =
    { tasks : List Task
    , categoryInput : String
    , maxPointsInput : String
    , grading : Maybe GradingModel
    , phase : Phase
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
    | ClickedContinueGrading
    | ClickedGoBack


init : ( Model, Cmd Msg )
init =
    let
        model =
            { tasks = [ { category = "Test 1", maxPoints = 5 }, { category = "Test 2", maxPoints = 10 }, { category = "Test 3", maxPoints = 15 } ]
            , categoryInput = ""
            , maxPointsInput = ""
            , grading = Nothing
            , phase = TaskInput
            }
    in
    ( model, Cmd.none )



--TODO: fix impossible state in view


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    case model.phase of
        TaskInput ->
            taskInputView model

        Grading ->
            Maybe.map (\gradingModel -> gradingView model.tasks gradingModel) model.grading
                |> Maybe.withDefault (E.text "This should never happen")


taskInputView model =
    E.row [ E.alignBottom, E.width E.fill, E.height E.fill, E.paddingXY 0 30 ]
        [ leftSite model
        , rightSite (Maybe.isJust model.grading)
        ]


gradingView : List Task -> GradingModel -> E.Element Msg
gradingView tasks scores =
    let
        rows =
            List.map scoreRow scores
    in
    E.column [ E.width E.fill, E.height E.fill ]
        [ Elements.secondaryButton "Cofnij do wprowdzania kategorii" ClickedGoBack
        , E.column [ E.width E.fill ] <|
            gradingHeaderRow tasks
                :: rows
        ]


gradingHeaderRow tasks =
    let
        columns =
            List.map (\task -> E.column [ E.width <| E.fillPortion 1 ] [ E.text task.category ]) tasks
    in
    E.row [ E.width E.fill, E.spacing 5, E.padding 5, Font.bold, Font.center ] <|
        E.column [ E.width <| E.fillPortion 1 ] [ E.text "Imię i nazwisko" ]
            :: columns


scoreRow score =
    E.row []
        [ E.column [] [ E.text score.student ] ]


leftSite model =
    E.column [ E.width <| E.fillPortion 5, E.height E.fill, E.paddingXY 20 0, E.spacing 20 ]
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
        , E.column
            [ E.width <| E.fillPortion 5 ]
            [ E.text "Kategoria" ]
        , E.column [ E.width <| E.fillPortion 1 ] [ E.text "Maksymalna liczba punktów" ]
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
        [ E.column [ E.width <| E.fillPortion 1, Font.color Palette.secondary ] [ E.text "+" ]
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


rightSite startedGrading =
    let
        gradingButton =
            if startedGrading then
                Elements.primaryButton "Kontynuuj wprowadzanie ocen" ClickedContinueGrading

            else
                Elements.primaryButton "Rozpocznij wprowadzanie ocen" ClickedStartGrading
    in
    E.column [ E.width <| E.fillPortion 3, E.height E.fill, E.spacing 5, E.centerY ]
        [ gradingButton
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
            if List.isEmpty model.tasks then
                ( model, Cmd.none )

            else
                let
                    emptyTaskScores =
                        List.map (\task -> ( task, NoInput )) model.tasks

                    scores =
                        List.map (\student -> { student = student, scores = emptyTaskScores }) sharedState.chosenClass.students
                in
                ( { model | grading = Just scores, phase = Grading }, Cmd.none )

        ClickedContinueGrading ->
            ( { model | phase = Grading }, Cmd.none )

        ClickedGoBack ->
            ( { model | phase = TaskInput }, Cmd.none )


createTask categoryInput maxPointsInput =
    String.toInt maxPointsInput
        |> Maybe.filter (\_ -> categoryInput /= "")
        |> Maybe.filter (\maxPoints -> maxPoints > 0)
        |> Maybe.map (\maxPoints -> { category = categoryInput, maxPoints = maxPoints })


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
