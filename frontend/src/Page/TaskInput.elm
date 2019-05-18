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
import List.Extra as List
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
    | Grading GradingModel
    | WentBackToTaskInput GradingModel


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
    | ClickedContinueGrading GradingModel
    | ClickedGoBack GradingModel
    | ChangedScore Student Task String


init : ( Model, Cmd Msg )
init =
    let
        model =
            { tasks = [ { category = "Temat zadania 1", maxPoints = 5 }, { category = "Temat zadanie 2", maxPoints = 10 }, { category = "Temat zadania 3", maxPoints = 15 } ]
            , categoryInput = ""
            , maxPointsInput = ""
            , phase = TaskInput
            }
    in
    ( model, Cmd.none )


extractGradingModel : Phase -> Maybe GradingModel
extractGradingModel phase =
    case phase of
        TaskInput ->
            Nothing

        Grading gradingModel ->
            Just gradingModel

        WentBackToTaskInput gradingModel ->
            Just gradingModel


view : SharedState -> Model -> E.Element Msg
view sharedState model =
    case model.phase of
        Grading gradingModel ->
            gradingView model.tasks gradingModel

        _ ->
            taskInputView model


taskInputView : Model -> E.Element Msg
taskInputView model =
    E.row [ E.alignBottom, E.width E.fill, E.height E.fill, E.paddingXY 0 30 ]
        [ leftSite model
        , rightSite (extractGradingModel model.phase)
        ]


gradingView : List Task -> GradingModel -> E.Element Msg
gradingView tasks scores =
    let
        rows =
            List.indexedMap scoreRow scores
    in
    E.column [ E.width E.fill, E.height E.fill, E.spacing 15, E.paddingXY 10 30 ]
        [ Elements.secondaryButton "Cofnij do wprowdzania zadań" (ClickedGoBack scores)
        , E.column [ E.width E.fill, E.spacing 10 ] <|
            gradingHeaderRow tasks
                :: rows
        ]


gradingHeaderRow tasks =
    let
        columns =
            List.map (\task -> E.column [ E.width <| E.fillPortion 1 ] [ E.text task.category ]) tasks
    in
    E.row [ E.height E.fill, E.width E.fill, E.spacing 5, E.padding 5, Font.bold, Font.center ] <|
        E.column [ E.width <| E.fillPortion 1 ] [ E.text "Imię i nazwisko" ]
            :: columns


scoreRow index studentScore =
    let
        columns =
            List.map (\( task, score ) -> E.column [ E.width <| E.fillPortion 1 ] [ scoreInput studentScore.student task score ]) studentScore.scores
    in
    E.row [ E.paddingXY 0 5, E.width E.fill, Background.color (bgColor index) ] <|
        E.column [ E.height E.fill, E.width E.fill ] [ E.text studentScore.student ]
            :: columns


scoreInput : Student -> Task -> Score -> E.Element Msg
scoreInput student task score =
    let
        text =
            case score of
                NoInput ->
                    ""

                Score value ->
                    String.fromInt value
    in
    Input.text [ Background.color (E.rgba255 0 0 0 0.0), E.padding 0, Border.width 0, onEnter EnterRow ] { text = text, onChange = ChangedScore student task, placeholder = Just <| Input.placeholder [] <| E.text "Points", label = Input.labelHidden "Points" }


leftSite model =
    E.column [ E.width <| E.fillPortion 5, E.height E.fill, E.paddingXY 20 0, E.spacing 20 ]
        [ E.el [ Font.bold, Font.size 18 ] <| E.text "Wprowadź teamty zadań"
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
            [ E.text "Temat" ]
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


rightSite maybeGradingModel =
    let
        gradingButton =
            case maybeGradingModel of
                Just gradingModel ->
                    Elements.primaryButton "Kontynuuj wprowadzanie ocen" (ClickedContinueGrading gradingModel)

                Nothing ->
                    Elements.primaryButton "Rozpocznij wprowadzanie ocen" ClickedStartGrading
    in
    E.column [ E.width <| E.fillPortion 3, E.height E.fill, E.spacing 5, E.paddingXY 0 40, E.centerY ]
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
                    ( { model
                        | tasks = model.tasks ++ [ task ]
                        , categoryInput = ""
                        , maxPointsInput = ""
                        , phase = addTaskToGradingModel model.phase task
                      }
                    , Cmd.none
                    )

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
                ( { model | phase = Grading scores }, Cmd.none )

        ClickedContinueGrading gradingModel ->
            ( { model | phase = Grading gradingModel }, Cmd.none )

        ClickedGoBack gradingModel ->
            ( { model | phase = WentBackToTaskInput gradingModel }, Cmd.none )

        ChangedScore student task scoreString ->
            ( { model | phase = updateScore model.phase student task scoreString }, Cmd.none )


addTaskToGradingModel : Phase -> Task -> Phase
addTaskToGradingModel phase task =
    case phase of
        WentBackToTaskInput scores ->
            WentBackToTaskInput <| List.map (\studentScore -> { studentScore | scores = studentScore.scores ++ [ ( task, NoInput ) ] }) scores

        _ ->
            phase


updateScore : Phase -> Student -> Task -> String -> Phase
updateScore phase student updatedTask newScoreStr =
    case phase of
        Grading oldScores ->
            Grading <|
                List.map
                    (\studentScore ->
                        if studentScore.student == student then
                            { studentScore | scores = updateStudentScore studentScore.scores updatedTask newScoreStr }

                        else
                            studentScore
                    )
                    oldScores

        _ ->
            phase


updateStudentScore scores updatedTask newScoreStr =
    let
        newScore =
            String.toInt newScoreStr
                |> Maybe.map (\i -> Score i)
                |> Maybe.withDefault NoInput
    in
    List.map
        (\( task, oldScore ) ->
            if task == updatedTask then
                ( task, newScore )

            else
                ( task, oldScore )
        )
        scores


createTask categoryInput maxPointsInput =
    String.toInt maxPointsInput
        |> Maybe.filter (\_ -> categoryInput /= "")
        |> Maybe.filter (\maxPoints -> maxPoints > 0)
        |> Maybe.map (\maxPoints -> { category = categoryInput, maxPoints = maxPoints })


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
