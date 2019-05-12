module Routing.Router exposing (Model, Msg(..), header, init, page, pageView, update, view)

import Browser
import Browser.Navigation
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Elements
import Html
import Html.Attributes
import Html.Events
import Page.Grading
import Page.TaskInput
import Palette
import Routing.Helpers exposing (..)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , taskInputModel : Page.TaskInput.Model
    , gradingModel : Page.Grading.Model
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | ChangedClass String
    | GotTaskInputMsg Page.TaskInput.Msg
    | GotGradingMsg Page.Grading.Msg


init : Model
init =
    let
        ( taskInputModel, _ ) =
            Page.TaskInput.init

        ( gradingModel, _ ) =
            Page.Grading.init
    in
    { route = TaskInput
    , taskInputModel = taskInputModel
    , gradingModel = gradingModel
    }


view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view toMsg sharedState model =
    let
        body =
            page sharedState (pageView sharedState model)
                |> E.map toMsg
    in
    { title = "foo"
    , body = [ E.layout [] body ]
    }


page sharedState body =
    E.column [ E.height E.fill, E.width E.fill, Font.color Palette.fontColor, Font.size 13, E.paddingXY 20 0 ]
        [ header sharedState
        , E.el [ E.height E.fill, E.width E.fill ] body
        ]


header sharedState =
    E.row [ E.width E.fill, E.height (E.px 75), E.alignTop, Border.color Palette.lightBlue, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 } ]
        [ E.column [ E.alignLeft, E.padding 10, E.width <| E.fillPortion 3, Font.color Palette.lightOrange, Font.size 24, Font.bold ] [ E.text "education assistant" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Podsumowanie" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Wprowadzanie wyników" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Zadania domowe" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1, E.padding 0, E.spacingXY 0 7 ]
            [ classSelector sharedState
            , E.el [ E.centerX ] <| Elements.primaryButtonScaled "Zarządzaj klasą" (NavigateTo TaskInput) 0.5
            ]
        ]


resetSelectAttributes =
    [ Html.Attributes.style "border" "none"
    , Html.Attributes.style "-moz-appearance" "none"
    , Html.Attributes.style "appearance" "none"
    , Html.Attributes.style "-ms-appearance" "none"
    , Html.Attributes.style "-webkit-appearance" "none"
    , Html.Attributes.style "background" "#FFFFFF"
    ]


classSelector : SharedState -> E.Element Msg
classSelector sharedState =
    Html.select ([ Html.Events.onInput ChangedClass, Html.Attributes.style "text-align" "center" ] ++ resetSelectAttributes) (classOptions sharedState)
        |> E.html
        |> E.el [ E.centerX, E.width E.fill ]


classOptions sharedState =
    let
        isSelected class =
            sharedState.chosenClass == class

        classText class =
            if isSelected class then
                Html.text class.name

            else
                Html.text class.name
    in
    List.map (\class -> Html.option [ Html.Attributes.selected <| isSelected class ] [ classText class ]) sharedState.allClasses


pageView : SharedState -> Model -> E.Element Msg
pageView sharedState model =
    case model.route of
        TaskInput ->
            Page.TaskInput.view sharedState model.taskInputModel
                |> E.map GotTaskInputMsg

        Grading ->
            Page.Grading.view sharedState model.gradingModel
                |> E.map GotGradingMsg

        NotFound ->
            E.text "404"


update : SharedState -> Model -> Msg -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState model msg =
    case msg of
        UrlChange url ->
            ( { model | route = parseUrl url }
            , Cmd.none
            , NoUpdate
            )

        NavigateTo route ->
            ( { model | route = route }
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        ChangedClass className ->
            ( model, Cmd.none, UpdateClass className )

        GotTaskInputMsg subMsg ->
            let
                ( nextModel, nextCmd ) =
                    Page.TaskInput.update sharedState subMsg model.taskInputModel
            in
            ( { model | taskInputModel = nextModel }
            , Cmd.map GotTaskInputMsg nextCmd
            , NoUpdate
            )

        GotGradingMsg subMsg ->
            let
                ( nextModel, nextCmd ) =
                    Page.Grading.update sharedState subMsg model.gradingModel
            in
            ( { model | gradingModel = nextModel }
            , Cmd.map GotGradingMsg nextCmd
            , NoUpdate
            )
