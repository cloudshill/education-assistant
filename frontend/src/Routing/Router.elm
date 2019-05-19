module Routing.Router exposing (Model, Msg(..), header, init, page, pageView, update, view)

import Browser
import Browser.Navigation
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elements
import Html
import Html.Attributes
import Html.Events
import Page.TaskInput
import Palette
import Routing.Helpers exposing (..)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , taskInputModel : Page.TaskInput.Model
    }


type Msg
    = UrlChange Url
    | NavigateTo Route
    | ChangedClass String
    | GotTaskInputMsg Page.TaskInput.Msg


init : Model
init =
    let
        ( taskInputModel, _ ) =
            Page.TaskInput.init
    in
    { route = TaskInput
    , taskInputModel = taskInputModel
    }


view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view toMsg sharedState model =
    let
        body =
            page sharedState model.route (pageView sharedState model)
                |> E.map toMsg
    in
    { title = "Education assistant"
    , body = [ E.layout [] body ]
    }


page sharedState route body =
    E.column [ E.height E.fill, E.width E.fill, Font.color Palette.fontColor, Font.size 13, E.paddingXY 20 0 ]
        [ header sharedState route
        , E.el [ E.height E.fill, E.width E.fill ] body
        ]


header sharedState route =
    E.row [ E.width E.fill, E.height (E.px 60), E.alignBottom, Border.color Palette.primary, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 } ]
        [ E.column [ E.alignLeft, E.alignBottom, E.paddingXY 0 15, E.width <| E.fillPortion 3, Font.color Palette.secondary, Font.size 24, Font.bold ] [ E.text "education assistant" ]
        , navbarButton "Podsumowanie" ClassView route
        , navbarButton "Wprowadzanie wyników" TaskInput route
        , navbarButton "Zadania domowe" NotFound route
        , navbarButton "Zarządzaj klasami" NotFound route
        , E.column [ E.alignRight, E.alignBottom, E.paddingXY 0 15, E.width <| E.fillPortion 1, E.spacingXY 0 7 ]
            [ classSelector sharedState

            --, E.el [ E.centerX ] <| Elements.primaryButtonScaled "Zarządzaj klasą" (NavigateTo TaskInput) 0.5
            ]
        ]


navbarButton text targetRoute currentRoute =
    let
        borderColor =
            if targetRoute == currentRoute then
                Palette.primary

            else
                E.rgb 1 1 1
    in
    E.column [ E.alignRight, E.alignBottom, E.width <| E.fillPortion 1, E.centerX, E.pointer, E.mouseOver [ Font.color Palette.highlightedFont ], E.htmlAttribute <| Html.Events.onClick (NavigateTo targetRoute) ]
        [ E.el [ E.paddingXY 0 15, E.centerX, Border.color borderColor, Border.solid, Border.widthEach { bottom = 4, top = 0, left = 0, right = 0 } ] <|
            E.text text
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

        ClassView ->
            E.none

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
