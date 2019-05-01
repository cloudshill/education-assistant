module Router exposing (Model, Msg(..), Route(..), header, init, page, pageView, parseUrl, routeParser, update, view)

import Browser
import Browser.Navigation
import Element as E
import Element.Border as Border
import Element.Font as Font
import Elements
import Html
import Html.Attributes
import Html.Events
import Page.TaskInput
import Palette
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import Url.Parser exposing ((</>))


type alias Model =
    { route : Route
    , taskInputModel : Page.TaskInput.Model
    }


type Route
    = NotFound
    | TaskInput


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
            page sharedState (pageView sharedState model)
                |> E.map toMsg
    in
    { title = "foo"
    , body = [ E.layout [] body ]
    }


page sharedState body =
    E.column [ E.height E.fill, E.width E.fill, Font.color Palette.fontColor, Font.size 13 ]
        [ header sharedState
        , body
        ]


header sharedState =
    E.row [ E.width E.fill, E.height <| E.fillPortion 1, E.alignTop, Border.color Palette.lightBlue, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 } ]
        [ E.column [ E.alignLeft, E.padding 5, E.width <| E.fillPortion 3, Font.color Palette.lightOrange, Font.size 16, Font.bold ] [ E.text "education assistant" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Podsumowanie" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Wprowadzanie wyników" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Zadania domowe" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1, E.padding 0, E.spacingXY 0 7 ]
            [ classSelector sharedState

            , E.el [ E.centerX ] <| Elements.primaryButtonScaled "Zarządzaj klasą" (NavigateTo TaskInput) 0.5
            ]
        ]


classSelector : SharedState -> E.Element Msg
classSelector sharedState =
    E.html <|
        Html.div []
            [ Html.select [ Html.Events.onInput ChangedClass ] (classOptions sharedState)
            ]


classOptions sharedState =
    let
        isSelected class =
            sharedState.chosenClass == class
    in
    List.map (\class -> Html.option [ Html.Attributes.selected <| isSelected class ] [ Html.text class.name ]) sharedState.allClasses


pageView : SharedState -> Model -> E.Element Msg
pageView sharedState model =
    case model.route of
        TaskInput ->
            Page.TaskInput.view sharedState model.taskInputModel
                |> E.map GotTaskInputMsg

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

        GotTaskInputMsg subMsg ->
            let
                ( nextModel, nextCmd ) =
                    Page.TaskInput.update subMsg model.taskInputModel
            in
            ( { model | taskInputModel = nextModel }
            , Cmd.map GotTaskInputMsg nextCmd
            , NoUpdate
            )

        ChangedClass className ->
            ( model, Cmd.none, UpdateClass className)


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            NotFound

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFound


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map TaskInput Url.Parser.top ]

reverseRoute : Route -> String
reverseRoute route =
    case route of
        _ ->
            "#/"