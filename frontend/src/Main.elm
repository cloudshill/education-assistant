module Main exposing (Model, Msg, init, update, view)

import Browser exposing (UrlRequest(..))
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
import Page.TaskInput
import Palette
import Routing.Router
import SharedState exposing (SharedState, Student)
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Category =
    String


type alias Task =
    { category : Category
    , maxPoints : Int
    }


type alias Model =
    { appState : AppState
    , url : Url
    , navKey : Browser.Navigation.Key
    }


type AppState
    = Ready SharedState Routing.Router.Model


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotRouterMsg Routing.Router.Msg
    | NoOp


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        sharedState =
            { navKey = key
            , chosenClass = { name = "5A", students = ["Basia", "Ryszard", "Mateusz"]}
            , allClasses = 
                [ { name = "7D", students = ["Ola", "Tomek", "Marek"]}
                , { name = "5A", students = ["Basia", "Ryszard", "Mateusz"]}
                , { name = "6C", students = ["Michał", "Małgosia", "Jaś"]}
                ]
            }
    in
    ( { appState = Ready sharedState Routing.Router.init
      , url = url
      , navKey = key
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    case model.appState of
        Ready sharedState subModel ->
            Routing.Router.view GotRouterMsg sharedState subModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            updateRouter { model | url = url } (Routing.Router.UrlChange url)

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navKey (Url.toString url) )

                External url ->
                    ( model, Browser.Navigation.load url )

        GotRouterMsg routerMsg ->
            updateRouter model routerMsg


        NoOp ->
            ( model, Cmd.none )


updateRouter model routerMsg =
    case model.appState of
        Ready sharedState routerModel ->
            let
                ( nextRouterModel, routerCmd, sharedStateUpdate ) =
                    Routing.Router.update sharedState routerModel routerMsg

                newSharedState = SharedState.update sharedState sharedStateUpdate

            in
            ( { model | appState = Ready newSharedState nextRouterModel }
            , Cmd.map GotRouterMsg routerCmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
