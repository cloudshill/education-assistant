module Main exposing (Model, Msg, init, update, view)

import Browser
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


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Category =
    String


type alias Class =
    String


type alias Task =
    { category : Category
    , maxPoints : Int
    }


type Model
    = TaskInput Page.TaskInput.Model


type alias HeaderModel a =
    { a | class : Class }


type Msg
    = NoOp
    | GotTaskInputMsg Page.TaskInput.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { chosenClass = "5A"
            , tasks = []
            , categoryInput = ""
            , maxPointsInput = ""
            }
    in
    ( TaskInput model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg { title, body } =
            { title = title, body = [ E.layout [] <| page <| E.map toMsg body ] }
    in
    case model of
        TaskInput subModel ->
            viewPage GotTaskInputMsg <| Page.TaskInput.view subModel


page body =
    E.column [ E.height E.fill, E.width E.fill, Font.color Palette.fontColor, Font.size 13 ]
        [ header
        , body
        ]


header =
    E.row [ E.width E.fill, E.height <| E.fillPortion 1, E.alignTop, Border.color Palette.lightBlue, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 } ]
        [ E.column [ E.alignLeft, E.padding 5, E.width <| E.fillPortion 3, Font.color Palette.lightOrange, Font.size 16, Font.bold ] [ E.text "education assistant" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Podsumowanie" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Wprowadzanie wyników" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1 ] [ E.el [ E.centerX ] <| E.text "Zadania domowe" ]
        , E.column [ E.alignRight, E.padding 5, E.width <| E.fillPortion 1, E.padding 0, E.spacingXY 0 7 ]
            [ E.el [ E.centerX ] <| E.text <| "Klasa " ++ "test"
            , E.el [ E.centerX ] <| Elements.primaryButtonScaled "Zarządzaj klasą" NoOp 0.5
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotTaskInputMsg subMsg, TaskInput subModel ) ->
            Page.TaskInput.update subMsg subModel
                |> updateWith TaskInput GotTaskInputMsg model

        -- Discard messages without a matching page
        ( _, _ ) ->
            ( model, Cmd.none )


updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
