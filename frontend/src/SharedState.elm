module SharedState exposing (SharedState, SharedStateUpdate(..), Student, update)

import Browser.Navigation
import List.Extra as List


type alias Student =
    String


type alias Class =
    { name : String
    , students : List Student
    }


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , chosenClass : Class
    , allClasses : List Class
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateClass String


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        NoUpdate ->
            sharedState

        UpdateClass className ->
            chooseClass className sharedState


chooseClass : String -> SharedState -> SharedState
chooseClass className state =
    let
        chosenClass =
            state.allClasses
                |> List.find (\class -> class.name == className)
                |> Maybe.withDefault state.chosenClass
    in
    { state | chosenClass = chosenClass }
