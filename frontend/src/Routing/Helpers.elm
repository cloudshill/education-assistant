module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>), s)


type Route
    = NotFound
    | TaskInput
    | ClassView


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
        [ Url.Parser.map TaskInput (s "input")
        , Url.Parser.map ClassView (s "class")
        ]


reverseRoute : Route -> String
reverseRoute route =
    case route of
        NotFound ->
            "#/404"

        ClassView ->
            "#/class"

        TaskInput ->
            "#/input"
