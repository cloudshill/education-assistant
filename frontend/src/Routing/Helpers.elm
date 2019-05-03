module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = NotFound
    | TaskInput
    | Grading


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
        [ Url.Parser.map TaskInput Url.Parser.top
        , Url.Parser.map Grading (Url.Parser.s "grading")
        ]


reverseRoute : Route -> String
reverseRoute route =
    case route of
        Grading ->
            "#/grading"

        _ ->
            "#/"
