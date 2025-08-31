module Routes exposing (Route(..), fromUrl, toString)

import Url
import Url.Parser as Parser exposing (Parser, map, oneOf, s, string, top, (</>))


type Route
    = Home
    | Repositories String
    | Settings


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Repositories (s "repositories" </> string)
        , map Settings (s "settings")
        ]


fromUrl : Url.Url -> Route
fromUrl url =
    case Parser.parse parser url of
        Just route ->
            route

        Nothing ->
            Home


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Repositories username ->
            "/repositories/" ++ username

        Settings ->
            "/settings"
