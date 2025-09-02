module GitHub.API exposing (Repository, User, errorToString, fetchRepositories, fetchUser)

import Http
import Json.Decode as Decode
import Task exposing (Task)



-- Data Types


type alias Repository =
    { id : Int
    , name : String
    , description : Maybe String
    , htmlUrl : String
    , language : Maybe String
    , stargazersCount : Int
    , topics : List String
    , updatedAt : String
    }


type alias User =
    { login : String
    , name : Maybe String
    , avatarUrl : String
    , bio : Maybe String
    }



-- Decoders


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "login" Decode.string)
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.field "avatar_url" Decode.string)
        (Decode.maybe (Decode.field "bio" Decode.string))


repositoryDecoder : Decode.Decoder Repository
repositoryDecoder =
    Decode.map8 Repository
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.field "description" Decode.string))
        (Decode.field "html_url" Decode.string)
        (Decode.maybe (Decode.field "language" Decode.string))
        (Decode.field "stargazers_count" Decode.int)
        (Decode.field "topics" (Decode.list Decode.string))
        (Decode.field "updated_at" Decode.string)



-- HTTP Functions


fetchUser : String -> Task Http.Error User
fetchUser username =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://api.github.com/users/" ++ username
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse userDecoder
        , timeout = Nothing
        }


fetchRepositories : String -> Task Http.Error (List Repository)
fetchRepositories username =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://api.github.com/users/" ++ username ++ "/starred?per_page=100"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse (Decode.list repositoryDecoder)
        , timeout = Nothing
        }


handleJsonResponse : Decode.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))



-- Error Handling


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body
