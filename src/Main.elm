module Main exposing (Model, Msg(..), RemoteData, Repository, SortOption, Toast, ToastType, User, main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, footer, form, h1, h2, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, for, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import InteropDefinitions as IO
import InteropPorts as IO
import Json.Decode as Decode
import Phosphor as Icon exposing (IconWeight(..))
import Process
import Routes exposing (Route(..))
import Task
import Url



-- Data Types


type RemoteData error data
    = NotAsked
    | Loading
    | Success data
    | Failure error


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


type SortOption
    = SortByStars
    | SortByUpdated
    | SortByName


type alias Toast =
    { id : Int
    , message : String
    , toastType : ToastType
    }


type ToastType
    = ToastSuccess
    | ToastError
    | ToastInfo


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , username : String
    , repositories : RemoteData Http.Error (List Repository)
    , user : RemoteData Http.Error User
    , selectedTopics : List String
    , searchQuery : String
    , topicSearchQuery : String
    , sortBy : SortOption
    , accumulatedRepos : List Repository
    , nextPageUrl : Maybe String
    , theme : String
    , toasts : List Toast
    , nextToastId : Int
    }


type alias MenuItem =
    { label : String
    , route : Route
    , icon : Icon.Icon
    }


type Msg
    = NoOp
    | JSReady
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UsernameChanged String
    | SearchRepositories String
    | UpdateTopicSearch String
    | UserFetched (Result Http.Error User)
    | RepositoriesFetched (Result Http.Error String)
    | ToggleTopic String
    | ClearTopics
    | ChangeSort SortOption
    | NavigateToRepositories String
    | ToggleTheme
    | AddToast String ToastType
    | RemoveToast Int



-- Menu Data


navigationItems : List MenuItem
navigationItems =
    [ { label = "Home", route = Home, icon = Icon.gear }
    ]


userActionItems : List MenuItem
userActionItems =
    [ { label = "Settings", route = Settings, icon = Icon.gear }
    ]



-- HTTP Functions


fetchUser : String -> Cmd Msg
fetchUser username =
    Http.get
        { url = "https://api.github.com/users/" ++ username
        , expect = Http.expectJson UserFetched userDecoder
        }


fetchRepositories : String -> Cmd Msg
fetchRepositories username =
    Http.get
        { url = "https://api.github.com/users/" ++ username ++ "/starred?per_page=100"
        , expect = Http.expectString RepositoriesFetched
        }


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



-- Helper Functions


getAllTopics : List Repository -> List String
getAllTopics repos =
    repos
        |> List.concatMap .topics
        |> unique


getTopicCounts : List Repository -> List String -> List ( String, Int )
getTopicCounts repos topics =
    topics
        |> List.map (\topic -> ( topic, countTopicOccurrences repos topic ))


countTopicOccurrences : List Repository -> String -> Int
countTopicOccurrences repos topic =
    repos
        |> List.filter (\repo -> List.member topic repo.topics)
        |> List.length


filterAndSortRepos : Model -> List Repository -> List Repository
filterAndSortRepos model repos =
    repos
        |> filterByTopics model.selectedTopics
        |> filterBySearch model.searchQuery
        |> sortRepos model.sortBy


filterByTopics : List String -> List Repository -> List Repository
filterByTopics selectedTopics repos =
    if List.isEmpty selectedTopics then
        repos

    else
        repos
            |> List.filter (\repo -> List.any (\topic -> List.member topic repo.topics) selectedTopics)


filterBySearch : String -> List Repository -> List Repository
filterBySearch query repos =
    if String.isEmpty (String.trim query) then
        repos

    else
        let
            lowerQuery =
                String.toLower query
        in
        repos
            |> List.filter
                (\repo ->
                    String.contains lowerQuery (String.toLower repo.name)
                        || (repo.description |> Maybe.map (String.toLower >> String.contains lowerQuery) |> Maybe.withDefault False)
                )


sortRepos : SortOption -> List Repository -> List Repository
sortRepos sortOption repos =
    case sortOption of
        SortByStars ->
            List.sortBy (.stargazersCount >> negate) repos

        SortByUpdated ->
            List.sortBy (.updatedAt >> String.toLower) repos

        SortByName ->
            List.sortBy (.name >> String.toLower) repos


stringToSortOption : String -> SortOption
stringToSortOption str =
    case str of
        "stars" ->
            SortByStars

        "updated" ->
            SortByUpdated

        "name" ->
            SortByName

        _ ->
            SortByStars


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


unique : List a -> List a
unique list =
    uniqueHelper [] list


uniqueHelper : List a -> List a -> List a
uniqueHelper seen remaining =
    case remaining of
        [] ->
            List.reverse seen

        x :: xs ->
            if List.member x seen then
                uniqueHelper seen xs

            else
                uniqueHelper (x :: seen) xs


fuzzyMatch : String -> String -> Bool
fuzzyMatch query target =
    let
        lowerQuery : String
        lowerQuery =
            String.toLower query
    in
    if String.isEmpty (String.trim lowerQuery) then
        True

    else
        let
            lowerTarget : String
            lowerTarget =
                String.toLower target
        in
        fuzzyMatchHelper (String.toList lowerQuery) (String.toList lowerTarget)


fuzzyMatchHelper : List Char -> List Char -> Bool
fuzzyMatchHelper queryChars targetChars =
    case ( queryChars, targetChars ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( q :: qs, t :: ts ) ->
            if q == t then
                fuzzyMatchHelper qs ts

            else
                fuzzyMatchHelper queryChars ts



-- Menu Helpers


viewMenuItem : Route -> MenuItem -> Html Msg
viewMenuItem currentRoute item =
    let
        isActive : Bool
        isActive =
            currentRoute == item.route

        activeClass : String
        activeClass =
            if isActive then
                "menu-active"

            else
                ""
    in
    li [] [ a [ href (Routes.toString item.route), class activeClass ] [ item.icon Regular |> Icon.toHtml [], text item.label ] ]


viewMenuItemTextOnly : Route -> MenuItem -> Html Msg
viewMenuItemTextOnly currentRoute item =
    let
        isActive : Bool
        isActive =
            currentRoute == item.route

        activeClass : String
        activeClass =
            if isActive then
                "menu-active"

            else
                ""
    in
    li [] [ a [ href (Routes.toString item.route), class activeClass ] [ text item.label ] ]


main : Program IO.Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : IO.Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Routes.fromUrl url

        initialData =
            case route of
                Repositories username ->
                    { username = username
                    , user = Loading
                    , repositories = Loading
                    , cmd = Cmd.batch [ fetchUser username, fetchRepositories username ]
                    }

                _ ->
                    { username = ""
                    , user = NotAsked
                    , repositories = NotAsked
                    , cmd = Cmd.none
                    }
    in
    ( { key = key
      , url = url
      , route = route
      , username = initialData.username
      , repositories = initialData.repositories
      , user = initialData.user
      , selectedTopics = []
      , searchQuery = ""
      , topicSearchQuery = ""
      , sortBy = SortByStars
      , accumulatedRepos = []
      , nextPageUrl = Nothing
      , theme = "dark"
      , toasts = []
      , nextToastId = 1
      }
    , Cmd.batch [ IO.fromElm IO.ElmReady, initialData.cmd ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    IO.toElm
        |> Sub.map
            (\result ->
                case result of
                    Ok data ->
                        case data of
                            IO.JSReady ->
                                JSReady

                    Err _ ->
                        NoOp
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        JSReady ->
            let
                cmd : Cmd Msg
                cmd =
                    case model.route of
                        Repositories username ->
                            if model.repositories == NotAsked && model.user == NotAsked then
                                Cmd.batch [ fetchUser username, fetchRepositories username ]

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case Routes.fromUrl url of
                        Repositories username ->
                            ( { model | user = Loading, repositories = Loading }, Cmd.batch [ fetchUser username, fetchRepositories username, Nav.pushUrl model.key (Url.toString url) ] )

                        _ ->
                            ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, route = Routes.fromUrl url }, Cmd.none )

        UsernameChanged username ->
            ( { model | username = username }, Cmd.none )

        SearchRepositories query ->
            ( { model | searchQuery = query }, Cmd.none )

        UpdateTopicSearch query ->
            ( { model | topicSearchQuery = query }, Cmd.none )

        UserFetched result ->
            case result of
                Ok user ->
                    ( { model | user = Success user }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | user = Failure error }
                    , Task.perform (\_ -> AddToast ("Failed to load user: " ++ errorToString error) ToastError) (Task.succeed ())
                    )

        RepositoriesFetched result ->
            case result of
                Ok jsonString ->
                    case Decode.decodeString (Decode.list repositoryDecoder) jsonString of
                        Ok repos ->
                            ( { model | repositories = Success repos }
                            , Cmd.none
                            )

                        Err decodeError ->
                            ( { model | repositories = Failure (Http.BadBody (Decode.errorToString decodeError)) }
                            , Task.perform (\_ -> AddToast "Failed to parse repository data" ToastError) (Task.succeed ())
                            )

                Err error ->
                    ( { model | repositories = Failure error }
                    , Task.perform (\_ -> AddToast ("Failed to load repositories: " ++ errorToString error) ToastError) (Task.succeed ())
                    )

        ToggleTopic topic ->
            let
                newSelectedTopics =
                    if List.member topic model.selectedTopics then
                        List.filter (\t -> t /= topic) model.selectedTopics

                    else
                        topic :: model.selectedTopics
            in
            ( { model | selectedTopics = newSelectedTopics }, Cmd.none )

        ClearTopics ->
            ( { model | selectedTopics = [] }, Cmd.none )

        ChangeSort sortOption ->
            ( { model | sortBy = sortOption }, Cmd.none )

        NavigateToRepositories username ->
            ( { model | user = Loading, repositories = Loading }
            , Cmd.batch [ fetchUser username, fetchRepositories username, Nav.pushUrl model.key (Routes.toString (Repositories username)) ]
            )

        ToggleTheme ->
            let
                newTheme =
                    if model.theme == "light" then
                        "dark"

                    else
                        "light"
            in
            ( { model | theme = newTheme }, Cmd.none )

        AddToast message toastType ->
            let
                newToast =
                    { id = model.nextToastId
                    , message = message
                    , toastType = toastType
                    }
            in
            ( { model
                | toasts = newToast :: model.toasts
                , nextToastId = model.nextToastId + 1
              }
            , Task.perform (\_ -> RemoveToast newToast.id) (Process.sleep 3000)
            )

        RemoveToast id ->
            ( { model | toasts = List.filter (\toast -> toast.id /= id) model.toasts }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Hoshi - GitHub Stars Browser"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "min-h-screen flex flex-col", attribute "data-theme" model.theme ]
        [ viewNavbar model.route
        , div [ class "flex flex-col flex-1" ]
            [ div [ class "drawer lg:hidden" ]
                [ input [ id "mobile-drawer", type_ "checkbox", class "drawer-toggle" ] []
                , viewDrawerSide model.route
                ]
            , div [ class "flex-1 container mx-auto p-4" ]
                [ case model.route of
                    Home ->
                        viewHomePage model

                    Repositories _ ->
                        viewRepositoriesPage model

                    Settings ->
                        viewSettings "Settings"
                ]
            ]
        , viewFooter
        , viewToasts model.toasts
        ]


viewNavbar : Route -> Html Msg
viewNavbar currentRoute =
    div [ class "navbar bg-base-100 border-b border-b-base-300 w-full" ]
        [ div [ class "flex-none lg:hidden" ]
            [ label [ for "mobile-drawer", attribute "aria-label" "open sidebar", class "btn btn-square btn-ghost" ]
                [ Icon.list Regular |> Icon.toHtml []
                ]
            ]
        , div [ class "flex-1 px-2 mx-2" ]
            [ a [ href (Routes.toString Home), class "btn btn-ghost normal-case text-xl font-bold" ]
                [ Icon.shootingStar Regular |> Icon.withClass "w-6 h-6 text-warning" |> Icon.toHtml []
                , text "Hoshi"
                ]
            ]
        , div [ class "flex-none hidden lg:block" ]
            [ ul [ class "menu menu-horizontal px-1" ]
                (List.map (viewMenuItemTextOnly currentRoute) navigationItems)
            ]
        , div [ class "flex-none" ]
            [ label [ class "swap swap-rotate" ]
                [ input [ type_ "checkbox", onClick ToggleTheme ] []
                , Icon.sun Bold |> Icon.withClass "w-6 h-6 swap-on" |> Icon.toHtml []
                , Icon.moon Bold |> Icon.withClass "w-6 h-6 swap-off" |> Icon.toHtml []
                ]
            , div [ class "dropdown dropdown-end" ]
                [ div [ attribute "tabindex" "0", attribute "role" "button", class "btn btn-ghost btn-circle avatar" ]
                    [ div [ class "w-6 rounded-full" ]
                        [ Icon.userCircle Bold |> Icon.withClass "w-6 h-6" |> Icon.toHtml []
                        ]
                    ]
                , ul [ attribute "tabindex" "0", class "menu menu-sm dropdown-content bg-base-100 rounded-box z-[1] mt-3 w-52 p-2 shadow-lg" ]
                    (List.map (viewMenuItemTextOnly currentRoute) userActionItems)
                ]
            ]
        ]


viewDrawerSide : Route -> Html Msg
viewDrawerSide currentRoute =
    div [ class "drawer-side" ]
        [ label [ for "mobile-drawer", attribute "aria-label" "close sidebar", class "drawer-overlay" ] []
        , ul [ class "menu bg-base-200 text-base-content min-h-full w-80 p-4" ]
            (List.concat
                [ [ -- User Info Section
                    li [ class "mb-4" ]
                        [ div [ class "flex items-center gap-3 px-3 py-2" ]
                            [ div [ class "avatar" ]
                                [ div [ class "w-10 rounded-full" ]
                                    [ Icon.userCircle Regular |> Icon.withClass "w-10 h-10" |> Icon.toHtml []
                                    ]
                                ]
                            , div []
                                [ div [ class "font-semibold" ] [ text "GitHub Stars" ]
                                , div [ class "text-sm opacity-70" ] [ text "Explore repositories" ]
                                ]
                            ]
                        ]
                  , li [ class "menu-title" ] [ text "Navigation" ]
                  ]
                , List.map (viewMenuItem currentRoute) navigationItems
                , [ li [ class "menu-title" ] [ text "Settings" ]
                  , li []
                        [ button [ onClick ToggleTheme, class "btn btn-ghost w-full justify-start" ]
                            [ Icon.palette Regular |> Icon.toHtml []
                            , text "Toggle Theme"
                            ]
                        ]
                  , li [ class "menu-title" ] [ text "Account" ]
                  ]
                , List.map (viewMenuItem currentRoute) userActionItems
                ]
            )
        ]


viewHomePage : Model -> Html Msg
viewHomePage model =
    div [ class "flex flex-col items-center justify-center" ]
        [ form [ onSubmit (NavigateToRepositories model.username) ]
            [ div [ class "input input-xl" ]
                [ Icon.magnifyingGlass Bold |> Icon.toHtml []
                , input
                    [ type_ "text"
                    , placeholder "GitHub username"
                    , value model.username
                    , onInput UsernameChanged
                    ]
                    []
                ]
            ]
        , div [ class "divider" ] [ text "Popular Users" ]
        , div [ class "flex flex-wrap gap-2 justify-center" ]
            [ a [ href <| Routes.toString <| Routes.Repositories "torvalds", class "btn btn-primary btn-outline btn-sm" ] [ text "torvalds" ]
            , a [ href <| Routes.toString <| Routes.Repositories "gaearon", class "btn btn-primary btn-outline btn-sm" ] [ text "gaearon" ]
            , a [ href <| Routes.toString <| Routes.Repositories "tj", class "btn btn-primary btn-outline btn-sm" ] [ text "tj" ]
            , a [ href <| Routes.toString <| Routes.Repositories "sindresorhus", class "btn btn-primary btn-outline btn-sm" ] [ text "sindresorhus" ]
            ]
        ]


viewRepositoriesPage : Model -> Html Msg
viewRepositoriesPage model =
    div [ class "flex flex-col gap-12 lg:flex-row min-h-screen" ]
        [ viewSidebar model
        , div [ class "flex-1 flex flex-col gap-4" ]
            [ viewHeader model
            , viewContent model
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "w-full lg:w-80 lg:min-h-screen lg:sticky lg:top-20 flex flex-col gap-4" ]
        [ label [ class "input w-full" ]
            [ Icon.magnifyingGlass Bold |> Icon.toHtml []
            , input
                [ type_ "text"
                , placeholder "Filter topics"
                , value model.topicSearchQuery
                , onInput UpdateTopicSearch
                ]
                []
            ]
        , div [ class "space-y-2 max-h-96 overflow-y-auto" ]
            (case model.repositories of
                Loading ->
                    List.repeat 8 viewTopicSkeleton

                Success repos ->
                    let
                        allTopics =
                            getAllTopics repos

                        topicCounts =
                            getTopicCounts repos allTopics

                        filteredTopicCounts =
                            if String.isEmpty (String.trim model.topicSearchQuery) then
                                topicCounts

                            else
                                List.filter (\( topic, _ ) -> fuzzyMatch model.topicSearchQuery topic) topicCounts
                    in
                    List.map (viewTopicFilter model.selectedTopics) (List.sortBy (Tuple.second >> negate) filteredTopicCounts)

                _ ->
                    List.repeat 8 viewTopicSkeleton
            )
        , button
            [ onClick ClearTopics
            , class "btn btn-outline btn-primary"
            , disabled (List.length model.selectedTopics == 0)
            ]
            [ text "Clear" ]
        ]


viewTopicFilter : List String -> ( String, Int ) -> Html Msg
viewTopicFilter selectedTopics ( topic, count ) =
    let
        isSelected =
            List.member topic selectedTopics

        buttonClass =
            if isSelected then
                "btn-primary"

            else
                "btn-ghost"
    in
    button
        [ onClick (ToggleTopic topic)
        , class ("btn btn-sm w-full justify-start " ++ buttonClass)
        ]
        [ text topic
        , div [ class "badge badge-xs ml-auto" ] [ text (String.fromInt count) ]
        ]


viewTopicSkeleton : Html Msg
viewTopicSkeleton =
    div [ class "flex-1 h-6 bg-base-300 rounded animate-pulse mb-3" ] []


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex flex-col" ]
        [ div [ class "breadcrumbs mb-4" ]
            [ ul []
                [ li [] [ a [ href (Routes.toString Home) ] [ Icon.house Regular |> Icon.toHtml [], text "Home" ] ]
                , li [] [ text "Repositories" ]
                , li [] [ text model.username ]
                ]
            ]
        , div [ class "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4" ]
            [ div [ class "flex items-center gap-4" ]
                [ case model.user of
                    Success user ->
                        div [ class "flex items-center gap-4" ]
                            [ Html.img [ Html.Attributes.src user.avatarUrl, class "w-14 h-14 rounded-full ring ring-primary ring-offset-base-100 ring-offset-2" ] []
                            , div []
                                [ div [ class "font-semibold text-lg" ] [ text (Maybe.withDefault user.login user.name) ]
                                , div [ class "text-sm text-secondary" ] [ text ("@" ++ user.login) ]
                                , case user.bio of
                                    Just bio ->
                                        div [ class "text-sm text-secondary max-w-32 truncate", title bio ] [ text bio ]

                                    Nothing ->
                                        text ""
                                ]
                            ]

                    Loading ->
                        div [ class "flex items-center gap-4" ]
                            [ div [ class "w-14 h-14 rounded-full bg-base-300 animate-pulse" ] []
                            , div [ class "flex flex-col gap-2" ]
                                [ div [ class "h-5 bg-base-300 rounded animate-pulse w-32" ] []
                                , div [ class "h-4 bg-base-300 rounded animate-pulse w-20" ] []
                                , div [ class "h-4 bg-base-300 rounded animate-pulse w-64" ] []
                                ]
                            ]

                    _ ->
                        div [ class "flex items-center gap-4" ]
                            [ div [ class "w-14 h-14 rounded-full bg-base-300 animate-pulse" ] []
                            , div [ class "flex flex-col gap-2" ]
                                [ div [ class "h-5 bg-base-300 rounded animate-pulse w-32" ] []
                                , div [ class "h-4 bg-base-300 rounded animate-pulse w-20" ] []
                                , div [ class "h-4 bg-base-300 rounded animate-pulse w-64" ] []
                                ]
                            ]
                ]
            , div [ class "flex flex-col sm:flex-row gap-4" ]
                [ label [ class "input w-full" ]
                    [ Icon.magnifyingGlass Bold |> Icon.toHtml []
                    , input
                        [ type_ "text"
                        , placeholder "Filter repositories"
                        , value model.searchQuery
                        , onInput SearchRepositories
                        ]
                        []
                    ]
                , label [ class "select w-full" ]
                    [ span [ class "label" ] [ Icon.sortAscending Bold |> Icon.toHtml [], text "Sort by" ]
                    , select
                        [ onInput (\value -> ChangeSort (stringToSortOption value))
                        ]
                        [ option [ value "stars", Html.Attributes.selected (model.sortBy == SortByStars) ] [ text "Stars" ]
                        , option [ value "updated", Html.Attributes.selected (model.sortBy == SortByUpdated) ] [ text "Updated" ]
                        , option [ value "name", Html.Attributes.selected (model.sortBy == SortByName) ] [ text "Name" ]
                        ]
                    ]
                ]
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    div [ class "flex-1" ]
        [ case model.repositories of
            NotAsked ->
                div [ class "text-center py-12" ]
                    [ Icon.star Regular |> Icon.withClass "w-16 h-16 mx-auto mb-4 text-base-content/50" |> Icon.toHtml []
                    , text "Enter a username to view starred repositories"
                    ]

            Loading ->
                div [ class "flex flex-col gap-4" ]
                    [ div [ class "stats stats-vertical lg:stats-horizontal border border-base-300" ]
                        [ div [ class "stat" ]
                            [ div [ class "stat-title" ] [ text "Total Repositories" ]
                            , div [ class "stat-value" ]
                                [ div [ class "loading loading-infinity" ] [] ]
                            , div [ class "stat-desc" ] [ text "Loading..." ]
                            ]
                        , div [ class "stat" ]
                            [ div [ class "stat-title" ] [ text "Filtered Results" ]
                            , div [ class "stat-value" ]
                                [ div [ class "loading loading-infinity" ] [] ]
                            , div [ class "stat-desc" ] [ text "Loading..." ]
                            ]
                        ]
                    , div [ class "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4" ]
                        (List.repeat 6 viewRepositorySkeleton)
                    ]

            Failure error ->
                div [ class "alert alert-error alert-soft" ]
                    [ Icon.warning Regular |> Icon.toHtml []
                    , div []
                        [ div [ class "font-semibold" ] [ text "Error loading repositories" ]
                        , div [] [ text (errorToString error) ]
                        ]
                    ]

            Success repos ->
                let
                    filteredRepos =
                        filterAndSortRepos model repos

                    totalCount =
                        List.length repos

                    filteredCount =
                        List.length filteredRepos
                in
                div [ class "flex flex-col gap-4" ]
                    [ div [ class "stats stats-vertical lg:stats-horizontal border border-base-300" ]
                        [ div [ class "stat" ]
                            [ div [ class "stat-title" ] [ text "Total Repositories" ]
                            , div [ class "stat-value" ] [ text (String.fromInt totalCount) ]
                            , div [ class "stat-desc" ] [ text "Starred by user" ]
                            ]
                        , div [ class "stat" ]
                            [ div [ class "stat-title" ] [ text "Filtered Results" ]
                            , div [ class "stat-value" ] [ text (String.fromInt filteredCount) ]
                            , div [ class "stat-desc" ]
                                [ text
                                    (if totalCount == filteredCount then
                                        "No filters applied"

                                     else
                                        String.fromInt (totalCount - filteredCount) ++ " hidden"
                                    )
                                ]
                            ]
                        , if not (List.isEmpty model.selectedTopics) then
                            div [ class "stat" ]
                                [ div [ class "stat-title" ] [ text "Active Filters" ]
                                , div [ class "stat-value" ] [ text (String.fromInt (List.length model.selectedTopics)) ]
                                , div [ class "stat-desc" ] [ text "Topics selected" ]
                                ]

                          else
                            text ""
                        ]
                    , div [ class "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4" ]
                        (List.map viewRepositoryCard filteredRepos)
                    ]
        ]


viewRepositoryCard : Repository -> Html Msg
viewRepositoryCard repo =
    div [ class "card bg-base-100 transition-all duration-300 hover:scale-105 border border-base-300" ]
        [ div [ class "card-body" ]
            [ div [ class "flex items-start justify-between mb-2" ]
                [ div [ class "card-title text-lg" ]
                    [ Html.a
                        [ Html.Attributes.href repo.htmlUrl
                        , Html.Attributes.target "_blank"
                        , Html.Attributes.rel "noopener noreferrer"
                        , class "link link-primary hover:link-accent"
                        ]
                        [ text repo.name ]
                    ]
                , div [ class "flex items-center gap-1 text-sm text-base-content/60" ]
                    [ Icon.star Regular |> Icon.withClass "w-4 h-4 text-warning" |> Icon.toHtml []
                    , text (String.fromInt repo.stargazersCount)
                    ]
                ]
            , case repo.description of
                Just desc ->
                    p [ class "text-sm text-base-content/80 mb-4 line-clamp-2" ] [ text desc ]

                Nothing ->
                    text "No description available."
            , div [ class "flex items-center justify-between" ]
                [ case repo.language of
                    Just lang ->
                        div [ class "flex items-center gap-2 text-secondary" ]
                            [ Icon.code Bold |> Icon.toHtml []
                            , span [ class "text-sm" ] [ text lang ]
                            ]

                    Nothing ->
                        div [ class "flex items-center gap-2 text-secondary" ]
                            [ Icon.code Bold |> Icon.toHtml []
                            , span [ class "text-sm" ] [ text "Unknown" ]
                            ]
                ]
            ]
        ]


viewRepositorySkeleton : Html Msg
viewRepositorySkeleton =
    div [ class "card bg-base-100 shadow-xl animate-pulse border border-base-300" ]
        [ div [ class "card-body" ]
            [ div [ class "flex items-start justify-between mb-2" ]
                [ div [ class "h-6 bg-base-300 rounded w-3/4" ] []
                , div [ class "flex items-center gap-1" ]
                    [ Icon.star Regular |> Icon.withClass "w-4 h-4 text-secondary" |> Icon.toHtml []
                    , div [ class "h-4 bg-base-300 rounded w-8" ] []
                    ]
                ]
            , div [ class "space-y-2 mb-4" ]
                [ div [ class "h-4 bg-base-300 rounded w-full" ] []
                , div [ class "h-4 bg-base-300 rounded w-2/3" ] []
                ]
            , div [ class "flex items-center gap-2" ]
                [ Icon.code Bold |> Icon.withClass "text-secondary" |> Icon.toHtml []
                , div [ class "h-4 bg-base-300 rounded w-12" ] []
                ]
            ]
        ]


viewSettings : String -> Html Msg
viewSettings title =
    div [ class "flex flex-col gap-6" ]
        [ div [ class "card bg-base-100 shadow-xl" ]
            [ div [ class "card-body" ]
                [ div [ class "card-title" ] [ Icon.gear Regular |> Icon.toHtml [], text title ]
                , div [] [ text "Manage your application settings" ]
                , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4 mt-4" ]
                    [ div [ class "card bg-base-200" ]
                        [ div [ class "card-body" ]
                            [ div [ class "card-title text-sm" ] [ text "Notifications" ]
                            , div [ class "form-control" ]
                                [ label [ class "label cursor-pointer" ]
                                    [ text "Email notifications"
                                    , input [ type_ "checkbox", class "toggle toggle-primary" ] []
                                    ]
                                ]
                            ]
                        ]
                    , div [ class "card bg-base-200" ]
                        [ div [ class "card-body" ]
                            [ div [ class "card-title text-sm" ] [ text "Theme" ]
                            , div [ class "form-control" ]
                                [ label [ class "label cursor-pointer" ]
                                    [ text "Dark mode"
                                    , input [ type_ "checkbox", class "toggle toggle-primary" ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "card-actions justify-end" ]
                    [ button [ class "btn btn-ghost" ] [ text "Reset" ]
                    , button [ class "btn btn-primary" ] [ text "Save Changes" ]
                    ]
                ]
            ]
        ]


viewFooter : Html Msg
viewFooter =
    footer [ class "footer footer-center text-base-content p-10" ]
        [ div [ class "grid grid-flow-col gap-4" ]
            [ a [ href "https://github.com", Html.Attributes.target "_blank", Html.Attributes.rel "noopener noreferrer" ]
                [ Icon.githubLogo Regular |> Icon.withClass "w-6 h-6" |> Icon.toHtml [] ]
            ]
        ]


viewToasts : List Toast -> Html Msg
viewToasts toasts =
    div [ class "toast toast-bottom toast-end z-[100]" ]
        (List.map viewToast toasts)


viewToast : Toast -> Html Msg
viewToast toast =
    div [ class ("alert alert-soft " ++ toastTypeToClass toast.toastType) ]
        [ case toast.toastType of
            ToastSuccess ->
                Icon.checkCircle Regular |> Icon.toHtml []

            ToastError ->
                Icon.warning Regular |> Icon.toHtml []

            ToastInfo ->
                Icon.info Regular |> Icon.toHtml []
        , span [] [ text toast.message ]
        , button
            [ onClick (RemoveToast toast.id)
            , class "btn btn-sm btn-circle btn-ghost"
            , attribute "aria-label" "Close toast"
            ]
            [ text "✕" ]
        ]


toastTypeToClass : ToastType -> String
toastTypeToClass toastType =
    case toastType of
        ToastSuccess ->
            "alert-success"

        ToastError ->
            "alert-error"

        ToastInfo ->
            "alert-info"
