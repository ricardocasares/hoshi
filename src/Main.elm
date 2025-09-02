module Main exposing (Model, Msg(..), RemoteData, SortOption, Theme, main)

import Browser
import Browser.Navigation as Nav
import GitHub.API as GH
import Html exposing (Html, a, button, div, footer, form, input, label, li, option, p, select, span, text, ul)
import Html.Attributes exposing (attribute, class, disabled, for, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import InteropDefinitions as IO
import InteropPorts as IO
import Phosphor as Icon exposing (IconWeight(..))
import Process
import Routes exposing (Route(..))
import Task
import UI.Alert as Alert
import UI.Card as Card
import UI.Toast as Toast
import Url



-- Data Types


type RemoteData error data
    = NotAsked
    | Loading
    | Success data
    | Failure error


type SortOption
    = SortByStars
    | SortByUpdated
    | SortByName


type Theme
    = Dark
    | Light



-- Toast and ToastType are now imported from Toast module


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , username : String
    , repositories : RemoteData Http.Error (List GH.Repository)
    , user : RemoteData Http.Error GH.User
    , selectedTopics : List String
    , searchQuery : String
    , topicSearchQuery : String
    , sortBy : SortOption
    , accumulatedRepos : List GH.Repository
    , nextPageUrl : Maybe String
    , theme : Theme
    , toasts : List (Toast.Toast Msg)
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
    | UserFetched (Result Http.Error GH.User)
    | RepositoriesFetched (Result Http.Error (List GH.Repository))
    | ToggleTopic String
    | ClearTopics
    | ChangeSort SortOption
    | NavigateToRepositories String
    | ToggleTheme
    | AddToast String Toast.ToastType
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
-- Helper Functions


stringToTheme : String -> Theme
stringToTheme str =
    case str of
        "dark" ->
            Dark

        _ ->
            Light


themeToString : Theme -> String
themeToString theme =
    case theme of
        Dark ->
            "dark"

        Light ->
            "light"


toggleTheme : Theme -> Theme
toggleTheme theme =
    case theme of
        Dark ->
            Light

        Light ->
            Dark


getAllTopics : List GH.Repository -> List String
getAllTopics repos =
    repos
        |> List.concatMap .topics
        |> unique


getTopicCounts : List GH.Repository -> List String -> List ( String, Int )
getTopicCounts repos topics =
    topics
        |> List.map (\topic -> ( topic, countTopicOccurrences repos topic ))


countTopicOccurrences : List GH.Repository -> String -> Int
countTopicOccurrences repos topic =
    repos
        |> List.filter (\repo -> List.member topic repo.topics)
        |> List.length


filterAndSortRepos : Model -> List GH.Repository -> List GH.Repository
filterAndSortRepos model repos =
    repos
        |> filterByTopics model.selectedTopics
        |> filterBySearch model.searchQuery
        |> sortRepos model.sortBy


filterByTopics : List String -> List GH.Repository -> List GH.Repository
filterByTopics selectedTopics repos =
    if List.isEmpty selectedTopics then
        repos

    else
        repos
            |> List.filter (\repo -> List.any (\topic -> List.member topic repo.topics) selectedTopics)


filterBySearch : String -> List GH.Repository -> List GH.Repository
filterBySearch query repos =
    if String.isEmpty (String.trim query) then
        repos

    else
        let
            lowerQuery : String
            lowerQuery =
                String.toLower query
        in
        repos
            |> List.filter
                (\repo ->
                    String.contains lowerQuery (String.toLower repo.name)
                        || (repo.description |> Maybe.map (String.toLower >> String.contains lowerQuery) |> Maybe.withDefault False)
                )


sortRepos : SortOption -> List GH.Repository -> List GH.Repository
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
init flags url key =
    let
        route : Route
        route =
            Routes.fromUrl url

        initialData : { username : String, user : RemoteData Http.Error GH.User, repositories : RemoteData Http.Error (List GH.Repository), cmd : Cmd Msg }
        initialData =
            case route of
                Repositories username ->
                    { username = username
                    , user = Loading
                    , repositories = Loading
                    , cmd = Cmd.batch [ Task.attempt UserFetched (GH.fetchUser username), Task.attempt RepositoriesFetched (GH.fetchRepositories username) ]
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
      , theme = stringToTheme flags.theme
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
                                Cmd.batch [ Task.attempt UserFetched (GH.fetchUser username), Task.attempt RepositoriesFetched (GH.fetchRepositories username) ]

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
                            ( { model
                                | user = Loading
                                , repositories = Loading
                                , selectedTopics = []
                                , topicSearchQuery = ""
                                , searchQuery = ""
                                , sortBy = SortByStars
                              }
                            , Cmd.batch [ Task.attempt UserFetched (GH.fetchUser username), Task.attempt RepositoriesFetched (GH.fetchRepositories username), Nav.pushUrl model.key (Url.toString url) ]
                            )

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
                    , Task.perform (\_ -> AddToast ("Failed to load user: " ++ GH.errorToString error) Toast.Error) (Task.succeed ())
                    )

        RepositoriesFetched result ->
            case result of
                Ok repos ->
                    ( { model | repositories = Success repos }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | repositories = Failure error }
                    , Task.perform (\_ -> AddToast ("Failed to load repositories: " ++ GH.errorToString error) Toast.Error) (Task.succeed ())
                    )

        ToggleTopic topic ->
            let
                newSelectedTopics : List String
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
            , Cmd.batch [ Task.attempt UserFetched (GH.fetchUser username), Task.attempt RepositoriesFetched (GH.fetchRepositories username), Nav.pushUrl model.key (Routes.toString (Repositories username)) ]
            )

        ToggleTheme ->
            let
                newTheme : Theme
                newTheme =
                    toggleTheme model.theme
            in
            ( { model | theme = newTheme }, IO.fromElm (IO.SaveTheme (themeToString newTheme)) )

        AddToast message toastType ->
            let
                newToast : Toast.Toast Msg
                newToast =
                    { id = model.nextToastId
                    , message = message
                    , toastType = toastType
                    , onClose = RemoveToast
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
    div [ class "min-h-screen flex flex-col", attribute "data-theme" (themeToString model.theme) ]
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
            [ a [ href (Routes.toString Home), class "btn btn-ghost normal-case text-xl font-normal" ]
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
                , Icon.moon Bold |> Icon.withClass "w-6 h-6 swap-on" |> Icon.toHtml []
                , Icon.sun Bold |> Icon.withClass "w-6 h-6 swap-off" |> Icon.toHtml []
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
        [ div [ class "flex-1 flex flex-col gap-4" ]
            [ viewHeader model
            , viewContent model
            ]
        ]


viewTopicFilter : List String -> ( String, Int ) -> Html Msg
viewTopicFilter selectedTopics ( topic, count ) =
    let
        isSelected : Bool
        isSelected =
            List.member topic selectedTopics

        buttonClass : String
        buttonClass =
            if isSelected then
                "menu-active"

            else
                ""
    in
    li
        [ onClick (ToggleTopic topic)
        , class "w-full"
        ]
        [ span [ class buttonClass ] [ text topic, div [ class "badge badge-sm" ] [ text (String.fromInt count) ] ]
        ]


viewTopicSkeleton : Html Msg
viewTopicSkeleton =
    li [ class "flex-1 bg-base-300 text-base-300 rounded animate-pulse mb-3" ] [ text "." ]


viewUserSkeleton : Html Msg
viewUserSkeleton =
    div [ class "flex items-center gap-4" ]
        [ div [ class "w-14 h-14 rounded-full bg-base-300 animate-pulse" ] []
        , div [ class "flex flex-col gap-2" ]
            [ div [ class "h-5 bg-base-300 rounded animate-pulse w-32" ] []
            , div [ class "h-4 bg-base-300 rounded animate-pulse w-20" ] []
            , div [ class "h-4 bg-base-300 rounded animate-pulse w-64" ] []
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "flex flex-col" ]
        [ div [ class "breadcrumbs mb-4" ]
            [ ul []
                [ li [] [ a [ href (Routes.toString Home) ] [ Icon.house Regular |> Icon.toHtml [], text "Home" ] ]
                , li [] [ text "Repositories" ]
                , li []
                    [ case model.user of
                        NotAsked ->
                            text ""

                        Loading ->
                            span [ class "h-4 w-20 animate animate-pulse bg-base-300 rounded" ] []

                        Success user ->
                            text user.login

                        Failure _ ->
                            span [ class "h-4 w-20 animate animate-pulse bg-base-300 rounded" ] []
                    ]
                ]
            ]
        , div [ class "flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4" ]
            [ div [ class "flex items-center gap-4" ]
                [ case model.user of
                    Success user ->
                        div [ class "flex items-center gap-4" ]
                            [ Html.img [ Html.Attributes.src user.avatarUrl, class "w-14 h-14 rounded-full ring ring-primary ring-offset-base-100 ring-offset-2" ] []
                            , div []
                                [ div [ class "font-semibold text-lg max-w-32 md:max-w-64 truncate" ] [ text (Maybe.withDefault user.login user.name) ]
                                , div [ class "text-sm text-secondary" ] [ text ("@" ++ user.login) ]
                                , case user.bio of
                                    Just bio ->
                                        div [ class "text-sm text-secondary max-w-32 truncate", title bio ] [ text bio ]

                                    Nothing ->
                                        text ""
                                ]
                            ]

                    Loading ->
                        viewUserSkeleton

                    _ ->
                        viewUserSkeleton
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
                , label [ class "input w-full font-normal sticky top-2 cursor-pointer", for "topics-modal" ]
                    [ span [ class "label" ]
                        [ Icon.funnelSimple Bold |> Icon.toHtml []
                        , text "Selected topics"
                        ]
                    , span [ class "text-right w-full" ]
                        [ if List.isEmpty model.selectedTopics then
                            text "0"

                          else
                            text <| String.fromInt <| List.length model.selectedTopics
                        ]
                    ]
                , input [ type_ "checkbox", class "modal-toggle", id "topics-modal" ] []
                , div [ class "modal modal-bottom sm:modal-middle", attribute "role" "dialog" ]
                    [ div [ class "modal-box flex flex-col gap-2" ]
                        [ ul [ class "menu flex-nowrap w-full p-0 gap-1 h-64 overflow-scroll" ]
                            (case model.repositories of
                                Loading ->
                                    List.repeat 8 viewTopicSkeleton

                                Success repos ->
                                    let
                                        allTopics : List String
                                        allTopics =
                                            getAllTopics repos

                                        topicCounts : List ( String, Int )
                                        topicCounts =
                                            getTopicCounts repos allTopics

                                        filteredTopicCounts : List ( String, Int )
                                        filteredTopicCounts =
                                            if String.isEmpty (String.trim model.topicSearchQuery) then
                                                topicCounts

                                            else
                                                List.filter (\( topic, _ ) -> fuzzyMatch model.topicSearchQuery topic) topicCounts
                                    in
                                    case filteredTopicCounts of
                                        [] ->
                                            [ div [ class "h-64 flex items-center justify-center" ] [ Icon.magnifyingGlass Bold |> Icon.withClass "h-12 w-12 text-neutral" |> Icon.toHtml [] ] ]

                                        list ->
                                            List.map (viewTopicFilter model.selectedTopics) (List.sortBy (Tuple.second >> negate) list)

                                _ ->
                                    List.repeat 8 viewTopicSkeleton
                            )
                        , div [ class "flex gap-2" ]
                            [ label [ class "input w-full" ]
                                [ Icon.magnifyingGlass Bold |> Icon.toHtml []
                                , input
                                    [ type_ "search"
                                    , placeholder "Filter topics"
                                    , value model.topicSearchQuery
                                    , onInput UpdateTopicSearch
                                    ]
                                    []
                                ]
                            , label
                                [ onClick ClearTopics
                                , class "btn btn-error btn-soft"
                                , for "topics-modal"
                                , disabled (List.isEmpty model.selectedTopics)
                                ]
                                [ text <| "Clear"
                                ]
                            ]
                        ]
                    , label [ class "modal-backdrop", for "topics-modal" ] [ text "Close" ]
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
                    [ div [ class "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4" ]
                        (List.repeat 6 viewRepositorySkeleton)
                    ]

            Failure error ->
                Alert.error [ class "alert-soft" ]
                    [ Icon.warning Regular |> Icon.toHtml []
                    , div []
                        [ div [ class "font-semibold" ] [ text "Error loading repositories" ]
                        , div [] [ text (GH.errorToString error) ]
                        ]
                    ]

            Success repos ->
                let
                    filtered : List GH.Repository
                    filtered =
                        filterAndSortRepos model repos
                in
                div [ class "flex flex-col gap-4" ]
                    [ div [ class "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4" ]
                        (List.map viewRepositoryCard filtered)
                    ]
        ]


viewRepositoryCard : GH.Repository -> Html Msg
viewRepositoryCard repo =
    Card.card [ class "border border-base-300" ]
        [ Card.body []
            [ div [ class "flex items-start justify-between mb-2" ]
                [ Card.title [ class "text-lg" ]
                    [ Html.a
                        [ Html.Attributes.href repo.htmlUrl
                        , Html.Attributes.target "_blank"
                        , Html.Attributes.rel "noopener noreferrer"
                        , class "link link-primary hover:link-accent"
                        ]
                        [ text repo.name ]
                    ]
                , div [ class "flex items-center gap-1 text-sm text-base-content/60" ]
                    [ Icon.star Regular |> Icon.withClass "text-warning" |> Icon.toHtml []
                    , text (String.fromInt repo.stargazersCount)
                    ]
                ]
            , p [ class "text-sm text-base-content/80 mb-4 line-clamp-2" ]
                [ case repo.description of
                    Just desc ->
                        text desc

                    Nothing ->
                        text "No description available."
                ]
            , div [ class "flex items-center gap-2 text-secondary" ]
                [ Icon.code Bold |> Icon.toHtml []
                , span [ class "font-mono text-xs" ]
                    [ case repo.language of
                        Just lang ->
                            text lang

                        Nothing ->
                            text "Unknown"
                    ]
                ]
            ]
        ]


viewRepositorySkeleton : Html Msg
viewRepositorySkeleton =
    Card.card [ class "bg-base-100 animate-pulse border border-base-300" ]
        [ Card.body []
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
        [ Card.card [ class "bg-base-100 shadow-xl" ]
            [ Card.body []
                [ Card.title [] [ Icon.gear Regular |> Icon.toHtml [], text title ]
                , div [] [ text "Manage your application settings" ]
                , div [ class "grid grid-cols-1 md:grid-cols-2 gap-4 mt-4" ]
                    [ Card.card [ class "bg-base-200" ]
                        [ Card.body []
                            [ Card.title [ class "text-sm" ] [ text "Notifications" ]
                            , div [ class "form-control" ]
                                [ label [ class "label cursor-pointer" ]
                                    [ text "Email notifications"
                                    , input [ type_ "checkbox", class "toggle toggle-primary" ] []
                                    ]
                                ]
                            ]
                        ]
                    , Card.card [ class "bg-base-200" ]
                        [ Card.body []
                            [ Card.title [ class "text-sm" ] [ text "Theme" ]
                            , div [ class "form-control" ]
                                [ label [ class "label cursor-pointer" ]
                                    [ text "Dark mode"
                                    , input [ type_ "checkbox", class "toggle toggle-primary" ] []
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Card.actions [ class "justify-end" ]
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


viewToasts : List (Toast.Toast Msg) -> Html Msg
viewToasts toasts =
    Toast.container [ class "toast-bottom toast-end" ]
        (List.map viewToast toasts)


viewToast : Toast.Toast Msg -> Html Msg
viewToast toast =
    Toast.toast toast
