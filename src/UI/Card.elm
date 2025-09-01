module UI.Card exposing (card, body, title, actions)

{-| A composable Card component based on daisyUI's card component.

This module provides functions to create card elements that can be easily composed
and extended with additional attributes and classes.

@docs card, body, title, actions

-}

import Html exposing (Html, div, h2)
import Html.Attributes exposing (class)


{-| The main card container.

Use this as the root element for your card. You can add modifiers like
`card-side`, `image-full`, or size classes like `card-lg` using the attrs parameter.

    card [ class "card-side" ]
        [ figure [] [ img [ src "image.jpg" ] [] ]
        , body []
            [ title [] [ text "Card Title" ]
            , p [] [ text "Card content" ]
            , actions [] [ button [] [ text "Action" ] ]
            ]
        ]

-}
card : List (Html.Attribute msg) -> List (Html msg) -> Html msg
card attrs children =
    div (class "card" :: attrs) children


{-| The card body container.

Contains the main content of the card including title, text, and actions.

-}
body : List (Html.Attribute msg) -> List (Html msg) -> Html msg
body attrs children =
    div (class "card-body" :: attrs) children


{-| The card title element.

Use this for the main heading of your card.

-}
title : List (Html.Attribute msg) -> List (Html msg) -> Html msg
title attrs children =
    h2 (class "card-title" :: attrs) children


{-| The card actions container.

Contains buttons or other interactive elements for the card.

-}
actions : List (Html.Attribute msg) -> List (Html msg) -> Html msg
actions attrs children =
    div (class "card-actions" :: attrs) children
