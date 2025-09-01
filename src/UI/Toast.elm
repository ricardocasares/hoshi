module UI.Toast exposing (Toast, ToastType(..), toast, container)

{-| A composable Toast notification component based on daisyUI's toast and alert components.

This module provides functions to create toast notifications that can be easily composed
and extended with additional attributes and classes.

@docs Toast, ToastType, toast, container

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onClick)
import Phosphor as Icon exposing (IconWeight(..))


{-| Represents a toast notification.
-}
type alias Toast msg =
    { id : Int
    , message : String
    , toastType : ToastType
    , onClose : Int -> msg
    }


{-| The type of toast notification.
-}
type ToastType
    = Error


{-| The main toast container for positioning multiple toasts.

Use this as the root element for your toast notifications. You can add positioning
classes like `toast-top`, `toast-bottom`, `toast-end`, etc. using the attrs parameter.

    container [ class "toast-bottom toast-end" ]
        [ toast model.toast1
        , toast model.toast2
        ]

-}
container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
container attrs children =
    div (class "toast z-[100]" :: attrs) children


{-| Create a toast notification element.

This renders a single toast with the appropriate styling based on its type.

    toast
        { id = 1
        , message = "Operation completed successfully"
        , toastType = Success
        , onClose = RemoveToast
        }

-}
toast : Toast msg -> Html msg
toast toastData =
    div [ class ("alert alert-soft " ++ toastTypeToClass toastData.toastType) ]
        [ toastIcon toastData.toastType
        , span [] [ text toastData.message ]
        , button
            [ onClick (toastData.onClose toastData.id)
            , class "btn btn-sm btn-circle btn-ghost"
            , attribute "aria-label" "Close toast"
            ]
            [ text "✕" ]
        ]


{-| Convert a ToastType to the corresponding daisyUI alert class.
-}
toastTypeToClass : ToastType -> String
toastTypeToClass toastType =
    case toastType of
        Error ->
            "alert-error"


{-| Get the appropriate icon for a toast type.
-}
toastIcon : ToastType -> Html msg
toastIcon toastType =
    case toastType of
        Error ->
            Icon.warning Regular |> Icon.toHtml []
