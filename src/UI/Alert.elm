module UI.Alert exposing (error)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


type AlertType
    = Error


alert : AlertType -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
alert alertType attrs children =
    let
        baseClass : String
        baseClass =
            case alertType of
                Error ->
                    "alert alert-error"
    in
    div (class baseClass :: attrs) children


error : List (Html.Attribute msg) -> List (Html msg) -> Html msg
error attrs children =
    alert Error attrs children