module UI.Alert exposing (success, error, warning, info)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


type AlertType
    = Success
    | Error
    | Warning
    | Info


alert : AlertType -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
alert alertType attrs children =
    let
        baseClass =
            case alertType of
                Success ->
                    "alert alert-success"

                Error ->
                    "alert alert-error"

                Warning ->
                    "alert alert-warning"

                Info ->
                    "alert alert-info"
    in
    div (class baseClass :: attrs) children


success : List (Html.Attribute msg) -> List (Html msg) -> Html msg
success attrs children =
    alert Success attrs children


error : List (Html.Attribute msg) -> List (Html msg) -> Html msg
error attrs children =
    alert Error attrs children


warning : List (Html.Attribute msg) -> List (Html msg) -> Html msg
warning attrs children =
    alert Warning attrs children


info : List (Html.Attribute msg) -> List (Html msg) -> Html msg
info attrs children =
    alert Info attrs children