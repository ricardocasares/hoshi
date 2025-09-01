module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), interop)

import TsJson.Codec as Codec exposing (Codec)
import TsJson.Decode as Decode exposing (Decoder)
import TsJson.Encode exposing (Encoder)


interop :
    { flags : Decoder Flags
    , toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    }
interop =
    { flags = flags
    , toElm = toElm |> Codec.decoder
    , fromElm = fromElm |> Codec.encoder
    }


type alias Flags =
    { basePath : String
    , theme : String
    }


flags : Decoder Flags
flags =
    Decode.map2 Flags
        (Decode.field "basePath" Decode.string)
        (Decode.field "theme" Decode.string)


type ToElm
    = JSReady


toElm : Codec ToElm
toElm =
    Codec.custom (Just "tag")
        (\vJSReady value ->
            case value of
                JSReady ->
                    vJSReady
        )
        |> Codec.variant0 "JSReady" JSReady
        |> Codec.buildCustom


type FromElm
    = ElmReady
    | SaveTheme String


fromElm : Codec FromElm
fromElm =
    Codec.custom (Just "tag")
        (\vElmReady vSaveTheme value ->
            case value of
                ElmReady ->
                    vElmReady

                SaveTheme theme ->
                    vSaveTheme theme
        )
        |> Codec.variant0 "ElmReady" ElmReady
        |> Codec.namedVariant1 "SaveTheme" SaveTheme ( "theme", Codec.string )
        |> Codec.buildCustom
