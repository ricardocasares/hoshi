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
    }


flags : Decoder Flags
flags =
    Decode.map Flags
        (Decode.field "basePath" Decode.string)


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


fromElm : Codec FromElm
fromElm =
    Codec.custom (Just "tag")
        (\vElmReady value ->
            case value of
                ElmReady ->
                    vElmReady
        )
        |> Codec.variant0 "ElmReady" ElmReady
        |> Codec.buildCustom
