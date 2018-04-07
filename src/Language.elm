module Language exposing (HasLanguage, Language(..), fromLocale)

import String


type Language
    = English
    | German


type alias HasLanguage a =
    { a | language : Language }


fromLocale : String -> Language
fromLocale localeString =
    case String.split "_" localeString of
        "en" :: _ ->
            English

        "de" :: _ ->
            German

        _ ->
            English
