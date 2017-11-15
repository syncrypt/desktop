module Language exposing (HasLanguage, Language(..))


type Language
    = English
    | German


type alias HasLanguage a =
    { a | language : Language }
