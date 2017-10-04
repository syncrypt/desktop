module Language exposing (Language(..), HasLanguage)


type Language
    = English
    | German


type alias HasLanguage a =
    { a | language : Language }
