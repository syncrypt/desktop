port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import VaultList.Css
import MainScreen.Css
import VaultDialog.Css
import VaultCreationDialog.Css


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ VaultList.Css.css
                , MainScreen.Css.css
                , VaultDialog.Css.css
                , VaultCreationDialog.Css.css
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
