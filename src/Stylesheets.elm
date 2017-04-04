port module Stylesheets exposing (..)

import Css.File exposing (CssFileStructure, CssCompilerProgram)
import View.Css.VaultList
import View.Css.MainScreen
import View.Css.VaultDialog
import VaultCreationDialog.Css


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ View.Css.VaultList.css
                , View.Css.MainScreen.css
                , View.Css.VaultDialog.css
                , VaultCreationDialog.Css.css
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
