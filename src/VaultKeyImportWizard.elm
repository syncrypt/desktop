module VaultKeyImportWizard exposing (settings, viewSettings)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Language exposing (HasLanguage)
import Model
import Translation as T
import Util exposing (button)
import WizardDialog.Model exposing (..)
import WizardDialog.View exposing (infoText)


steps : List (StepConfig Model.Model Model.Msg)
steps =
    [ ( "Import Vault Key", step1 )
    , ( "Select Vault Import Folder", step2 )
    ]


settings : Model.Model -> WizardSettings Model.Msg
settings model =
    { address = Model.WizardDialogMsg
    , name = "VaultKeyImportWizard"
    , onFinishMsg = Just Model.VaultKeyImportWizardFinished
    , steps = steps |> List.map Tuple.first
    , wizardType = VaultKeyImportWizard
    , closable = False
    }


wizardContent : List (Html msg) -> Html msg
wizardContent body =
    div [ class "MainScreen-VaultKeyImportWizard" ]
        body


viewSettings : State Model.Msg -> Model.Model -> Maybe (ViewSettings Model.Msg)
viewSettings state model =
    WizardDialog.Model.viewSettings steps state model


t : T.VaultKeyImportWizardText -> HasLanguage a -> String
t text model =
    T.t (T.VaultKeyImportWizardText text) model



-- STEPS


step1 model state =
    Just
        { title = t T.VaultKeyImport model
        , contents =
            wizardContent
                [ infoText []
                    [ t T.ImportYourVaultKeyHere model ]
                , div []
                    [ button []
                        { label = t T.SelectVaultKeyFileForImport model
                        , onClick = Model.OpenVaultKeyImportFileDialog
                        }
                    ]
                ]

        -- TODO only show next button if key file was selected
        , buttons = Default
        }


step2 model state =
    Just
        { title = t T.VaultKeyImport model
        , contents =
            wizardContent
                [ infoText []
                    [ t T.SelectYourVaultImportDestinationFolder model ]
                , div []
                    [ button []
                        { label = t T.SelectYourVaultImportDestinationFolder model
                        , onClick = Model.OpenVaultImportFolderDialog
                        }
                    ]
                ]
        , buttons = Default
        }
