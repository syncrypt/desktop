module Dialog exposing (..)

import Ui.Modal


type alias WithModalState a =
    { a | modal : Ui.Modal.Model }


asModalIn : WithModalState a -> Ui.Modal.Model -> WithModalState a
asModalIn state modal =
    { state | modal = modal }
