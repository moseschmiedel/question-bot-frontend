module Ui exposing
    ( apptitle
    )

import Element exposing 
    ( Element
    , Attribute
    , el
    , text
    )
import Element.Font as Font

apptitle : List (Attribute msg) -> String -> Element msg
apptitle attrs title = el (attrs ++ [ Font.size 50 ]) <| text title