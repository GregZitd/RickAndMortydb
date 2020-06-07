module Palett exposing (..)

import Element
import Element.Border as Border

type alias Height = Int
type alias Width = Int
    
type alias WindowSize =
    { width : Int
    , height : Int
    }


setWindowHeightPc : Float -> WindowSize -> WindowSize
setWindowHeightPc perc window =
    let height = window.height
    in { window | height = percent perc height }

setWindowWidthPc : Float -> WindowSize -> WindowSize
setWindowWidthPc perc window =
    let width = window.width
    in { window | width = percent perc width }

setWindowHeightPx : Int -> WindowSize -> WindowSize
setWindowHeightPx size window =
    { window | height = size }

setWindowWidthPx : Int -> WindowSize -> WindowSize
setWindowWidthPx size window =
    { window | width = size }

noFocusShadow : Element.Attribute msg
noFocusShadow =
    Element.focused
        [ Border.shadow
              { offset = (0,0)
              , size = 0
              , blur = 0
              , color = white
                        }
        ]
    
black : Element.Color
black = Element.rgb255 0 0 0

white : Element.Color
white = Element.rgb255 255 255 255

orange : Element.Color
orange = Element.rgb255 255 140 0
         
green : Element.Color
green = Element.rgb255 0 204 204

grey : Element.Color
grey = Element.rgb255 105 105 105

percent : Float -> Int -> Int
percent perc num =
    round <| perc * (toFloat num) / 100
