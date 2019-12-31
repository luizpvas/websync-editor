module DomElement exposing (DomElement, fromBrowser)

import Browser.Dom


type alias DomElement =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


fromBrowser : Browser.Dom.Element -> DomElement
fromBrowser { element } =
    { x = round element.x
    , y = round element.y
    , width = round element.width
    , height = round element.height
    }
