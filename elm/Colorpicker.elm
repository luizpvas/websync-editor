module Colorpicker exposing (decoder, encode, view, white)

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


white : String
white =
    "#FFFFFF"



-- Json


encode : String -> Value
encode =
    Encode.string


decoder : Decoder String
decoder =
    Decode.string



-- View


view : String -> (String -> msg) -> Html msg
view selected onPick =
    Dropdown.view
        (div [ class "ws-colorpicker-trigger", style "background" selected ] [])
        (div [ class "ws-colorpicker" ]
            [ div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#F7FAFC"
                , color selected onPick "#EDF2F7"
                , color selected onPick "#E2E8F0"
                , color selected onPick "#CBD5E0"
                , color selected onPick "#A0AEC0"
                , color selected onPick "#718096"
                , color selected onPick "#4A5568"
                , color selected onPick "#2D3748"
                , color selected onPick "#1A202C"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#FFF5F5"
                , color selected onPick "#FED7D7"
                , color selected onPick "#FEB2B2"
                , color selected onPick "#FC8181"
                , color selected onPick "#F56565"
                , color selected onPick "#E53E3E"
                , color selected onPick "#C53030"
                , color selected onPick "#9B2C2C"
                , color selected onPick "#742A2A"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#FFFAF0"
                , color selected onPick "#FEEBC8"
                , color selected onPick "#FBD38D"
                , color selected onPick "#F6AD55"
                , color selected onPick "#ED8936"
                , color selected onPick "#DD6B20"
                , color selected onPick "#C05621"
                , color selected onPick "#9C4221"
                , color selected onPick "#7B341E"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#FFFFF0"
                , color selected onPick "#FEFCBF"
                , color selected onPick "#FAF089"
                , color selected onPick "#F6E05E"
                , color selected onPick "#ECC94B"
                , color selected onPick "#D69E2E"
                , color selected onPick "#B7791F"
                , color selected onPick "#975A16"
                , color selected onPick "#744210"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#F0FFF4"
                , color selected onPick "#C6F6D5"
                , color selected onPick "#9AE6B4"
                , color selected onPick "#68D391"
                , color selected onPick "#48BB78"
                , color selected onPick "#38A169"
                , color selected onPick "#2F855A"
                , color selected onPick "#276749"
                , color selected onPick "#22543D"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#E6FFFA"
                , color selected onPick "#B2F5EA"
                , color selected onPick "#81E6D9"
                , color selected onPick "#4FD1C5"
                , color selected onPick "#38B2AC"
                , color selected onPick "#319795"
                , color selected onPick "#2C7A7B"
                , color selected onPick "#285E61"
                , color selected onPick "#234E52"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#EBF8FF"
                , color selected onPick "#BEE3F8"
                , color selected onPick "#90CDF4"
                , color selected onPick "#63B3ED"
                , color selected onPick "#4299E1"
                , color selected onPick "#3182CE"
                , color selected onPick "#2B6CB0"
                , color selected onPick "#2C5282"
                , color selected onPick "#2A4365"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#EBF4FF"
                , color selected onPick "#C3DAFE"
                , color selected onPick "#A3BFFA"
                , color selected onPick "#7F9CF5"
                , color selected onPick "#667EEA"
                , color selected onPick "#5A67D8"
                , color selected onPick "#4C51BF"
                , color selected onPick "#434190"
                , color selected onPick "#3C366B"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#FAF5FF"
                , color selected onPick "#E9D8FD"
                , color selected onPick "#D6BCFA"
                , color selected onPick "#B794F4"
                , color selected onPick "#9F7AEA"
                , color selected onPick "#805AD5"
                , color selected onPick "#6B46C1"
                , color selected onPick "#553C9A"
                , color selected onPick "#44337A"
                ]
            , div [ class "ws-colorpicker-row" ]
                [ color selected onPick "#FFF5F7"
                , color selected onPick "#FED7E2"
                , color selected onPick "#FBB6CE"
                , color selected onPick "#F687B3"
                , color selected onPick "#ED64A6"
                , color selected onPick "#D53F8C"
                , color selected onPick "#B83280"
                , color selected onPick "#97266D"
                , color selected onPick "#702459"
                ]
            ]
        )


color : String -> (String -> msg) -> String -> Html msg
color selected onPick hex =
    if selected == hex then
        div [ class "ws-colorpicker-color selected", style "background" hex ] []

    else
        div [ class "ws-colorpicker-color", style "background" hex, onClick (onPick hex) ] []
