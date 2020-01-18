module Icon exposing
    ( alignCenter
    , alignLeft
    , alignRight
    , button
    , close
    , divider
    , download
    , fadeNone
    , fadeTiltLeft
    , fadeTiltRight
    , fadeWave
    , illustration
    , image
    , move
    , redo
    , save
    , text
    , trash
    , undo
    , youtube
    )

import Svg exposing (..)
import Svg.Attributes exposing (..)


move : Svg msg
move =
    svg [ viewBox "0 0 512 512", width "18", height "18" ] [ Svg.path [ fill "currentColor", d "M353 429l-74 74a32 32 0 01-46 0l-74-74c-9-10-9-25 0-34l11-11c10-10 25-10 35 0l23 26V284H102l26 23c10 10 10 25 0 35l-11 11c-9 9-24 9-34 0L9 279a32 32 0 010-46l74-74c10-9 25-9 34 0l11 11c10 10 10 25 0 35l-26 23h126V102l-23 26c-10 10-25 10-35 0l-11-11c-9-9-9-24 0-34l74-74c13-12 33-12 46 0l74 74c9 10 9 25 0 34l-11 11c-10 10-25 10-35 0l-23-26v126h126l-26-23c-10-10-10-25 0-35l11-11c9-9 24-9 34 0l74 74c12 13 12 33 0 46l-74 74c-10 9-25 9-34 0l-11-11c-10-10-10-25 0-35l26-23H284v126l23-26c10-10 25-10 35 0l11 11c9 9 9 24 0 34z" ] [] ]


trash : Svg msg
trash =
    svg [ viewBox "0 0 20 20", height "18" ] [ Svg.path [ d "M6 2l2-2h4l2 2h4v2H2V2h4zM3 6h14l-1 14H4L3 6zm5 2v10h1V8H8zm3 0v10h1V8h-1z" ] [] ]


close : Svg msg
close =
    svg [ viewBox "0 0 20 20", height "18" ] [ Svg.path [ d "M10 8.586L2.929 1.515 1.515 2.929 8.586 10l-7.071 7.071 1.414 1.414L10 11.414l7.071 7.071 1.414-1.414L11.414 10l7.071-7.071-1.414-1.414L10 8.586z" ] [] ]


alignLeft : Svg msg
alignLeft =
    svg [ viewBox "0 0 20 20", height "18" ] [ Svg.path [ d "M1 1h18v2H1V1zm0 8h18v2H1V9zm0 8h18v2H1v-2zM1 5h12v2H1V5zm0 8h12v2H1v-2z" ] [] ]


alignRight : Svg msg
alignRight =
    svg [ viewBox "0 0 20 20", height "18" ] [ Svg.path [ d "M1 1h18v2H1V1zm0 8h18v2H1V9zm0 8h18v2H1v-2zM7 5h12v2H7V5zm0 8h12v2H7v-2z" ] [] ]


alignCenter : Svg msg
alignCenter =
    svg [ viewBox "0 0 20 20", height "18" ] [ Svg.path [ d "M1 1h18v2H1V1zm0 8h18v2H1V9zm0 8h18v2H1v-2zM4 5h12v2H4V5zm0 8h12v2H4v-2z" ] [] ]


button : Svg msg
button =
    svg [ fill "none", viewBox "0 0 24 24", width "24" ] [ Svg.path [ fill "currentColor", fillRule "evenodd", d "M21 8H3a1 1 0 00-1 1v6c0 .6.4 1 1 1h18c.6 0 1-.4 1-1V9c0-.6-.4-1-1-1zM3 6a3 3 0 00-3 3v6a3 3 0 003 3h18a3 3 0 003-3V9a3 3 0 00-3-3H3z", clipRule "evenodd" ] [] ]


divider : Svg msg
divider =
    svg [ fill "none", viewBox "0 0 24 24", width "24" ] [ Svg.path [ fill "currentColor", fillRule "evenodd", d "M24 13H0v-2h24v2z", clipRule "evenodd" ] [] ]


image : Svg msg
image =
    svg [ fill "none", viewBox "0 0 24 24", width "24" ] [ Svg.path [ fill "currentColor", fillRule "evenodd", d "M22 6H2v12h.6l5.2-6.3 4 2.5 5.7-6.1 4.5 4.5V6zM2 4a2 2 0 00-2 2v12c0 1.1.9 2 2 2h20a2 2 0 002-2V6a2 2 0 00-2-2H2zm15.5 7l-5.3 5.8-4-2.5-2.6 3.2h15.9v-2.6l-4-4zM6 9a1 1 0 11-2 0 1 1 0 012 0zm1 0a2 2 0 11-4 0 2 2 0 014 0z", clipRule "evenodd" ] [] ]


text : Svg msg
text =
    svg [ fill "none", viewBox "0 0 24 24", width "24" ] [ Svg.path [ fill "currentColor", d "M4 19.5l1.3-.1L11.5 4h2l6.2 15.4 1.3.1V21h-5.1v-1.5l1.3-.2-1.1-3.1H9l-1.3 3.1 1.4.2V21H4v-1.5zm5.7-5.2h5.6L12.6 7l-2.9 7.3z" ] [] ]


youtube : Svg msg
youtube =
    svg [ width "24", height "24", fill "none" ] [ Svg.path [ fill "currentColor", fillRule "evenodd", d "M4 5a4 4 0 0 0-4 4v6a4 4 0 0 0 4 4s2 .5 8 .5 8-.5 8-.5a4 4 0 0 0 4-4V9a4 4 0 0 0-4-4s-4-.5-8-.5S4 5 4 5zm6 10.5l6-3.5-6-3.5v7z", clipRule "evenodd" ] [] ]


illustration : Svg msg
illustration =
    svg [ viewBox "0 0 478 478", height "18" ] [ Svg.path [ fill "currentColor", d "M476 231c-2-3-4-6-7-7l-67-36 67-36a17 17 0 000-30L247 2c-5-3-11-3-16 0L9 122a17 17 0 000 30l67 36-67 36a17 17 0 000 30l67 36-67 36a17 17 0 000 30l222 120c5 3 11 3 16 0l222-120a17 17 0 000-30l-67-36 67-36c8-5 11-15 7-23zM53 137L239 36l186 101-186 100L53 137zm372 204L239 441 53 341l59-31 119 63c5 3 11 3 16 0l119-63 59 31zm-186-2L53 239l59-32 119 64c5 3 11 3 16 0l119-64 59 32-186 100z" ] [] ]


fadeWave : Svg msg
fadeWave =
    svg [ fill "none", viewBox "0 0 30 30", width "40" ] [ Svg.path [ fill "#818181", d "M29 11H1v5.4c1 .8 3.3 3.1 8.9 2.5 4.2-.5 5.1-1 10.2-2.5 4-1.1 8 1.3 8.9 2.5V11z" ] [] ]


fadeNone : Svg msg
fadeNone =
    svg [ fill "none", viewBox "0 0 30 30", width "40" ] [ Svg.path [ fill "#818181", d "M1 11h28v8H1z" ] [] ]


fadeTiltRight : Svg msg
fadeTiltRight =
    svg [ fill "none", viewBox "0 0 30 30", width "40" ] [ Svg.path [ fill "#818181", d "M29 11H1l28 7.9V11z" ] [] ]


fadeTiltLeft : Svg msg
fadeTiltLeft =
    svg [ fill "none", viewBox "0 0 30 30", width "40" ] [ Svg.path [ fill "#818181", d "M1 11h28L1 18.9V11z" ] [] ]


download : Svg msg
download =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M13 8V2H7v6H2l8 8 8-8h-5zM0 18h20v2H0v-2z" ] [] ]


save : Svg msg
save =
    svg [ viewBox "0 0 20 20" ] [ Svg.path [ d "M0 2C0 .9.9 0 2 0h14l4 4v14a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V2zm5 0v6h10V2H5zm6 1h3v4h-3V3z" ] [] ]


undo : Svg msg
undo =
    svg [ viewBox "0 0 24 24", width "24", height "24" ] [ Svg.path [ d "M12.5 8c-2.6 0-5 1-6.9 2.6L2 7v9h9l-3.6-3.6A8 8 0 0 1 20 16l2.4-.8a10.5 10.5 0 0 0-10-7.2z" ] [] ]


redo : Svg msg
redo =
    svg [ viewBox "0 0 24 24", width "24", height "24" ] [ Svg.path [ d "M18.4 10.6a10.5 10.5 0 0 0-16.9 4.6L4 16a8 8 0 0 1 12.7-3.6L13 16h9V7l-3.6 3.6z" ] [] ]
