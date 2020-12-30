# elm-pivot-table

Elm implementation of the pivot table.
Based on the [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/1.1.8/).

## Usage
Useless but demonstrates feel of the package.

```elm
module Main exposing (main)

import PivotTable exposing
    ( Table
    , makeTable
    , pivotTable
    )

import Element exposing
    ( Element
    , layout
    , width
    , height
    , fill
    , el
    , centerX
    , centerY
    , text
    )

import Element.Border
import Element.Background

type alias Person =
    { name : String
    , age : Int
    , gender : Gender
    }

type Gender
    = Male
    | Female

genderToString : Gender -> String
genderToString g = case g of
    Male -> "Male"
    Female -> "Female"

sampleTable : Table Person
sampleTable = makeTable
    [ Person "Alice" 23 Female
    , Person "Bob" 16 Male
    , Person "Charlie" 34 Male
    ]

pivotTableElement : Element msg
pivotTableElement =
    pivotTable
        { rowGroupFields = [.name]
        , colGroupFields = [.gender >> genderToString,.name]
        , aggregator = List.length
        , viewRow = \t ->
            el
                [ Element.Border.width 1
                , width fill
                , height fill
                , Element.Background.color <| Element.rgb255 196 255 196
                ] <| el [centerX, centerY] <| text t
        , viewCol = \t ->
            el
                [ Element.Border.width 1
                , width fill
                , height fill
                , Element.Background.color <| Element.rgb255 196 196 255
                ] <| el [centerX, centerY] <| text t
        , viewAgg = \i ->
            el
                [ Element.Border.width 1
                , width fill
                , height fill
                ] <| el [centerX, centerY] <| text <| String.fromInt i
        }
        sampleTable

main = pivotTableElement |> layout []
```
