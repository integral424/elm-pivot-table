module PivotTable exposing
    ( Table, Field
    , makeTable, getField
    , Aggregator, pivotTable, pivotTableHtml
    )

{-| This package provides a pivot table view function with which
you can analyze and visualize your data by grouping various fields.


# Definition

@docs Table, Field


# Table operations

@docs makeTable, getField


# Pivot table

In this package, a pivot table is
a table to show values grouped by some fields.


## Example 1

    genderToString : Gender -> String
    genderToString gender =
        case gender of
            Male   -> "Male"
            Female -> "Female"

    pivotTable
        { rowGroupFields = []
        , colGroupFields = [ genderField ]
        , aggregator = List.length
        , viewRow = always Element.none
        , viewCol = Element.text
        , viewAgg = String.fromInt >> Element.text
        }
        myTable

The code above produces a table:

```text
|Male |Female |
|-----|-------|
|2    |1      |
```

@docs Aggregator, pivotTable, pivotTableHtml

-}

import Array exposing (Array)
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , fillPortion
        , height
        , none
        , row
        , shrink
        , width
        )
import Html
    exposing
        ( Html
        , table
        , td
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( colspan
        , rowspan
        )
import List.Extra
import Set exposing (Set)


{-| In this package, a table is defined as a list of rows.

In the most naive case, a table may be represented by a list of records like:

    type alias Person =
        { name : String
        , age : Int
        , gender : Gender
        }

    type Gender
        = Male
        | Female

    type alias MyTable =
        Table Person

**Note:** Notice that rows can be any type.

-}
type Table row
    = Table (List row)


{-| A function to create a table.

Since the `Table` is an opaque type, all user of this package must use this
function to create a `Table` value.

-}
makeTable : List row -> Table row
makeTable rows =
    Table rows


{-| In this package, a field is defined as a function from row to something.
-}
type alias Field row a =
    row -> a


{-| A function to get values from a table.

With the example appeared in the [`Table` doc](#Table),
each field can be accessed like:

    nameField = .name
    ageField = .age
    genderField = .gender

    myTable : MyTable
    myTable = makeTable
        [ { name = "Alice",   age = 17, gender = Female }
        , { name = "Bob",     age = 8,  gender = Male   }
        , { name = "Charlie", age = 35, gender = Male   }
        ]

    getField nameField myTable   -- ["Alice", "Bob", "Charlie"]
    getField ageField myTable    -- [17, 8, 35]
    getField genderField myTable -- [Female, Male, Male]

**Note:** Notice that a field does not have to be a record element.
This is a valid code:

    nameInitialLetterField = .name >> String.left 1

    getField nameInitialLetterField myTable -- ["A", "B", "C"]

-}
getField : Field row a -> Table row -> List a
getField field (Table rows) =
    List.map field rows



-- Utility functions for `Table`


length : Table row -> Int
length (Table rows) =
    List.length rows


map : (row1 -> row2) -> Table row1 -> Table row2
map f (Table rows) =
    rows |> List.map f |> Table


indexedMap : (Int -> row1 -> row2) -> Table row1 -> Table row2
indexedMap f (Table rows) =
    rows |> List.indexedMap f |> Table



-- Grouping functions


{-| Group rows by a given field, that is, the same group has the same field value.

Result is a list of groups.
A group is composed of a field value (of type `comparable`) and a table value.
All rows in this table has the same field value.

-}
groupByField : Field row comparable -> Table row -> List ( comparable, Table row )
groupByField field (Table rows) =
    rows
        |> List.sortBy field
        -- sorting is necessary for `List.Extra.groupWhile`
        |> List.Extra.groupWhile (\r1 r2 -> field r1 == field r2)
        |> List.map (\( first, rest ) -> ( field first, Table (first :: rest) ))


{-| Group rows by given several fields.

Notice that group of a group forms a tree.
See [`Tree` definition](#Tree) for detail.

-}
group : List (Field row comparable) -> Table row -> Tree row comparable
group fields tbl =
    let
        reduce : Field row comparable -> Tree row comparable -> Tree row comparable
        reduce field tree =
            case tree of
                Leaf tbl_ ->
                    groupByField field tbl_
                        |> List.map (Tuple.mapSecond Leaf)
                        |> Node

                Node lst ->
                    lst
                        |> List.map (Tuple.mapSecond (reduce field))
                        |> Node
    in
    List.foldl reduce (Leaf tbl) fields


{-| A type representing a result of the `group` function.

Grouping by several fields forms a tree.


## Example

    myTable2 =
        makeTable
            [ { col1 = "A", col2 = 1, col3 = "a", col4 = 1 }
            , { col1 = "A", col2 = 1, col3 = "b", col4 = 2 }
            , { col1 = "A", col2 = 2, col3 = "a", col4 = 1 }
            , { col1 = "A", col2 = 2, col3 = "b", col4 = 2 }
            , { col1 = "A", col2 = 2, col3 = "c", col4 = 3 }
            , { col1 = "A", col2 = 3, col3 = "a", col4 = 1 }
            , { col1 = "A", col2 = 3, col3 = "a", col4 = 2 }
            , { col1 = "B", col2 = 2, col3 = "a", col4 = 1 }
            , { col1 = "B", col2 = 2, col3 = "b", col4 = 2 }
            , { col1 = "B", col2 = 2, col3 = "c", col4 = 3 }
            ]

  - col1 = "A"
      - col2 = 1
          - col3 = "a"
              - row#1
          - col3 = \*b
              - row#2
      - col2 = 2
          - col3 = "a"
              - row#3
          - col3 = "b"
              - row#4
          - col3 = "c"
              - row#5
      - col2 = 3
          - col3 = "a"
              - row#6, row#7
  - col1 = "B"
      - col2 = 2
          - col3 = "a"
              - row#8
          - col3 = "b"
              - row#9
          - col3 = "c"
              - row#10

-}
type Tree row comparable
    = Node (List ( comparable, Tree row comparable ))
    | Leaf (Table row)



-- Tree utility functions


{-| A path of a tree from the root to a leaf
-}
type alias TreePath comparable =
    List comparable


{-| Get a tree by path.
-}
getTreeAt : TreePath comparable -> Tree row comparable -> Maybe (Tree row comparable)
getTreeAt path tree =
    case path of
        [] ->
            Just tree

        first :: rest ->
            case tree of
                Leaf _ ->
                    Nothing

                Node lst ->
                    lst
                        |> List.filter (\( c, _ ) -> c == first)
                        |> List.head
                        |> Maybe.andThen (\( _, subTree ) -> getTreeAt rest subTree)


{-| Get a tree by path, only when the path reaches a leaf.
-}
getAt : TreePath comparable -> Tree row comparable -> Maybe (Table row)
getAt path tree =
    case getTreeAt path tree of
        Nothing ->
            Nothing

        Just (Node _) ->
            Nothing

        Just (Leaf tbl) ->
            Just tbl


{-| Get possible paths from given tree.
-}
getPaths : Tree row comparable -> List (TreePath comparable)
getPaths tree =
    case tree of
        Leaf _ ->
            [ [] ]

        Node lst ->
            lst
                |> List.map (Tuple.mapSecond getPaths)
                |> List.concatMap (\( c, paths ) -> paths |> List.map (\path -> c :: path))


{-| Convert row type into another type.
-}
mapTree : (row1 -> row2) -> Tree row1 comparable -> Tree row2 comparable
mapTree f tree =
    case tree of
        Node lst ->
            lst
                |> List.map (Tuple.mapSecond (mapTree f))
                |> Node

        Leaf tbl ->
            Leaf (map f tbl)


{-| Extract all `Table` values from given tree.

The result is a list of tables ordered by depth-first-search order.

-}
treeToTableList : Tree row comparable -> List (Table row)
treeToTableList tree =
    case tree of
        Leaf tbl ->
            [ tbl ]

        Node lst ->
            List.concatMap (Tuple.second >> treeToTableList) lst


{-| Get the width of a tree, which is the number of leafs the tree has.
-}
getWidth : Tree row comparable -> Int
getWidth tree =
    case tree of
        Leaf _ ->
            1

        Node lst ->
            lst |> List.map (Tuple.second >> getWidth) |> List.sum


{-| Apply a function in breadth-first-search order.

The result is a list of each levels result, that is:

    result = applyHorizontally f tree

    List.Extra.getAt 0 result == Just firstDepthResult
    List.Extra.getAt 1 result == Just secondDepthResult
    List.Extra.getAt 2 result == Just thirdDepthResult

-}
applyHorizontally : (( comparable, Tree row comparable ) -> a) -> Tree row comparable -> List (List a)
applyHorizontally f tree =
    let
        applyOneLevel : Tree row comparable -> List a
        applyOneLevel tree_ =
            case tree_ of
                Leaf _ ->
                    []

                Node lst ->
                    List.map f lst

        getSubTrees : Tree row comparable -> List (Tree row comparable)
        getSubTrees tree_ =
            case tree_ of
                Leaf _ ->
                    []

                Node lst ->
                    List.map Tuple.second lst

        g : List (Tree row comparable) -> List (List a) -> List (List a)
        g trees prevResult =
            let
                result : List a
                result =
                    List.concatMap applyOneLevel trees

                nextResult : List (List a)
                nextResult =
                    result :: prevResult
            in
            case result of
                [] ->
                    prevResult

                _ ->
                    g (List.concatMap getSubTrees trees) nextResult
    in
    g [ tree ] [] |> List.reverse



-- Pivot table functions


{-| The pivot table groups table rows.
After that, the `Aggregator` aggregates
the list of rows into a single value (of any type).
-}
type alias Aggregator row agg =
    List row -> agg


{-| Draws a pivot table of `Html` type.

This view makes use of `colspan` and `rowspan` attributes of html table.
Use this view function when you want to avoid using `elm-ui`.

-}
pivotTableHtml :
    { rowGroupFields : List (Field row comparable1)
    , colGroupFields : List (Field row comparable2)
    , aggregator : Aggregator row agg
    , viewRow : comparable1 -> Html msg
    , viewCol : comparable2 -> Html msg
    , viewAgg : agg -> Html msg
    }
    -> Table row
    -> Html msg
pivotTableHtml { rowGroupFields, colGroupFields, aggregator, viewRow, viewCol, viewAgg } tbl =
    let
        indexedTable : Table ( Int, row )
        indexedTable =
            indexedMap Tuple.pair tbl

        getRowByIndex : Table row -> Int -> Maybe row
        getRowByIndex (Table lst) index =
            List.Extra.getAt index lst

        -- ignore index
        columnShim : Field row comparable -> Field ( Int, row ) comparable
        columnShim col =
            Tuple.second >> col

        rowGroup : Tree Int comparable1
        rowGroup =
            group (List.map columnShim rowGroupFields) indexedTable |> mapTree Tuple.first

        colGroup : Tree Int comparable2
        colGroup =
            group (List.map columnShim colGroupFields) indexedTable |> mapTree Tuple.first

        -- rowPaths : List (TreePath comparable1)
        -- rowPaths = getPaths rowGroup
        colPaths : List (TreePath comparable2)
        colPaths =
            getPaths colGroup

        viewColHeaderCells : Tree Int comparable2 -> List ( Html msg, Tree Int comparable2 )
        viewColHeaderCells tree =
            let
                f : ( comparable2, Tree Int comparable2 ) -> ( Html msg, Tree Int comparable2 )
                f ( c, subTree ) =
                    ( th [ colspan <| getWidth subTree ] [ viewCol c ], subTree )
            in
            case tree of
                Leaf _ ->
                    []

                Node lst ->
                    List.map f lst

        viewColHeaders : List (Tree Int comparable2) -> List (Html msg)
        viewColHeaders trees =
            case trees of
                [] ->
                    []

                _ ->
                    let
                        tmp1 : List ( Html msg, Tree Int comparable2 )
                        tmp1 =
                            List.concatMap viewColHeaderCells trees

                        cells : List (Html msg)
                        cells =
                            List.map Tuple.first tmp1

                        subTrees : List (Tree Int comparable2)
                        subTrees =
                            List.map Tuple.second tmp1

                        shim : Html msg
                        shim =
                            td [ colspan <| List.length rowGroupFields ] []
                    in
                    if List.length rowGroupFields == 0 then
                        tr [] cells :: viewColHeaders subTrees

                    else
                        tr [] (shim :: cells) :: viewColHeaders subTrees

        pathToTableItems : List (Maybe ( Int, comparable1 )) -> List (Html msg)
        pathToTableItems lst =
            List.filterMap (Maybe.map (\( width, c ) -> th [ rowspan width ] [ viewRow c ])) lst

        aggregateForRow : Table Int -> List (Html msg)
        aggregateForRow tbl_ =
            colPaths
                |> List.filterMap (\path -> getAt path colGroup)
                |> List.map getIndices
                |> List.map (Set.intersect (getIndices tbl_))
                |> List.map Set.toList
                |> List.map (List.filterMap (getRowByIndex tbl))
                |> List.map aggregator
                |> List.map viewAgg
                |> List.map List.singleton
                |> List.map (td [])

        viewRows : Tree Int comparable1 -> List (Html msg)
        viewRows tree =
            List.map2 Tuple.pair (organize tree) (treeToTableList tree)
                |> List.map (Tuple.mapBoth pathToTableItems aggregateForRow)
                |> List.map (\( x, y ) -> List.append x y)
                |> List.map (tr [])

        organize : Tree Int comparable1 -> List (List (Maybe ( Int, comparable1 )))
        organize tree =
            let
                f :
                    ( comparable1, Tree Int comparable1 )
                    -> List (List (Maybe ( Int, comparable1 )))
                f ( c, subTree ) =
                    organize subTree
                        |> List.indexedMap (g c subTree)

                g :
                    comparable1
                    -> Tree Int comparable1
                    -> Int
                    -> List (Maybe ( Int, comparable1 ))
                    -> List (Maybe ( Int, comparable1 ))
                g c subTree i path =
                    if i == 0 then
                        Just ( getWidth subTree, c ) :: path

                    else
                        Nothing :: path
            in
            case tree of
                Leaf _ ->
                    [ [] ]

                Node lst ->
                    List.concatMap f lst

        getIndices : Table Int -> Set Int
        getIndices (Table indices) =
            Set.fromList indices
    in
    viewColHeaders [ colGroup ] ++ viewRows rowGroup |> table []


{-| Draws a pivot table.

  - `rowGroupFields` is a list of `Field`'s to group and generates grouped _rows_.
  - `colGroupFields` is a list of `Field`'s to group and generates grouped _columns_.
  - `aggregator` aggregates each grouped set of table data into a single value.
  - `viewRow`, `viewCol` and `viewAgg` are view functions to show each headers and cells.

-}
pivotTable :
    { rowGroupFields : List (Field row comparable1)
    , colGroupFields : List (Field row comparable2)
    , aggregator : Aggregator row agg
    , viewRow : comparable1 -> Element msg
    , viewCol : comparable2 -> Element msg
    , viewAgg : agg -> Element msg
    }
    -> Table row
    -> Element msg
pivotTable { rowGroupFields, colGroupFields, aggregator, viewRow, viewCol, viewAgg } (Table rows) =
    let
        indexedTable : Table ( Int, row )
        indexedTable =
            indexedMap Tuple.pair (Table rows)

        arr : Array row
        arr =
            Array.fromList rows

        getRowByIndex : Int -> Maybe row
        getRowByIndex index =
            Array.get index arr

        getIndices : Table Int -> Set Int
        getIndices (Table indices) =
            Set.fromList indices

        -- ignore index
        columnShim : Field row comparable -> Field ( Int, row ) comparable
        columnShim field =
            Tuple.second >> field

        rowGroup : Tree Int comparable1
        rowGroup =
            group (List.map columnShim rowGroupFields) indexedTable |> mapTree Tuple.first

        colGroup : Tree Int comparable2
        colGroup =
            group (List.map columnShim colGroupFields) indexedTable |> mapTree Tuple.first

        rowPaths : List (TreePath comparable1)
        rowPaths =
            getPaths rowGroup

        viewRowHeader : Tree Int comparable1 -> Element msg
        viewRowHeader tree =
            tree
                |> applyHorizontally (\( c, subTree ) -> el [ width fill, height <| fillPortion <| getWidth subTree ] <| viewRow c)
                |> List.map (column [ width <| fillPortion 1, height fill ])
                |> row [ width <| fillPortion <| List.length rowGroupFields, height <| fillPortion <| getWidth rowGroup ]

        viewRightPartColumn : Int -> ( comparable2, Tree Int comparable2 ) -> Element msg
        viewRightPartColumn h ( c, subTree ) =
            column
                [ width <| fillPortion <| getWidth subTree
                , height fill
                ]
                [ el [ width fill, height fill ] <| viewCol c
                , viewRightPart (h - 1) subTree
                ]

        viewRightPart : Int -> Tree Int comparable2 -> Element msg
        viewRightPart h tree =
            case tree of
                Leaf tbl ->
                    rowPaths
                        |> List.map (\path -> getAt path rowGroup)
                        |> List.map (Maybe.map getIndices >> Maybe.withDefault Set.empty)
                        |> List.map (Set.intersect (getIndices tbl))
                        |> List.map Set.toList
                        |> List.map (List.filterMap getRowByIndex)
                        |> List.map aggregator
                        |> List.map viewAgg
                        |> column [ width <| fillPortion 1, height <| fillPortion <| h ]

                Node lst ->
                    lst
                        |> List.map (viewRightPartColumn h)
                        |> row [ width fill, height fill ]
    in
    row
        [ width fill ]
        [ column
            [ width shrink
            , height fill
            ]
            [ el
                [ width fill
                , height <| fillPortion <| List.length colGroupFields
                ]
              <|
                Element.none
            , el
                [ width fill
                ]
              <|
                viewRowHeader rowGroup
            ]
        , el
            [ width shrink
            , height fill
            ]
          <|
            viewRightPart (List.length colGroupFields + getWidth rowGroup) colGroup
        ]
