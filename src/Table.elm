module Table exposing
    ( Column
    , Table
    , emptyTable
    , makeTable
    , Aggregator
    , pivotTable
    , pivotTableHtml
    )

import Html exposing
    ( Html
    , table
    , td
    , th
    , tr
    )
import Html.Attributes exposing
    ( colspan
    , rowspan
    )
import Array exposing (Array)
import List.Extra
import Set exposing (Set)

import Element exposing
    ( Element
    , none
    , el
    , row
    , column
    , width
    , height
    , fill
    , fillPortion
    , shrink
    )

type Table row
    = Table (List row)


type alias Column row a =
    row -> a


makeTable : List row -> Table row
makeTable rows =
    Table rows


emptyTable : Table row
emptyTable =
    Table []



--getColumn : Column row a -> Table row -> List a
--getColumn col (Table rows) = List.map col rows


length : Table row -> Int
length (Table rows) =
    List.length rows


map : (row1 -> row2) -> Table row1 -> Table row2
map f (Table rows) =
    rows |> List.map f |> Table



--map f = getColumn f >> Table


indexedMap : (Int -> row1 -> row2) -> Table row1 -> Table row2
indexedMap f (Table rows) =
    rows |> List.indexedMap f |> Table


groupByColumn : Column row comparable -> Table row -> List ( comparable, Table row )
groupByColumn col (Table rows) =
    rows
        |> List.sortBy col
        |> List.Extra.groupWhile (\r1 r2 -> col r1 == col r2)
        |> List.map (\( first, rest ) -> ( col first, Table (first :: rest) ))


type Tree row comparable
    = Node (List ( comparable, Tree row comparable ))
    | Leaf (Table row)


type alias TreePath comparable =
    List comparable


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


getAt : TreePath comparable -> Tree row comparable -> Maybe (Table row)
getAt path tree =
    case getTreeAt path tree of
        Nothing ->
            Nothing

        Just (Node _) ->
            Nothing

        Just (Leaf tbl) ->
            Just tbl


getPaths : Tree row comparable -> List (TreePath comparable)
getPaths tree =
    case tree of
        Leaf _ ->
            [ [] ]

        Node lst ->
            lst
                |> List.map (Tuple.mapSecond getPaths)
                |> List.concatMap (\( c, paths ) -> paths |> List.map (\path -> c :: path))


mapTree : (row1 -> row2) -> Tree row1 comparable -> Tree row2 comparable
mapTree f tree =
    case tree of
        Node lst ->
            lst
                |> List.map (Tuple.mapSecond (mapTree f))
                |> Node

        Leaf tbl ->
            Leaf (map f tbl)



-- treeToTable : Tree row comparable -> Table row
-- treeToTable tree =
--     let
--         f : Tree row comparable -> List row
--         f subTree =
--             case subTree of
--                 Leaf (Table rows) -> rows
--                 Node lst -> lst |> List.concatMap (Tuple.second >> f)
--     in
--     f tree |> Table


treeToTableList : Tree row comparable -> List (Table row)
treeToTableList tree =
    case tree of
        Leaf tbl ->
            [ tbl ]

        Node lst ->
            List.concatMap (Tuple.second >> treeToTableList) lst


getWidth : Tree row comparable -> Int
getWidth tree =
    case tree of
        Leaf _ ->
            1

        Node lst ->
            lst |> List.map (Tuple.second >> getWidth) |> List.sum

{- breadth first traversal -}
applyHorizontally : ((comparable, Tree row comparable) -> a) -> Tree row comparable -> List (List a)
applyHorizontally f tree =
    let
        applyOneLevel : Tree row comparable -> List a
        applyOneLevel tree_ =
            case tree_ of
                Leaf _ -> []
                Node lst -> List.map f lst

        getSubTrees : Tree row comparable -> List (Tree row comparable)
        getSubTrees tree_ =
            case tree_ of
                Leaf _ -> []
                Node lst -> List.map Tuple.second lst

        g : List (Tree row comparable) -> List (List a) -> List (List a)
        g trees prevResult =
            let
                result : List a
                result = List.concatMap applyOneLevel trees

                nextResult : List (List a)
                nextResult = result :: prevResult
            in
            case result of
                [] -> prevResult
                _ ->  g (List.concatMap getSubTrees trees) nextResult
    in
    g [tree] [] |> List.reverse


group : List (Column row comparable) -> Table row -> Tree row comparable
group cols tbl =
    let
        reduce : Column row comparable -> Tree row comparable -> Tree row comparable
        reduce col tree =
            case tree of
                Leaf tbl_ ->
                    groupByColumn col tbl_
                        |> List.map (Tuple.mapSecond Leaf)
                        |> Node

                Node lst ->
                    lst
                        |> List.map (Tuple.mapSecond (reduce col))
                        |> Node
    in
    List.foldl reduce (Leaf tbl) cols


type alias Aggregator row comparable =
    List row -> comparable


pivotTableHtml :
    { rowHeaders : List (Column row comparable1)
    , colHeaders : List (Column row comparable2)
    , aggregator : Aggregator row comparable3
    , viewRow : comparable1 -> Html msg
    , viewCol : comparable2 -> Html msg
    , viewAgg : comparable3 -> Html msg
    }
    -> Table row
    -> Html msg
pivotTableHtml { rowHeaders, colHeaders, aggregator, viewRow, viewCol, viewAgg } tbl =
    let
        indexedTable : Table ( Int, row )
        indexedTable =
            indexedMap Tuple.pair tbl

        getRowByIndex : Table row -> Int -> Maybe row
        getRowByIndex (Table lst) index =
            List.Extra.getAt index lst

        -- ignore index
        columnShim : Column row comparable -> Column ( Int, row ) comparable
        columnShim col =
            Tuple.second >> col

        rowGroup : Tree Int comparable1
        rowGroup =
            group (List.map columnShim rowHeaders) indexedTable |> mapTree Tuple.first

        colGroup : Tree Int comparable2
        colGroup =
            group (List.map columnShim colHeaders) indexedTable |> mapTree Tuple.first

        -- rowPaths : List (TreePath comparable1)
        -- rowPaths = getPaths rowGroup
        colPaths : List (TreePath comparable2)
        colPaths =
            getPaths colGroup

        viewColHeaderCells : Tree Int comparable2 -> List ( Html msg, Tree Int comparable2 )
        viewColHeaderCells tree =
            let
                f : (comparable2, Tree Int comparable2) -> ( Html msg, Tree Int comparable2 )
                f (c, subTree) = (th [ colspan <| getWidth subTree ] [ viewCol c ], subTree)
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
                            td [ colspan <| List.length rowHeaders ] []
                    in
                    if List.length rowHeaders == 0 then
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

pivotTable :
    { rowHeaders : List (Column row comparable1)
    , colHeaders : List (Column row comparable2)
    , aggregator : Aggregator row comparable3
    , viewRow : comparable1 -> Element msg
    , viewCol : comparable2 -> Element msg
    , viewAgg : comparable3 -> Element msg
    }
    -> Table row
    -> Element msg
pivotTable { rowHeaders, colHeaders, aggregator, viewRow, viewCol, viewAgg } (Table rows) =
    let
        indexedTable : Table ( Int, row )
        indexedTable = indexedMap Tuple.pair (Table rows)

        arr : Array row
        arr = Array.fromList rows

        getRowByIndex : Int -> Maybe row
        getRowByIndex index = Array.get index arr

        getIndices : Table Int -> Set Int
        getIndices (Table indices) =
            Set.fromList indices

        -- ignore index
        columnShim : Column row comparable -> Column ( Int, row ) comparable
        columnShim col =
            Tuple.second >> col

        rowGroup : Tree Int comparable1
        rowGroup =
            group (List.map columnShim rowHeaders) indexedTable |> mapTree Tuple.first

        colGroup : Tree Int comparable2
        colGroup =
            group (List.map columnShim colHeaders) indexedTable |> mapTree Tuple.first

        rowPaths : List (TreePath comparable1)
        rowPaths =
            getPaths rowGroup

        viewRowHeader : Int -> Tree Int comparable1 -> Element msg
        viewRowHeader _ tree =
            tree
                |> applyHorizontally (\(c, subTree) -> el [ width fill, height <| fillPortion <| getWidth subTree ] <| viewRow c)
                |> List.map (column [ width <| fillPortion 1, height fill ])
                |> row [ width <| fillPortion <| List.length rowHeaders, height <| fillPortion <| getWidth rowGroup ]

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
                        |> column [width <| fillPortion 1, height <| fillPortion <| h]
                Node lst ->
                    lst
                        |> List.map (\(c, subTree) -> column [width <| fillPortion <| getWidth subTree, height fill ] [ el [width fill, height fill] <| viewCol c, viewRightPart (h-1) subTree ])
                        |> row [ width fill, height fill ]
    in
    row
        [ width fill ]
        [ column [ width shrink, height fill ] [ el [ width fill, height <| fillPortion <| List.length colHeaders ] <| Element.none, el [width fill] <| viewRowHeader (List.length rowHeaders) rowGroup ]
        , el [ width shrink, height fill ] <| viewRightPart (List.length colHeaders + getWidth rowGroup) colGroup
        ]
