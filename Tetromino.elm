module Tetromino exposing (..)

import Html exposing (..)
import Basics exposing (..)
import Color exposing (Color)
import Collage exposing (..)
import Element exposing (..)
import List
import Block exposing (Block)


type alias Location =
    ( Int, Int )


type alias Tetromino =
    { shape : List Location
    , block : Block
    , pivot : { r : Float, c : Float }
    , rows : Int
    , cols : Int
    }


toForm : Tetromino -> Form
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate ( row, col ) =
            move
                ( (toFloat col) * Block.size
                , (toFloat row) * Block.size
                )
                form

        forms =
            List.map translate shape
    in
        group forms



-- Long I


i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , block = Block Color.lightOrange
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 4
    , cols = 1
    }


j : Tetromino



-- Two Ls


j =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, -1 )
        , ( -1, 0 )
        ]
    , block = Block Color.lightGreen
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


drawPivot : Tetromino -> Form
drawPivot { pivot } =
    let
        dot =
            circle 5 |> filled Color.black

        translate =
            move ( pivot.c * Block.size, pivot.r * Block.size )
    in
        translate dot


rotateLocation : { r : Float, c : Float } -> Float -> Location -> Location
rotateLocation pivot angle ( row, col ) =
    let
        rowOrigin =
            (toFloat row) - pivot.r

        colOrigin =
            (toFloat col) - pivot.c

        ( s, c ) =
            ( sin (angle), cos (angle) )

        rowRotated =
            rowOrigin * c - colOrigin * s

        colRotated =
            rowOrigin * s + colOrigin * c

        ( newRow, newCol ) =
            ( round <| rowRotated + pivot.r, round <| colRotated + pivot.c )
    in
        ( newRow, newCol )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        rotateHelper =
            rotateLocation tetromino.pivot (degrees 90)

        newShape =
            List.map rotateHelper tetromino.shape
    in
        { tetromino
            | shape = newShape
            , rows = tetromino.cols
            , cols = tetromino.rows
        }



--
-- jr : Tetromino
-- jr =
--     { shape =
--         [ ( 1, 0 )
--         , ( 0, 0 )
--         , ( -1, 1 )
--         , ( -1, 0 )
--         ]
--     , block = Block Color.lightBlue
--     }
--
--
--
-- -- S
--
--
-- s =
--     { shape =
--         [ ( 0, 0 )
--         , ( 0, 1 )
--         , ( -1, 1 )
--         , ( -1, 2 )
--         ]
--     , block = Block Color.lightPurple
--     }
--
--
-- sr : Tetromino
-- sr =
--     { shape =
--         [ ( 0, 2 )
--         , ( 0, 1 )
--         , ( -1, 1 )
--         , ( -1, 0 )
--         ]
--     , block = Block Color.darkYellow
--     }
--
--
--
-- -- Box
--
--
-- o : Tetromino
-- o =
--     { shape =
--         [ ( 1, 0 )
--         , ( 1, 1 )
--         , ( 0, 1 )
--         , ( 0, 0 )
--         ]
--     , block = Block Color.lightRed
--     }


tetromino : Tetromino
tetromino =
    rotate <| rotate <| rotate <| rotate <| rotate <| rotate j


main : Html msg
main =
    collage 400 400 [ toForm tetromino, drawPivot tetromino ] |> toHtml
