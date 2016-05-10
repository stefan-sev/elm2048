module GameModel where

import Utils exposing ((!), transpose, zip, zipWith, transformMaybe, initSeed)
import List exposing (..)
import Maybe exposing (..)
import Random exposing (Seed)

{--MODEL--}

-- this show if Tile has a Number of is Empty
type Tile = Number Int | Empty

-- this is the board
type alias Grid = List (List Tile)

-- this is the state of the game
type Progress = InProgress | GameOver | Won

type alias GameState = 
    { grid: Grid
    , score: Int 
    , initSeed: Seed
    , gameProgress: Progress
    }

gridSize : Int
gridSize = 4

emptyGrid : Grid
emptyGrid = repeat gridSize <| repeat gridSize <| Empty

defaultGame : GameState 
defaultGame = { 
      grid = emptyGrid
    , score = 0
    , initSeed = initSeed
    , gameProgress = InProgress
    }

readTile : (Int, Int) -> Grid -> Tile
readTile (x, y) g = 
    let
        withMaybe = andThen (g ! y) (\a -> a ! x) 
    in
        case withMaybe of
            Just t ->
                t
            Nothing ->
                Empty

setTile : (Int, Int) -> Grid -> Tile -> Grid
setTile (x,y) grid tile = 
    let 
        row = grid ! y
        newRow = case row of
            Just r -> 
                (List.take x r) ++ [tile] ++ (List.drop (x+1) r)
            Nothing -> 
                [Empty]
    in
        case newRow of
            [Empty] ->
                grid
            xs ->
                (take y grid) ++ [xs] ++ (drop (y+1) grid)

tileToInt : Tile -> Int
tileToInt tile = 
    case tile of
        Number i -> i
        Empty -> 0

intToTile : Int -> Tile
intToTile i = 
    case i of
        0 -> Empty
        _ -> Number i
        
tilesWithCoordinates : Grid -> List (Tile, Int, Int)
tilesWithCoordinates grid = grid 
    |> List.map (\r -> zip r [0..(gridSize-1)]) 
    |> zipWith (\j r -> List.map (\(t,i) -> (t,i,j)) r) [0..(gridSize-1)]
    |> concat


rotateGrid : Grid -> Grid
rotateGrid grid =
    let 
        maybeGrid = List.map (List.map (\x -> Just x)) grid
        transposedgrid = transpose maybeGrid 
        reconvertedgrid = 
            List.map (List.map (\x -> 
                case x of
                    Just x -> 
                        x
                    Nothing ->
                        Empty
                )
            ) transposedgrid
    in
        List.map reverse <| reconvertedgrid

