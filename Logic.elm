module Logic where

import InputModel exposing (..)

import GameModel exposing
    ( GameState
    , Progress(..)
    , Tile(..)
    , Grid
    , gridSize
    , emptyGrid
    , defaultGame
    , readTile
    , setTile
    , tileToInt
    , intToTile
    , tilesWithCoordinates
    , rotateGrid
    )

import Utils exposing (..)
import List exposing (..)
import Random

groupedByTwo : List a -> List (List a) 
groupedByTwo l = 
    case l of
        (x::y::xs) ->
            if x == y then
                [x,y] :: (groupedByTwo xs)
            else
                [x] :: (groupedByTwo (y::xs))
        (x::[]) ->
            [[x]]
        [] ->
            [[]]

slideRow : List Tile -> (List Tile, Int)
slideRow tilerow =
    let 
        grouped = 
            groupedByTwo 
                <| filter (\t -> t /= Empty) tilerow
        lastelem = 
            head <| reverse grouped
        groupedWithoutempty = 
            case lastelem of
                Just [] ->
                    Maybe.map reverse <| tail <| reverse grouped 
                Just _ ->
                    Just grouped 
                Nothing ->
                    Nothing
        slide ly = 
            take gridSize
                <| (List.map (intToTile << sum << (map tileToInt)) ly)
                    ++ (repeat gridSize Empty)
        count lyy = 
            sum << (map tileToInt)
                <| concat
                <| filter (\x -> (length x) > 1) lyy
    in
        case groupedWithoutempty of
            Just x ->
                (slide x, count x)
            Nothing -> 
                (tilerow, 0)

slideGrid : Direction -> Grid -> (Grid, Int)
slideGrid dir grid = 
    let 
        rotatedGrid = 
            case dir of
                Down -> rotateGrid grid
                Right -> (rotateGrid << rotateGrid) grid
                Up -> (rotateGrid << rotateGrid << rotateGrid) grid
                otherwise -> grid
        
        rowsWithScores = List.map slideRow <| rotatedGrid

        slidRotatedGrid = List.map fst rowsWithScores
        scoreGained = sum <| List.map snd rowsWithScores

        slidGrid = 
            case dir of
                Up -> rotateGrid slidRotatedGrid
                Right -> (rotateGrid << rotateGrid) slidRotatedGrid
                Down -> (rotateGrid << rotateGrid << rotateGrid) slidRotatedGrid
                otherwise -> slidRotatedGrid
    in
        (slidGrid, scoreGained)

slideGameState : Controls -> GameState -> GameState
slideGameState controls gameState =
    let
        newGridScore = slideGrid controls.tilePushDirection gameState.grid
    in
        if fst newGridScore == gameState.grid then
            gameState 
        else
            { gameState |
                grid  = fst newGridScore
              , score = gameState.score + snd newGridScore  
            }

gameWon : Grid -> Bool
gameWon grid =
    let
        gameSt = concat grid
            |> filter (\t -> t == Number 2048)
            |> length
    in
        0 /= gameSt

gameLost : Grid -> Bool
gameLost grid =
    let 
        up = fst <| slideGrid Up grid
        down = fst <| slideGrid Down grid
        left = fst <| slideGrid Left grid
        right = fst <| slideGrid Right grid
    in
        and
        [ grid /= emptyGrid
        , up == down
        , down  == left
        , left == right 
        , right == grid
        ]

win : GameState -> GameState
win gameState =
    { gameState |
        gameProgress = Won
    }

lose : GameState -> GameState
lose gameState =
    { gameState |
        gameProgress = GameOver
    }
    
probability2 : Float
probability2 = 0.9

newTile : Float -> Tile
newTile x = 
    if x < probability2 then
        Number 2
    else 
        Number 4

emptyTiles : Grid -> List (Int, Int)
emptyTiles grid =
    tilesWithCoordinates grid
        |> filter (\(t,_,_) -> t == Empty)
        |> List.map (\(_,x,y) -> (x,y))

newTileCoordinate : Float -> Grid -> Maybe (Int, Int)
newTileCoordinate random grid =
    let 
        emptyTileIndices = emptyTiles grid
    in
        case emptyTileIndices of
            [] ->
                Nothing
            otherwise ->
                    emptyTileIndices !
                    (floor    <|
                    (toFloat <| 
                        List.length emptyTileIndices) * random)

placeRandomTile : GameState -> GameState
placeRandomTile gameState =
    let 
        (rand1, seed1) = Random.generate randomGenerator gameState.initSeed
        (rand2, newSeed2) = Random.generate randomGenerator seed1              
        tileIndex = newTileCoordinate rand1 gameState.grid
        newT   = newTile rand2
    in
        case tileIndex of
            Nothing ->
                { gameState |
                    initSeed = newSeed2
                }
            Just index ->
                { gameState |
                    initSeed = newSeed2
                  , grid = setTile index gameState.grid newT
                }

newGame : GameState
newGame =
    placeRandomTile <| placeRandomTile defaultGame

stepGame : Controls -> GameState -> GameState
stepGame controls gameState =
    if controls.newGameButtonPressed then
        newGame 
    else if (gameState.gameProgress /= InProgress) then
        gameState
    else if (gameWon gameState.grid) then 
        win gameState
    else if (gameLost gameState.grid) then
        lose gameState
    else if (controls.tilePushDirection /= None) then
        let
            pushedState = slideGameState controls gameState
        in
            if (pushedState == gameState) then 
                gameState
            else
                placeRandomTile pushedState
    else
        gameState

