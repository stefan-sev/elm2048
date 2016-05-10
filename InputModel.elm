module InputModel where 

import Signal exposing (..)
import Keyboard
import Random

type Direction = Up | Down | Right | Left | None

type alias Controls = 
    {tilePushDirection: Direction
    ,newGameButtonPressed: Bool
    }

type alias Keys = { x:Int, y:Int }

playerDirection : Signal Direction
playerDirection = 
        Signal.merge (Signal.map convertDirection Keyboard.arrows) (Signal.map convertDirection Keyboard.wasd)

       

convertDirection : Keys -> Direction
convertDirection keys =
    if keys.x == 0 && keys.y == 1 then
        Up
    else if  keys.x == 0 && keys.y == (-1) then
        Down
    else if  keys.x == 1 && keys.y == 0 then 
        Right
    else if   keys.x == (-1) && keys.y == 0 then 
        Left 
    else  
        None

generatorFloat : Random.Generator Float
generatorFloat = Random.float 0 1

startSeed : Random.Seed
startSeed = Random.initialSeed 9


