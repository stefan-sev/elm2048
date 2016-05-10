-- Elm 2048
module Elm2048 where 

import InputModel exposing ( Controls
                           , playerDirection
                           , Direction (..)) 

import GameModel exposing (defaultGame, GameState)
import Logic exposing (stepGame)
import Rendering2048 exposing (displayGameState)
import Signal 

main = Signal.map displayGameState gameState

port score : Signal Int
port score =
    Signal.map (\x -> x.score) gameState
                
port newGameButton : Signal Bool

controls : Signal Controls
controls = Signal.map2 setControls playerDirection newGameButton

gameState : Signal GameState
gameState = Signal.foldp stepGame defaultGame controls

setControls : Direction -> Bool -> Controls
setControls d b =
    { tilePushDirection = d
    , newGameButtonPressed = b
    }
