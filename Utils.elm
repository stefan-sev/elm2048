module Utils where

import List exposing (..)
import Maybe exposing (..)
import Random 

infixr 9 !

(!) : List a -> Int -> Maybe a
(!) l n =
    case (l,n) of
        ((x::xs),0) -> Just x
        ((x::xs),n) -> xs ! (n - 1)
        _ -> Nothing

transpose : List (List (Maybe a)) -> List (List (Maybe a))
transpose ls = case ls of
    ([]::xs) -> []
    _ -> (List.map ownListHead ls) :: transpose (List.map (transformMaybe << List.tail) ls)



transformMaybe : Maybe (List a) -> (List a)
transformMaybe i =
    case i of
        Nothing -> []
        Just i -> i

ownListHead : List(Maybe a) -> Maybe a
ownListHead ls =
    case ls of
        (Nothing :: xs) -> Nothing
        (Just x :: xs) -> Just x
        [] -> Nothing


zip : List a -> List b -> List (a,b)
zip xs ys =
    case (xs, ys) of
        (x :: xs', y :: ys') ->
            (x, y) :: zip xs' ys'
        (_ , _) ->
            []

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith func xs ys =
    case (xs, ys) of
        (x :: xs', y :: ys') ->
            (func x y) :: zipWith func xs' ys'
        (_ , _) ->
            []

and : List Bool -> Bool
and l =
    case l of
        [] ->
            True
        (x::xs) ->
            x && (and xs)

initSeed : Random.Seed
initSeed = Random.initialSeed 42
    
randomGenerator : Random.Generator Float
randomGenerator = Random.float 0 1

listRandom : Int -> Random.Seed -> List Float 
listRandom i s =  
    case i of
        0 -> []
        _ -> 
            let 
                (n, newSeed) = Random.generate randomGenerator s
            in
                n :: (listRandom (i - 1) newSeed)
