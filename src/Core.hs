{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Core where

import Card
import Stats

import Data.Function (on)
import System.Random (StdGen)
import Control.Lens (makeLenses, use, assign, zoom, _1, _2, to)
import Control.Lens.Operators
import Control.Monad.Random (runRand, MonadRandom, Rand, liftRand)
import Control.Monad (when, unless)
import Control.Monad.State (State, MonadState (..))

data Task = Number | Shape | Color deriving (Enum, Bounded, Show)

data Board = Board {
    _stimuli :: [Card],
    _response :: Card
} deriving Show

makeLenses ''Board

data CoreState = CoreState {
    _board :: Board,
    _category :: Task,
    _progress :: Int,
    _flag :: Maybe Bool,
    _lastCat :: Maybe Task,
    _gen :: StdGen,
    _time :: Int,
    _running :: Bool,
    _stats :: Stats
}

makeLenses ''CoreState

match :: Card -> Card -> Task -> Bool
match x y t = match' t x y
    where
    match' :: Task -> Card -> Card -> Bool
    match' Number = (==) `on` _number
    match' Shape = (==) `on` _shape
    match' Color = (==) `on` _color

newBoard :: MonadRandom m => m Board
newBoard = Board <$> randomCardSet <*> randomCard

firstTrial :: Rand StdGen CoreState
firstTrial = do
    board <- newBoard
    task <- randomEnum
    g <- liftRand $ \g -> (g, g)
    return $ CoreState board task 0 Nothing Nothing g 0 True (Stats 0 0 0 0 0)

onChoiceMade :: Int -> State CoreState Bool
onChoiceMade n = do
    -- match the card chosen with response card
    card <- use $ board.response
    chosen <- use $ board.stimuli.to (!!n)
    res <- match card chosen <$> use category
    pres <- maybe False (match card chosen) <$> use lastCat
    flag .= Just res

    -- update stats
    stats.trial += 1
    unless res $ stats.err += 1
    when (not res && pres) $ stats.preservation += 1

    -- draw new cards
    board <~ (zoom gen . state $ runRand newBoard)
    time .= 0

    -- make (or lose) progress
    if res
        then do
            current <- progress <+= 1
            when (current == 10) $ do
                -- new category
                stats.complete += 1
                progress .= 0
                lastCat <~ Just <$> use category
                category <~ (zoom gen . state $ runRand randomEnum)
        else
            progress .= 0

    use $ stats.complete.to (<6)
