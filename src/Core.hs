{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Core where

import Card
import Stats

import Data.Function (on)
import Data.Fixed (Fixed(..))
import Control.Lens (makeLenses, use, assign, zoom, _1, _2, to)
import Control.Lens.Operators
import Control.Monad.Random (runRand, MonadRandom, Rand, liftRand)
import Control.Monad (when, unless)
import Control.Monad.State (State, MonadState (..))
import System.Random (StdGen)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time (nominalDiffTimeToSeconds)

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
    _timestamp :: POSIXTime,
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

firstTrial :: POSIXTime -> Rand StdGen CoreState
firstTrial t = do
    board <- newBoard
    task <- randomEnum
    g <- liftRand $ \g -> (g, g)
    return $ CoreState board task 0 Nothing Nothing g t True (Stats 0 0 0 0 0 0 0)

onChoiceMade :: Int -> POSIXTime -> State CoreState Bool
onChoiceMade n t' = do
    -- match the card chosen with response card
    card <- use $ board.response
    chosen <- use $ board.stimuli.to (!!n)
    res <- match card chosen <$> use category
    pers <- maybe False (match card chosen) <$> use lastCat
    flag ?= res

    -- draw new cards
    board <~ (zoom gen . state $ runRand newBoard)

    -- update stats
    t <- timestamp <<.= t'
    let (MkFixed pico) = nominalDiffTimeToSeconds (t' - t)
    let dt = fromIntegral $ pico `div` 1000000000
    stats.trial += 1
    stats.time += dt
    unless res $ stats.err += 1 >> stats.errTime += dt
    when (not res && pers) $ stats.perseveration += 1

    -- make (or lose) progress
    if res
        then do
            current <- progress <+= 1
            when (current == 10) $ do
                -- new category
                cat <- stats.complete <+= 1
                when (cat == 1) $ stats.firstCat <~ use (stats.trial)
                progress .= 0
                lastCat <~ Just <$> use category
                category <~ (zoom gen . state $ runRand randomEnum)
        else
            progress .= 0

    use $ stats.complete.to (<6)
