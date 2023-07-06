{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Core where

import Card
import Stats
import Options

import Data.Function (on)
import Data.Fixed (Fixed(..))
import Control.Lens (makeLenses, use, assign, zoom, _1, _2, to, Getter)
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
    _stats :: Stats,
    _options :: Options
}

makeLenses ''CoreState

notComplete :: Getter CoreState Bool
notComplete = to $ \s -> (s^.stats.complete) < (s^.options.goalCat)

match :: Card -> Card -> Task -> Bool
match x y t = match' t x y
    where
    match' :: Task -> Card -> Card -> Bool
    match' Number = (==) `on` _number
    match' Shape = (==) `on` _shape
    match' Color = (==) `on` _color

newBoard :: MonadRandom m => m Board
newBoard = Board <$> randomCardSet <*> randomCard

firstTrial :: POSIXTime -> Options -> Rand StdGen CoreState
firstTrial t opt = do
    board <- newBoard
    task <- randomEnum
    g <- liftRand $ \g -> (g, g)
    return $ CoreState board task 0 Nothing Nothing g t True (Stats 0 0 0 0 0 0 0) opt

onChoiceMade :: Int -> POSIXTime -> State CoreState Bool
onChoiceMade n t' = do
    -- match the card chosen with response card
    card <- use $ board.response
    chosen <- use $ board.stimuli.to (!!n)
    cat <- use category
    let res = match card chosen cat
    pers <- maybe False (match card chosen) <$> use lastCat
    flag ?= res

    -- draw new cards
    board <~ (zoom gen . state $ runRand newBoard)

    -- update stats
    t <- timestamp <<.= t'
    let dt = nominalDiffTimeToSeconds (t' - t)
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
                completed <- stats.complete <+= 1
                when (completed == 1) $ stats.firstCat <~ use (stats.trial)
                progress .= 0
                lastCat ?= cat
                category <~ (zoom gen . state $ runRand randomEnum)
        else
            progress .= 0

    use notComplete
