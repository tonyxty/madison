{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Core where

import Card
import Stats

import Data.Function (on)
import System.Random (StdGen)
import Control.Lens (makeLenses, use, assign, zoom)
import Control.Lens.Operators
import Control.Monad.Random (runRand, MonadRandom, Rand, liftRand)
import Control.Monad (when, unless)
import Control.Monad.State (State, MonadState (..))

data Task = Number | Shape | Color deriving (Enum, Bounded, Show)

data Board = Board {
    _cardSet :: [Card],
    _target :: Card
} deriving Show

makeLenses ''Board

data CoreState = CoreState {
    _board :: Board,
    _task :: Task,
    _flag :: Maybe Bool,
    _lastTask :: Maybe Task,
    _gen :: StdGen,
    _time :: Int,
    _stats :: Stats
}

makeLenses ''CoreState

match :: Task -> Card -> Card -> Bool
match Number = (==) `on` _number
match Shape = (==) `on` _shape
match Color = (==) `on` _color

onChoiceMade :: MonadState CoreState m => Int -> m ()
onChoiceMade n = do
    task' <- use task
    lastTask' <- use lastTask
    card' <- use $ board.target
    cards <- use $ board.cardSet
    let chosen = cards !! n
    let res = match task' card' chosen
    flag .= Just res

    unless res $ stats.err %= (+1)
    let pres = case lastTask' of
            Just lastTask' -> match lastTask' card' chosen
            Nothing -> False
    when (not res && pres) $ stats.preservation %= (+1)

newBoard :: MonadRandom m => m Board
newBoard = Board <$> randomCardSet <*> randomCard

firstTrial :: Rand StdGen CoreState
firstTrial = do
    board <- newBoard
    task <- randomEnum
    g <- liftRand $ \g -> (g, g)
    return $ CoreState board task Nothing Nothing g 0 (Stats 0 0 0 0)

nextTrial :: State CoreState ()
nextTrial = do
    (zoom gen . state $ runRand newBoard) >>= assign board
    time .= 0
    stats.trial %= (+1)
    numTrial <- use $ stats.trial
    when (numTrial `mod` 10 == 0) $ do
        use task >>= assign lastTask . Just
        (zoom gen . state $ runRand randomEnum) >>= assign task
