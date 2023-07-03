{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Card where

import Control.Lens (makeLenses)
import Control.Monad.Random.Class (MonadRandom (..))
import System.Random.Shuffle (shuffleM)

data Color = Red | Green | Yellow | Blue deriving (Enum, Bounded, Eq, Show)
data Shape = Circle | Star | Box | Cross deriving (Enum, Bounded, Eq)
data Number = One | Two | Three | Four deriving (Enum, Bounded, Eq, Show)

instance Show Shape where
    show Circle = "o"
    show Star = "*"
    show Box = "#"
    show Cross = "+"

data Card = Card {
    _number :: Number,
    _color :: Color,
    _shape :: Shape
} deriving (Eq, Show)

makeLenses ''Card

randomPermutation :: (Enum a, Bounded a, MonadRandom m) => m [a]
randomPermutation = shuffleM [minBound..maxBound]

randomEnum :: forall a m. (Enum a, Bounded a, MonadRandom m) => m a
randomEnum = toEnum <$> getRandomR (fromEnum (minBound :: a), fromEnum (maxBound :: a))

randomCard :: MonadRandom m => m Card
randomCard = Card <$> randomEnum <*> randomEnum <*> randomEnum

randomCardSet :: MonadRandom m => m [Card]
randomCardSet = zipWith3 Card [minBound..maxBound] <$> randomPermutation <*> randomPermutation
