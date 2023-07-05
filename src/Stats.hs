{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Data.Fixed (Pico)
import Control.Lens (makeLenses, Getter, to, (^.))

data Stats = Stats {
    _trial :: Int,
    _complete :: Int,
    _err :: Int,
    _perseveration :: Int,
    _firstCat :: Int,
    _time :: Pico,
    _errTime :: Pico
}

makeLenses ''Stats

correct :: Getter Stats Int
correct = to $ \s -> s^.trial - s^.err

correctTime :: Getter Stats Pico
correctTime = to $ \s -> s^.time - s^.errTime

nonPerseveration :: Getter Stats Int
nonPerseveration = to $ \s -> s^.err - s^.perseveration
