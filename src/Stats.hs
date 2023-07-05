{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Data.Fixed (Pico)
import Control.Lens (makeLenses)

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
