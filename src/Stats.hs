{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens (makeLenses)

data Stats = Stats {
    _trial :: Int,
    _complete :: Int,
    _err :: Int,
    _perseveration :: Int,
    _firstCat :: Int,
    _time :: Int,
    _errTime :: Int
}

makeLenses ''Stats
