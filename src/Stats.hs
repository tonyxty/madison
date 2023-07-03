{-# LANGUAGE TemplateHaskell #-}
module Stats where

import Control.Lens (makeLenses)

data Stats = Stats {
    _totalTrial :: Int,
    _err :: Int,
    _preservation :: Int,
    _tle :: Int
}

makeLenses ''Stats
