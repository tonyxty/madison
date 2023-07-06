{-# LANGUAGE TemplateHaskell #-}
module Options where

import Options.Applicative
import Control.Lens (makeLenses)

data Options = Options {
    _goalCat :: Int
}

makeLenses ''Options

goalCategory :: Parser Int
goalCategory = option auto $ long "category" <> short 'c' <>
    metavar "NUM" <> value 6 <> showDefault <> help "Number of categories to complete"

parser :: Parser Options
parser = Options <$> goalCategory

optParse :: ParserInfo Options
optParse = info parser $ fullDesc <> progDesc "Wisconsin Card Sorting Test"
