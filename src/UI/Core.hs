module UI.Core where

import Core
import Stats
import UI.Card

import Brick (Widget, (<=>), (<+>), str, hBox, vBox)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Control.Lens.Operators

drawCore :: CoreState -> Widget n
drawCore state = borderWithLabel (str . show $ state^.stats.trial) . center . vBox $ hCenter <$> [
        hBox $ drawCard <$> state^.board.cardSet,
        drawCard $ state^.board.target,
        str $ maybe "....." flagStr (state^.flag)
    ]
    where
    flagStr :: Bool -> String
    flagStr True = "RIGHT"
    flagStr False = "WRONG"
