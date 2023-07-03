module UI.Core where

import Core
import Stats
import UI.Card
import UI.Attr

import Brick (Widget, (<=>), (<+>), str, hBox, vBox, withAttr, padTop, Padding (..))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, hCenter)
import Control.Lens.Operators

drawCore :: CoreState -> Widget n
drawCore state = borderWithLabel (str "Wisconsin Card Sorting Test") . center . vBox $ hCenter <$> [
        hBox $ drawCard <$> state^.board.cardSet,
        drawCard $ state^.board.target,
        maybe (str ".....") flagStr $ state^.flag
    ]
    where
    flagStr :: Bool -> Widget n
    flagStr True = withAttr rightAttr $ str "RIGHT"
    flagStr False = withAttr wrongAttr $ str "WRONG"
