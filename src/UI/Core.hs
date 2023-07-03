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
        hBox $ drawCard <$> state^.board.stimuli,
        drawCard $ state^.board.response,
        maybe (str ".....") flagStr $ state^.flag,
        padTop Max $ str "Press q to finish the test and see the report.  Press Esc to quit."
    ]
    where
    flagStr :: Bool -> Widget n
    flagStr True = withAttr rightAttr $ str "RIGHT"
    flagStr False = withAttr wrongAttr $ str "WRONG"
