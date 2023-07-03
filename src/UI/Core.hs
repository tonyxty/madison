module UI.Core where

import Core
import UI.Card

import Brick (Widget, (<=>), (<+>), hBox, str, strWrap)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center, vCenter)
import Control.Lens.Operators

drawCore :: CoreState -> Widget n
drawCore state = borderWithLabel (str . show $ state^.trial) . center $
    hBox (drawCard <$> state^.board.cardSet)
    <=>
    (drawFlag' (state^.flag) . drawCard $ state^.board.target)

    where

    drawFlag :: Bool -> Widget n
    drawFlag x = str $ if x then "RIGHT" else "WRONG"

    drawFlag' :: Maybe Bool -> Widget n -> Widget n
    drawFlag' Nothing = id
    drawFlag' (Just x) = (<+> drawFlag x)
