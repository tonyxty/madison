module UI.Card where

import Card
import UI.Attr

import Brick (Widget, str, (<=>), (<+>), withAttr, withBorderStyle, setAvailableSize)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)

drawCard :: Card -> Widget n
drawCard (Card number color shape) =
    withBorderStyle unicodeRounded . border . setAvailableSize (5, 5) .
    arrange number . withAttr (colorToAttr color) . center . str $ show shape
    where
    arrange :: Number -> Widget n -> Widget n
    arrange One w = w
    arrange Two w = w <=> w
    arrange Three w = w <=> w <=> w
    arrange Four w = (w <+> w) <=> (w <+> w)
