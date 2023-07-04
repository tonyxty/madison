module UI.Report where

import Stats

import Brick (Widget, str, strWrap, hLimit, vBox, padLeft, Padding (Pad), padRight)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table (table, surroundingBorder, alignRight, columnBorders, renderTable)
import Control.Lens.Operators
import Data.List (transpose)
import Numeric (showFFloat)

drawReport :: Stats -> Widget n
drawReport stats = borderWithLabel (str "Report") . center . hLimit 80 . renderTable .
    surroundingBorder False . columnBorders False . alignRight 2 . table $ contents
    where
    contents :: [[Widget n]]
    contents = transpose [str <$> identifiers, padLeft (Pad 2) . padRight (Pad 5) . str <$> descriptions, str <$> values ]

    identifiers = ["cc", "ra", "rc", "re", "rep", "rpe"]
    descriptions = [
            "Completed categories:",
            "Total number of responses:",
            "Total number of correct responses:",
            "Total number of erroneous responses:",
            "Erroneous response percentage:",
            "Perseverative errors:"
        ]
    values = [
            show (stats^.complete),
            show (stats^.trial),
            show (stats^.trial - stats^.err),
            show (stats^.err),
            percentage (stats^.err) (stats^.trial),
            show (stats^.perseveration)
        ]

    percentage :: Integral a => a -> a -> String
    percentage x y = showFFloat (Just 2) (fromIntegral x / fromIntegral y * 100.0) "%"
