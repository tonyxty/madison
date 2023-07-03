module UI.Report where

import Stats

import Brick (Widget, str, strWrap, hLimit, vBox)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Control.Lens.Operators

drawReport :: Stats -> Widget n
drawReport stats = borderWithLabel (str "Report") . center . hLimit 80 . vBox $ strWrap <$> [
        "Completed categories: " ++ show (stats^.complete),
        "Total number of trials: " ++ show (stats^.trial),
        "Total number of errors: " ++ show (stats^.err),
        "Preservation errors: " ++ show (stats^.preservation)
    ]
