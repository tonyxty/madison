module UI.Report where

import Core
import Stats

import Brick (Widget, str, strWrap, hLimit, vBox, padLeft, Padding (Pad), padRight, BrickEvent (..), EventM, halt)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table (table, surroundingBorder, alignRight, columnBorders, renderTable)
import Graphics.Vty (Event(..), Key (..))
import Data.List (transpose)
import Control.Lens.Operators
import Numeric (showFFloat)

handleReportEvent :: BrickEvent n e -> EventM n CoreState ()
handleReportEvent (VtyEvent (EvKey KEsc [])) = halt
handleReportEvent _ = return ()

drawReport :: Stats -> Widget n
drawReport stats = borderWithLabel (str "Report") . center . hLimit 80 .
    renderTable . surroundingBorder False . columnBorders False . alignRight 2 . table $ contents
    where
    contents :: [[Widget n]]
    contents = transpose [str <$> identifiers, padLeft (Pad 2) . padRight (Pad 5) . str <$> descriptions, str <$> values ]

    identifiers = ["cc", "rt", "ra", "rc", "re", "rep", "rct", "ret", "rf", "rpe", "rpep", "nrpe", "nrepep"]
    descriptions = [
            "Completed categories:",
            "Total response time:",
            "Total number of responses:",
            "Total number of correct responses:",
            "Total number of erroneous responses:",
            "Erroneous response percentage:",
            "Correct response time:",
            "Erroneous response time:",
            "Responses to complete first category:",
            "Perseverative errors:",
            "Perseverative error percentage:",
            "Non-perseverative errors:",
            "Non-perseverative error percentage:"
        ]
    values = [
            show (stats^.complete),
            showTime (stats^.time),
            show (stats^.trial),
            show (stats^.trial - stats^.err),
            show (stats^.err),
            percentage (stats^.err) (stats^.trial),
            showTime (stats^.time - stats^.errTime),
            showTime (stats^.errTime),
            show (stats^.firstCat),
            show (stats^.perseveration),
            percentage (stats^.perseveration) (stats^.trial),
            show (stats^.err - stats^.perseveration),
            percentage (stats^.err - stats^.perseveration) (stats^.trial)
        ]

    percentage :: Integral a => a -> a -> String
    percentage x y = showFFloat (Just 2) (fromIntegral x / fromIntegral y * 100.0) "%"

    showTime :: Int -> String
    showTime x = shows (x `div` 1000) $ '.' : shows (x `mod` 1000) "s"
