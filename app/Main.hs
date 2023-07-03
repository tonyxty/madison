module Main where

import UI.App (appMain)
import Stats (Stats (..))

main :: IO ()
main = do
    stats <- appMain
    putStrLn "=== REPORT ==="
    putStrLn $ "total trials: " ++ show (_trial stats)
    putStrLn $ "total errors: " ++ show (_err stats)
    putStrLn $ "preservation errors: " ++ show (_preservation stats)
