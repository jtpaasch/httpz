module Main (main) where

import qualified App.Application as App

main :: IO ()
main = do
  result <- App.get ()
  -- result <- App.getWithParams ()
  -- result <- App.post ()
  -- result <- App.put ()
  -- result <- App.delete ()
  case result of
    Left e -> putStrLn e
    Right r -> putStrLn r
