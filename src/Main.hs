module Main (main) where

{-

| The main entry point into the progam.

-}

import qualified App.Application as App

main :: IO ()
main = do

  -- Try 'em all (uncomment the one you want to try):
  result <- App.get ()
  -- result <- App.getWithParams ()
  -- result <- App.post ()
  -- result <- App.put ()
  -- result <- App.delete ()

  -- Print the result (or the error).
  case result of
    Left e -> putStrLn e
    Right r -> putStrLn r
