module Main (main) where

import Graphmod (graphmod)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= graphmod
