module Main where

import System.Environment

import FrontEnd as FE

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  src <- readFile filename
  let t = FE.parseInterproc src
  putStrLn $ show t