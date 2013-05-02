module Main where

import System.Environment

import FrontEnd as FE
import SemanticsAnalysis as SA
import Plot as PLT

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  src <- readFile filename
  let t = FE.parseInterproc src
  --putStrLn $ show t
  let outFileDot = filename ++ ".dot"
  PLT.genDotFile outFileDot t
  newT <- SA.aiProcess t
  putStrLn $ show newT
  let outFile = filename ++ ".certified"
  writeFile outFile (show newT)
  

