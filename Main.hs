module Main where

import System.Environment

--import FrontEnd as FE
import HaskellPPL as P

main :: IO ()
{-
main = do
  args <- getArgs
  let filename = head args
  src <- readFile filename
  let t = FE.parseInterproc src
  putStrLn $ show t
-}

main = P.withPPL $
       do { p1 <- P.polyhedron_from_consys cs1
          ; p2 <- P.polyhedron_from_consys cs2
          ; P.polyhedron_hull p1 p2
          ; cs' <- P.consys_from_polyhedron p1
          ; putStrLn $ "Convex Hull of " ++ show cs1 ++ " and " ++ show cs2
          ; print cs'
          ; p3 <- P.polyhedron_from_consys cs3
          ; putStrLn $ "Widening of " ++ show cs3 ++ " and " ++ show cs'
          ; P.polyhedron_h79_widening p3 p1
          ; cs'' <- P.consys_from_polyhedron p3
          ; print cs''
          }
    where cs1 = [x %== 1] :: P.LinConSys -- [x%==0, y%==1]
          cs2 = [x %== 1] :: P.LinConSys -- [x%==1, y%==3]
          cs3 = [] :: P.LinConSys -- [1+2*x%==y, 1%<=y, y%<=4]
          x = var 0
          y = var 1
