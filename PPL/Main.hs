
module Main where

import HaskellPPL



main :: IO ()
main = withPPL $
       do { p1 <- polyhedron_from_consys cs1
          ; p2 <- polyhedron_from_consys cs2
          ; polyhedron_hull p1 p2
          ; cs' <- consys_from_polyhedron p1
          ; putStrLn $ "Convex Hull of " ++ show cs1 ++ " and " ++ show cs2
          ; print cs'
          ; p3 <- polyhedron_from_consys cs3
          ; putStrLn $ "Widening of " ++ show cs3 ++ " and " ++ show cs'
          ; polyhedron_h79_widening p3 p1
          ; cs'' <- consys_from_polyhedron p3
          ; print cs''
          }
    where cs1 = [x %== 1] :: LinConSys -- [x%==0, y%==1]
          cs2 = [x %== 1] :: LinConSys -- [x%==1, y%==3]
          cs3 = [] :: LinConSys -- [1+2*x%==y, 1%<=y, y%<=4]
          x = var 0
          y = var 1


