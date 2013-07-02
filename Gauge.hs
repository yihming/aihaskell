module Gauge where

import Data.List

{- Define the data structures. -}

data Z = Nat Int
       | Top
instance Show Z where
  show (Nat n) = show n
  show Top = "⊤"

data Gauge = Gauge ([Int], [Int])
instance Show Gauge where
  show (Gauge ([], _)) = error "The lower gauge cannot be empty!"
  show (Gauge (_, [])) = error "The upper gauge cannot be empty!"
  show (Gauge (lg@(x:xs), ug@(y:ys))) = 
    "[ " ++ show x ++ f xs 1 ++ " , " ++ show y ++ f ys 1 ++ " ]"
    where
      f :: [Int] -> Int -> String
      f [] _ = ""
      f (x:xs) n 
        | x > 0  = " + " ++ show x ++ " λ" ++ show n ++ f xs (n+1)
        | x == 0 = f xs (n+1)
        | x < 0  = " - " ++ show (-x) ++ " λ" ++ show n ++ f xs (n+1)

type Section = [(Int, Z)]

data RWInt = Value Int
           | PlusInf
           | MinusInf
instance Show RWInt where
  show (Value n) = show n
  show PlusInf = "+∞"
  show MinusInf = "-∞"

data Interval = Interval (RWInt, RWInt)
instance Show Interval where
  show (Interval (a, b)) = "[ " ++ show a ++ " , " ++ show b ++ " ]"

type Var = [Char]

data Environment = Env [(Var, Gauge)] deriving Show

type GaugeSection = (Section, Environment)

type LoopCounterDom = [(Int, Interval)]

type GaugeDomain = (GaugeSection, LoopCounterDom)

{- Gauge Operations  -}