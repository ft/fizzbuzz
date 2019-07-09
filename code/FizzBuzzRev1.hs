module Main where
import System.Environment

(⊘) :: Integral a => a -> a -> Bool
(⊘) n m = n `mod` m == 0

data Command = Skip | Halt | Print String
  deriving Show

type Program = [Command]

step :: Command -> String -> String
step Skip t      = t
step Halt _      = ""
step (Print s) t = s ++ t

eval :: Program -> String
eval = foldr step ""

type Context = Program -> Program

base :: (Integral a, Show a) => a -> Context
base n = \x -> x ++ [ Print(show n) ]

zz :: Integral a => a -> String -> a -> Context
zz m s n | n ⊘ m = \x -> [Print s] ++ x ++ [Halt]
         | otherwise = id

fizz, buzz :: Integral a => a -> Context

fizz = zz 3 "fizz"
buzz = zz 5 "buzz"

fb :: (Integral a, Show a) => a -> Program
fb n = (base n . fizz n . buzz n) [Skip]

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz n = eval $ fb n

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
