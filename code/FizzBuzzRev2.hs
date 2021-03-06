module Main where
import Prelude hiding (print)
import System.Environment

(⊘) :: Integral a => a -> a -> Bool
(⊘) n m = n `mod` m == 0

type Program = String -> String

skip, halt :: Program
skip = id
halt = const ""

print :: String -> Program
print = (++)

type Context = Program -> Program

base :: (Integral a, Show a) => a -> Context
base n = \x -> x . print (show n)

zz :: Integral a => a -> String -> a -> Context
zz m s n | n ⊘ m = \x -> print s . x . halt
         | otherwise = id

fizz, buzz :: Integral a => a -> Context

fizz = zz 3 "fizz"
buzz = zz 5 "buzz"

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz n = (base n . fizz n . buzz n) skip ""

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
