module Main

import Me.Russoul.Control.Monad.Reader
import Me.Russoul.Control.Monad.Either
import Me.Russoul.Control.Monad.List
import Me.Russoul.Control.Monad.Identity
import Me.Russoul.Control.Monad.ReaderEitherList

string0 : ReaderEitherList Int String String

string1 : Either String String

string2 : Identity String

compose : ReaderEitherList Int String String
compose = do'
  return (!string0 ++ !string1 ++ !string2)

factors : List Int -> ReaderEitherList Int String Int
factors ks = do'
  n <- read
  unless (n > 0) $ throw "Input must be positive"
  x <- [1 .. n]
  guard {f = List} (all id [(x `mod` k /= 0) | k <- ks])
  return x

prog : ?
prog = (factors [2, 3, 5, 7]).run.run 100

main : IO ()
main = putStrLn "OK"
