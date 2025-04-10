module Main

import Me.Russoul.Control.Monad.Reader
import Me.Russoul.Control.Monad.Either
import Me.Russoul.Control.Monad.List
import Me.Russoul.Control.Monad.Identity
import Me.Russoul.Control.Monad.ReaderEitherList

err : ReaderEitherList Int String String

err' : Either String String

err'' : Identity String

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
