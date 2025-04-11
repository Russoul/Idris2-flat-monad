module Main

import Me.Russoul.Control.Monad.Reader
import Me.Russoul.Control.Monad.Either
import Me.Russoul.Control.Monad.List
import Me.Russoul.Control.Monad.IO
import Me.Russoul.Control.Monad.Identity
import Me.Russoul.Control.Monad.ReaderEitherList

import Data.String

import System

public export
interface Error e where
  error : e

public export
interface Context r where
  constructor MkContext
  context : r

------------------- LIFTING ---------------------

string0 : ReaderEitherList Int String String

string1 : Either String String

string2 : Identity String

string3 : Maybe String

namespace LocalEmbedding
  public export
  Error String where
    error = "My Error"

  namespace Either
    public export
    embed : Error e => Maybe a -> Either e a
    embed (Just x) = Right x
    embed Nothing = Left error

  namespace ReaderEitherList
    public export
    embed : Error e => Maybe a -> ReaderEitherList s e a
    embed = embed . LocalEmbedding.Either.embed

string : ReaderEitherList Int String String
string = do'
  return (!string0 ++ !string1 ++ !string2 ++ !string3)

------------------- INTERPRETING ---------------------

prog0 : Either String String

prog1 : Maybe String

prog2 : ReaderEitherList Int String String

prog3 : Identity String

namespace Interpret
  namespace Either
    public export
    embed : Interpolation e => Either e a -> IO a
    embed (Left err) = die "\{err}"
    embed (Right ok) = pure ok

  namespace Maybe
    public export
    embed : Maybe a -> IO a
    embed (Just ok) = pure ok
    embed Nothing = die "Nothing"

  namespace ReaderEither
    public export
    embed : Context r => Interpolation e => ReaderEitherList r e String -> IO String
    embed t = go (t.run.run context)
     where
      go : Either e (List String) -> IO String
      go (Left err) = die "\{err}"
      go (Right str) = pure (unwords str)


prog : IO String
prog = do'
  let %hint
      context : Context Int
      context = MkContext 0
  return (!prog0 ++ !prog1 ++ !prog2 ++ !prog3)


------------------ SILLY PROGRAM ---------------------------

coprime : List Int -> ReaderEitherList Int String Int
coprime factors = do'
  limit <- read
  unless (limit > 0) $ throw "Input must be positive"
  candidate <- [1 .. limit]
  guard {f = List} (all id [(candidate `mod` factor /= 0) | factor <- factors])
  return candidate

main : IO ()
main = do'
  let %hint
      context : Context Int
      context = MkContext 100
  putStrLn !(map show $ coprime [2, 3, 5, 7])
