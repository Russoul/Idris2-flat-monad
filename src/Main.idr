module Main

import Data.List
import Data.List.Elem

data Effect : Type where
  St : Type -> Effect
  Ex : Type -> Effect
  In : Effect

data Eff : List Effect -> Type -> Type where
  Pure : a -> Eff effects a
  Throw : e -> Eff (Ex e :: effects) a
  Get : Eff (St s :: effects) s
  Put : s -> Eff (St s :: effects) ()
  Drop : Eff effects a -> Eff (e :: effects) a
  Bind : Eff effects a -> (a -> Eff effects b) -> Eff effects b
  ReadLine : Eff (In :: effects) String

interpret : List Effect -> Type -> Type
interpret [] a = a
interpret (St s :: rest) a = s -> interpret rest (s, a)
interpret (Ex e :: rest) a = interpret rest (Either e a)
interpret (In :: rest) a = interpret rest (IO a)

return : {effects : _} -> a -> interpret effects a
return {effects = []} x = x
return {effects = St s :: effects} x = \i => return (i, x)
return {effects = Ex e :: effects} x = return (Right x)
return {effects = In :: effects} x = return (io_pure x)

get : Eff (St s :: effects) s
get = Get

put : s -> Eff (St s :: effects) ()
put = Put

throw : e -> Eff (Ex e :: effects) a
throw = Throw

readLine : Eff (In :: effects) String
readLine = ReadLine

map : {effects : _} -> (a -> b) -> Eff effects a -> Eff effects b

-- bind : {effects : _} -> Eff effects a -> (a -> Eff effects b) -> Eff effects b
-- bind {effects = []} x g = g x
-- bind {effects = St s :: effects} f g = \i => bind (f i) (uncurry $ \i, x => g x i)
-- bind {effects = Ex e :: effects} f g = bind f h
--  where
--   h : Either e a -> Eff (Ex e :: effects) b
--   h (Left x) = throw {effects = Ex e :: effects} x
--   h (Right x) = g x

interface Lift effects effects' | effects' where
  lift : Eff effects a -> Eff effects' a

(>>=) : {effects, effects' : _} -> Lift effects effects' => Eff effects a -> (a -> Eff effects' b) -> Eff effects' b
(>>=) f g = Bind (lift f) g

prog1 : Eff [Ex String] ()
prog1 = throw "crash!"

Lift [] [] where
  lift = id

Lift xs ys => Lift (x :: xs) (x :: ys) where
  lift = ?fffff

Lift [x] (x :: xs) where
  lift = ?fff

Lift effects effects' => Lift effects (x :: effects') where
  lift = Drop . lift

{h : Effect} -> {effects : _} -> Lift effects (h :: effects) where
  lift x = Drop x

prog2 : Eff [St Int, Ex String, In] ()
prog2 = do
  x <- prog1
  i <- get
  l <- readLine
  ?ff

main : IO ()
main = putStrLn "Hello from Idris2!"
