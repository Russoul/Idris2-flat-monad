module Me.Russoul.Control.Monad.ReaderEitherList

import Me.Russoul.Control.Monad.Reader
import Me.Russoul.Control.Monad.Either
import Me.Russoul.Control.Monad.List
import Me.Russoul.Control.Monad.Identity

public export
record ReaderEitherList r e a where
  constructor MkReaderEitherList
  run : Reader r (Either e (List a))

export
Functor (ReaderEitherList r e) where
  map f (MkReaderEitherList (MkReader underlying)) = MkReaderEitherList $ MkReader $ \x =>
    let t = underlying x in
    map (map f) t

export
Applicative (ReaderEitherList r e) where
  pure x = MkReaderEitherList (pure (pure (pure x)))
  MkReaderEitherList (MkReader f) <*> MkReaderEitherList (MkReader x) = MkReaderEitherList (MkReader $ \i => f i <*> x i)
    where
     (<*>) : Either e (List (a -> b)) -> Either e (List a) -> Either e (List b)
     f <*> x = map (Prelude.(<*>) {f = List}) f `Prelude.(<*>)` x

export
Monad (ReaderEitherList r e) where
  MkReaderEitherList (MkReader f) >>= g = MkReaderEitherList $ MkReader $ \i =>
    let t = f i in
    case t of
      Left err => throw err
      Right ok => go i ok
       where
        go : r -> List a -> Either e (List b)
        go i [] = Right []
        go i (x :: xs) =
          let a = (g x).run.run i in
          let b = go i xs in
          map (++) a <*> b

namespace Reader
  export
  embed : Reader r a -> ReaderEitherList r e a
  embed = MkReaderEitherList . map (pure . pure)

namespace Either
  export
  embed : Either e a -> ReaderEitherList r e a
  embed = MkReaderEitherList . pure . map pure

namespace List
  export
  embed : List a -> ReaderEitherList r e a
  embed = MkReaderEitherList . pure . pure

namespace Identity
  export
  embed : Identity a -> ReaderEitherList r e a
  embed = MkReaderEitherList . pure . pure . pure . run

namespace ReadEitherList
  export
  embed : ReaderEitherList r e a -> ReaderEitherList r e a
  embed = id
