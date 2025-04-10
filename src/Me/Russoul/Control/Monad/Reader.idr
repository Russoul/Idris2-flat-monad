module Me.Russoul.Control.Monad.Reader

import Me.Russoul.Control.Monad.Identity

public export
record Reader r a where
  constructor MkReader
  run : r -> a

public export
Functor (Reader r) where
  map g (MkReader f) = MkReader (g . f)

public export
Applicative (Reader r) where
  pure x = MkReader (\_ => x)
  MkReader f <*> MkReader g = MkReader $ \i =>
    f i (g i)

public export
read : Reader r r
read = MkReader id

namespace Identity
  public export
  embed : Identity a -> Reader r a
  embed (MkIdentity x) = pure x
