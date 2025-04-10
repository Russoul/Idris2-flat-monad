module Me.Russoul.Control.Monad.EitherState

import Me.Russoul.Control.Monad.Either
import Me.Russoul.Control.Monad.State
import Me.Russoul.Control.Monad.Identity

public export
record EitherState e s a where
  constructor MkEitherState
  run : s -> Either e (s, a)

public export
Functor (EitherState e s) where
  map f (MkEitherState t) = MkEitherState (map (mapSnd f) . t)

public export
Applicative (EitherState e s) where
  pure x = MkEitherState (\i => Right (i, x))
  MkEitherState f <*> MkEitherState x = MkEitherState $ \i =>
    case f i of
      Left err => Left err
      Right (i, f) => case x i of
        Left err => Left err
        Right (i, arg) => Right (i, f arg)

public export
Monad (EitherState e s) where
  MkEitherState t >>= f = MkEitherState $ \i =>
    case t i of
      Left err => Left err
      Right (i, x) => (f x).run i

namespace Identity
  public export
  embed : Identity a -> EitherState e s a
  embed (MkIdentity x) = pure x

namespace Either
  public export
  embed : Either e a -> EitherState e s a
  embed t = MkEitherState (\i => map (i,) t)

namespace State
  public export
  embed : State s a -> EitherState e s a
  embed t = MkEitherState (Right . t.run)
