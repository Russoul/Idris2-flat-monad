module Me.Russoul.Control.Monad.State

import Me.Russoul.Control.Monad.Identity

public export
record State s a where
  constructor MkState
  run : s -> (s, a)

public export
Functor (State s) where
  map g (MkState f) = MkState (mapSnd g . f)

public export
Applicative (State s) where
  pure x = MkState $
    \i => (i, x)

  MkState f <*> MkState g = MkState $ \i =>
    let (i, f) = f i in
    let (i, x) = g i in
    (i, f x)

public export
Monad (State s) where
  MkState f >>= g = MkState $ \i =>
    let (i, x) = f i in
    let MkState x = g x in
    x i

public export
get : State s s
get = MkState dup

public export
put : s -> State s ()
put i = MkState (const (i, ()))

public export
modify : (s -> s) -> State s ()
modify f = MkState (\i => (f i, ()))

namespace Identity
  public export
  embed : Identity a -> State s a
  embed (MkIdentity x) = pure x

namespace State
  public export
  embed : State s a -> State s a
  embed = id
