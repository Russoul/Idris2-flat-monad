module Me.Russoul.Control.Monad.Either

import Me.Russoul.Control.Monad.Identity

public export
throw : e -> Either e a
throw = Left

namespace Identity
  public export
  embed : Identity a -> Either e a
  embed (MkIdentity x) = pure x

