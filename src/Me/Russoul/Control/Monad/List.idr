module Me.Russoul.Control.Monad.List

import Me.Russoul.Control.Monad.Identity

namespace Identity
  public export
  embed : Identity a -> List a
  embed (MkIdentity x) = pure x
