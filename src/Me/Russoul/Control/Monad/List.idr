module Me.Russoul.Control.Monad.List

import Me.Russoul.Control.Monad.Identity

namespace Identity
  public export
  embed : Identity a -> List a
  embed (MkIdentity x) = pure x

namespace List
  public export
  embed : List a -> List a
  embed = id
