module Me.Russoul.Control.Monad.Identity

public export
record Identity (0 a : Type) where
  constructor MkIdentity
  run : a

public export
return : a -> Identity a
return = MkIdentity

namespace Identity
  public export
  embed : Identity a -> Identity a
  embed = id
