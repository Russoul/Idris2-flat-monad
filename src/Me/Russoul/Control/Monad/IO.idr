module Me.Russoul.Control.Monad.IO

import Me.Russoul.Control.Monad.Identity

namespace Identity
  public export
  embed : Identity a -> IO a
  embed = io_pure . run

namespace IO
  public export
  embed : IO a -> IO a
  embed = id
