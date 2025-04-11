module Me.Russoul.Control.Monad.IO

namespace IO
  public export
  embed : IO a -> IO a
  embed = id
