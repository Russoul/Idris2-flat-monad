# Idris2-flat-monad

Requires a custom idris2 build (see `pack.toml`). 

The idea of the project is to support implicit embedding of arbitrary monadic computation `t : m a` into `embed t : m' a` where
there is a canonical embedding `embed : forall a. m a -> m' a`.

The modified idris2 build supports a custom do notation, called `do'` that in desugaring phase gets desugared similar to `do` but wraps monadic computations in `embed`.
`embed` shall be an overloaded function name provided by the user (or library author) realising those canonical embeddings.

Quite important restriction placed on the monad type, for overloading resolution to work as we'd like, is `flatness`. I.e. the monad, however complex, must be a data or record name applied to the return type `a`.
E.g. `EitherState e s a` vs `EitherT e (StateT s) a`. Unless complied, resolution of ambiguous `embed` gets resolved incorrectly, as overloading resolution commits to an alternative merely by comparing the top-level spines of the target and the alternative. If the heads rigidly match, it commits without checking whether the arguments match as well.
