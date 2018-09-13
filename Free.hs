import Control.Monad.Trans
import Control.Monad (liftM)

data FreeF f a x = Pure a | Free (f x)

newtype FreeT f m a =
    FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

instance (Functor f, Monad m) => Monad (FreeT f m) where
    return a = FreeT (return (Pure a))
    FreeT m >>= f = FreeT $ m >>= \v -> case v of
        Pure a -> runFreeT (f a)
        Free w -> return (Free (fmap (>>= f) w))

instance MonadTrans (FreeT f) where
    lift = FreeT . liftM Pure

liftF :: (Functor f, Monad m) => f r -> FreeT f m r
liftF x = FreeT (return (Free (fmap return x)))
