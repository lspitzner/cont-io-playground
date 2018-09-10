{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ContIO.Class where



import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Class



class MonadIO m => MonadContIO m where
  liftContIO :: ContT () IO a -> m a

instance MonadContIO (ContT () IO) where
  liftContIO = id

-- eeeevil monadtrans-based instance
instance {-# OVERLAPPABLE #-} (MonadIO (t m), MonadTrans t, MonadContIO m) => MonadContIO (t m) where
  liftContIO = lift . liftContIO

