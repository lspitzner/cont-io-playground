module Control.Monad.ContIO
    -- wild-card re-exports to make things easier
  ( module Control.Monad.ContIO
  , module Control.Monad.ContIO.Class
  , module Control.Monad.Trans.Cont
  , module Control.Monad.Trans.Class
  , module Control.Monad.IO.Class
  )
where



import           Data.Function                  ( fix )
import           Control.Concurrent
import           Control.Concurrent.Chan

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Class

import           Control.Monad.ContIO.Class



forkSequential :: ContT r IO Bool
forkSequential = ContT $ \c -> c True >> c False

forkSequentialN :: Int -> ContT () IO Int
forkSequentialN n = ContT $ forM_ [1 .. n]

moveToNewThread :: ContT () IO ()
moveToNewThread = ContT $ \c -> void $ forkIO (c ())


-- this might be more efficient than default `forever` (?)
foreverCont :: ContT r m () -> ContT r m a
foreverCont a = ContT $ const $ fix $ runContT a . const

contProcessor :: Chan (ContT () IO ()) -> IO ()
contProcessor c = evalContT $ foreverCont $ join $ liftIO $ readChan c
-- contProcessor c = forever $ readChan c >>= evalContT

moveToProcessor :: Chan (ContT () IO ()) -> ContT () IO ()
moveToProcessor chan = ContT $ \c -> writeChan chan $ liftIO $ c ()


createFinalPoint :: MonadContIO m => m (a -> m a)
createFinalPoint = liftContIO $ ContT $ \c -> do
  v <- newEmptyMVar
  c $ \x -> liftContIO $ ContT $ \c2 -> putMVar v (c2 x)
  join $ takeMVar v

withLifted
  :: MonadContIO m => (forall r . (a -> IO r) -> IO r) -> (a -> m b) -> m b
withLifted withC f = do
  escape <- createFinalPoint
  a      <- liftContIO $ ContT withC
  b      <- f a
  escape b

-- | A version of `resetT` both specialized and generalized to MonadContIO.
--
-- For reference: `resetT :: (Monad m) => ContT r m r -> ContT r' m r`
--
-- resetT      is polymorphic over the things "below" in the monad stack, while
-- resetContIO is polymorphic over the things "above".
resetContIO :: MonadContIO m => m a -> m a
resetContIO m = do
  escape <- createFinalPoint
  r <- m
  escape r
-- or for people that dislike points (and readability):
-- resetCIO = (createFinalPoint >>=) . (>>=)
