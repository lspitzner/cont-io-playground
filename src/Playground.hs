module Playground where



import           Control.Monad.ContIO
import           Control.Monad.Trans.State.Strict



mockWith :: String -> (() -> IO a) -> IO a
mockWith s c = do
  putStrLn $ "aquire " ++ s
  x <- c ()
  putStrLn $ "release " ++ s
  pure x


testWithLifted1 :: IO ()
testWithLifted1 = evalContT $ flip evalStateT (0 :: Int) $ do
  withLifted (mockWith "a") $ \() -> liftIO $ putStrLn "inside"
  liftIO (putStrLn "outside")

testWithLifted2 :: IO ()
testWithLifted2 = evalContT $ flip evalStateT (0 :: Int) $ do
  withLifted (mockWith "a") $ \() -> modify (+ 1)
  get >>= liftIO . print

testWithLifted3 :: IO ()
testWithLifted3 = evalContT $ flip evalStateT (0 :: Int) $ do
  withLifted (mockWith "a") $ \() -> do
    withLifted (mockWith "b") $ \() -> do
      modify (+ 1)
  get >>= liftIO . print

-- this will deadlock - shiftT can violate the call-cont-exactly-once
-- constraint.
testWithLifted4 :: IO ()
testWithLifted4 = evalContT $ do
  withLifted (mockWith "a") $ \() -> shiftT $ \_c -> pure ()

-- no deadlock, all clean.
testWithLifted5 :: IO ()
testWithLifted5 = evalContT $ do
  withLifted (mockWith "a") $ \() -> resetT $ pure ()

-- this will deadlock - callCC escapes out of the with-block.
testWithLifted6 :: IO ()
testWithLifted6 = evalContT $ do
  callCC $ \c -> do
    withLifted (mockWith "a") $ \() -> c ()

-- but even worse: callCC breaks resetContIO.
testWithLifted7 :: IO ()
testWithLifted7 = evalContT $ do
  callCC $ \c -> resetContIO $ do
    liftIO $ putStrLn "before"
    () <- c ()
    liftIO $ putStrLn "after"

testWithLifted8 :: IO ()
testWithLifted8 = evalContT $ do
  callCC $ \c -> resetT $ do
    liftIO $ putStrLn "before"
    () <- c ()
    liftIO $ putStrLn "after"

