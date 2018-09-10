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

