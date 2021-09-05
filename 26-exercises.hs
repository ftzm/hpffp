module Chapter26 where

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad
import Control.Monad.IO.Class

--------------------------------------------------------------------------------
-- Exercises

rDec :: Num a => Reader a a
rDec = (subtract 1) <$> ask

-- >>> fmap (runReader rDec) [1..10]
-- [0,1,2,3,4,5,6,7,8,9]

rShow :: Show a => ReaderT a Identity String
rShow = show <$> ask

-- >>> fmap (runReader rShow) [1..10]
-- ["1","2","3","4","5","6","7","8","9","10"]

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  input <- ask
  lift $ putStrLn $ "Hi: " ++ (show input)
  return $ input + 1

-- >>> traverse (runReaderT rPrintAndInc) [1..10]
-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- Hi: 6
-- Hi: 7
-- Hi: 8
-- Hi: 9
-- Hi: 10
-- [2,3,4,5,6,7,8,9,10,11]

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  input <- get
  lift $ putStrLn $ "Hi: " ++ (show input)
  put $ input + 1
  return $ show input

-- >>> runStateT sPrintIncAccum 10-- Hi: 10
-- Hi: 10
-- ("10",11)

-- >>> mapM (runStateT sPrintIncAccum) [1..5]-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- [("1",2),("2",3),("3",4),("4",5),("5",6)]

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  liftIO $ guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
      ("Good, was very excite: " ++ e)

-- Skipping Scotty, not interested
-- Also skipping Morra, signal to noise on transformers is too low.
