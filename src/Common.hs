module Common where

import Control.Monad.IO.Class ( MonadIO(..) )

import qualified Grammar.Abs as Abs


writeln :: MonadIO m => String -> m ()
writeln = liftIO . putStrLn


showPosition :: Abs.BNFC'Position -> String
showPosition position = case position of
  Just (x, y) -> " (line " ++ show x ++ ", column " ++ show y ++ ")"
  Nothing -> ""


(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

