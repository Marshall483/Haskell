{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader
import Control.Monad.State
import System.Directory

walkDirectory :: ( MonadState [FilePath] m, MonadIO m, MonadReader Integer m ) => Integer -> m ()
walkDirectory i = do
  paths <- get
  a <- ask
  --liftIO $ print ("a" ++ show a)
  liftIO $ print ("i" ++ show i)
  case paths of
    [] -> pure ()
    path : rest -> do
      if i == a then pure () 
      else liftIO $ putStrLn path
      put rest
      isDir <- liftIO $ doesDirectoryExist path
      when isDir $ do
        ds <- liftIO $ listDirectory path
        put $ rest ++ map ((path ++ "/") ++) ds
        walkDirectory (i + 1)
      if i == a then pure ()  else walkDirectory (i + 1)
      

main :: IO ()
main = do
  ds <- execStateT (runReaderT (walkDirectory 0) 3) ["."]
  forM_ ds print