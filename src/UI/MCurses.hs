{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module MCurses where

import Control.Monad
import Control.Monad.State

import UI.NCurses

data MEnv = MEnv 
  { menv_x :: Int }


type MCurses m a = (Monad m, MonadState MEnv m) => m a

x :: MCurses m Int
x = menv_x <$> get

