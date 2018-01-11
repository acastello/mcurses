{-# LANGUAGE FlexibleContexts, RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}

module MCurses where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

import UI.NCurses

data MEnv m s = MEnv 
  { me_panels :: M.Map String (Panel m s) 
  , me_z      :: [Panel m s]
  }

emptyMEnv :: MEnv m s
emptyMEnv = MEnv mempty []

type MCurses m s a = (MonadIO m, MonadState (MEnv m s) m) => m a

data Panel m s = Panel
  { p_name    :: String
  , p_win     :: Window
  , on_leave  :: MCurses m s ()
  }

f :: MCurses m s [Panel m s]
f = me_z <$> get

-- instance MonadState (MEnv m s, s) m => MonadState s m where
