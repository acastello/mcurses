{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, GADTs, RankNTypes, ImpredicativeTypes #-}

module UI.FRP.MCurses where

import Control.Arrow
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.Reader
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.Trans.MSF
import Control.Monad.IO.Class

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.MonadicStreamFunction
import Data.Semigroup
import Data.Unique

import Prelude hiding (until)

import System.IO
import System.Random

import UI.MCurses hiding (io)

type IOMC = MC IO

window :: MSF 
