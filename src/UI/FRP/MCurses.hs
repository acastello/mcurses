{-# LANGUAGE Arrows                       #-}
{-# Language GeneralizedNewtypeDeriving   #-}
{-# Language GADTs                        #-} 
{-# Language RankNTypes                   #-}
{-# Language ImpredicativeTypes           #-}
{-# Language FlexibleInstances            #-}

module UI.FRP.MCurses where

import           Control.Arrow
import qualified Control.Arrow.Transformer as A
import           Control.Arrow.Transformer.Reader
import qualified Control.Category
import           Control.Category hiding ((.), id)
import           Control.Concurrent 
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class

import           Data.ByteString.Char8 hiding (putStrLn)
import qualified Data.ByteString.Char8 as BS
import           Data.List as L
import           Data.Semigroup
import           Data.Unique

import           Prelude hiding (until)

import           System.IO
import           System.Random

import           UI.MCurses hiding (io)

type IOMC = MC IO 

newtype Curs a b = Curs (Kleisli IOMC a b)
    deriving (Category, Arrow, ArrowLoop)

data Signal a = Source a (Signal a) 
