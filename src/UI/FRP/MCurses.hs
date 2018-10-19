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
type CSF = MSF IOMC

window :: Int -> CSF ByteString ByteString
window n = MSF $ \str -> do 
    w <- ini 
    str' <- cons w str
    return (str', arrM $ cons w)
    where
    ini = do y <- liftIO $ randomRIO (0.25, 0.75) :: IOMC Double
             x <- liftIO $ randomRIO (0.25, 0.75) :: IOMC Double
             w <- newWindow (Height/5) (Width/5) (realToFrac y * Height)
                                                 (realToFrac x * Width)
             drawBorder w
             newRegion w (Height-2) (Width-2) 1 1
    cons win str = do
        moveCursor win 1 1
        erase win
        drawByteString win str
        render
        liftIO $ threadDelay (n * 100000)
        drawByteString win (BS.drop 1 str)
        render
        return (BS.drop 1 str)
        
m1 :: CSF () ()
m1 = proc () -> do
    rec is <- iPre "string strung" -< os
        os <- window 5 -< is
    returnA -< ()

embedCSF :: CSF a b -> [a] -> IO [b]
embedCSF op = runMC . embed op

reactimateCSF :: CSF () () -> IO ()
reactimateCSF = runMC . reactimate
