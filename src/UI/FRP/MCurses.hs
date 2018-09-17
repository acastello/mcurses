{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, GADTs, RankNTypes, ImpredicativeTypes #-}

module UI.FRP.MCurses where

import Control.Arrow
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.Reader
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.IO.Class

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.Semigroup
import Data.Unique

import Prelude hiding (until)

import System.IO
import System.Random

import UI.MCurses hiding (io)

newtype Curs m a b = Curs (InputsReader (MCArrow m) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type InputsReader = ReaderArrow (SignalSource Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO
type SignalSource = IOMC
type Finalizer = IOMC ()

data Signal a = Signal a (Wire a) Finalizer 

newtype Wire a = Wire [a -> IOMC a]

runSignal :: Signal a -> IOMC a
runSignal (Signal sourc (Wire wires) fini) = do
    x <- L.foldr (>=>) return wires sourc
    fini
    return x

source :: a -> Signal a -> Signal a
source x (Signal _ trans fini) = Signal x trans fini

trans :: (a -> IOMC a) -> Signal a -> Signal a
trans op (Signal ini (Wire ops) fini) = Signal ini (Wire $ ops ++ [op]) fini

signaler :: Int -> Int -> Curs IO (Signal ByteString) (Signal ByteString)
signaler delayn dropn = (\s -> consum s `liftM` iniW) ^>> act 
    where 
    iniW = do 
        y <- liftIO (randomRIO (0.2,0.8))
        x <- liftIO (randomRIO (0.2,0.8))
        w <- newWindow (Height/4) (Width/4) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
    consum sig win = trans (repl win) sig
    repl win str = rend win str >> tran win str
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay (delayn * 100000)
    tran _ str = do 
        liftIO $ hPrint stderr $ "signaler: " <> str
        ch <- liftIO (randomRIO ('A','z'))
        return (BS.drop dropn str <> BS.singleton ch)
                                          
m1 :: Curs IO () ByteString               
m1 = proc () -> do
    rec os <- signaler 9 2 -< source "string strung" os
    act -< runSignal os

-- input :: Curs IO () (Signal ByteString)
-- input = arr (\() -> foreverSignal getByteString)
-- 
runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

