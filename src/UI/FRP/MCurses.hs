{-# LANGUAGE Arrows                       #-}
{-# Language GeneralizedNewtypeDeriving   #-}
{-# Language GADTs                        #-}
{-# Language RankNTypes                   #-}
{-# Language ImpredicativeTypes           #-}

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

type InputsReader = ReaderArrow (Signal Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

data Signal val = Signal (Wire val) (IOMC ())

newtype Wire val = Wire (IOMC (val, Maybe (Wire val)))

scan :: val -> Signal val -> Signal val
scan x (Signal wire fini) = Signal (Wire $ return (x, Just wire)) fini

runSignal :: Signal val -> IOMC [val]
runSignal (Signal wire fini) = do xs <- runWire wire
                                  fini
                                  return xs

runWire :: Wire val -> IOMC [val]
runWire (Wire op) = do (x, mwir) <- op
                       liftM (x:) $ case mwir of
                           Nothing -> return []
                           Just wir -> runWire wir

mapSignalWithInit :: (a -> IOMC b) -> IOMC () -> Signal a -> Signal b
mapSignalWithInit f fini' sig = 
    let Signal wire' fini = mapSignal f sig
    in Signal wire' (fini >> fini')

mapSignal :: (a -> IOMC b) -> Signal a -> Signal b
mapSignal f (Signal wire fini) = Signal (mapWire wire) fini
    where
    mapWire (Wire op) = Wire $ do 
        (x, mwir) <- op
        y <- f x
        case mwir of
            Nothing -> return (y, Nothing)
            Just wire' -> return (y, Just (mapWire wire'))

window :: Int -> Curs IO (Signal ByteString) (Signal ByteString)
window n = (\s -> consum s `liftM` iniW) ^>> act 
    where 
    iniW = do 
        y <- liftIO (randomRIO (0.2,0.8)) :: IOMC Double
        x <- liftIO (randomRIO (0.2,0.8)) :: IOMC Double
        w <- newWindow (Height/5) (Width/5) 
                       (Constant y * Height) (Constant x * Width)
        drawBorder w
        render
        return w
    consum sig win = mapSignalWithInit (repl win) 
                           (drawByteString stdWindow "!!!!!!!!!") sig
    repl win str = rend win str >> tran win str
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay (n * 100000)
    tran _ str = do 
        return (BS.drop 1 str)

m1 :: Curs IO () (Signal ByteString)
m1 = proc () -> do
    rec let is = scan "string strung" os
        os <- window 5 -< is
    returnA -< os

runCurs :: Curs IO a (Signal b) -> a -> IO [b]
runCurs (Curs op) i = runMC $ do
    runSignal =<< runKleisli (runReader op) (i, undefined)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

