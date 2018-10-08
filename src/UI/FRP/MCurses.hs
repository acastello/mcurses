{-# LANGUAGE Arrows                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE ImpredicativeTypes           #-}
{-# LANGUAGE RecursiveDo                  #-}

module UI.FRP.MCurses where

import Control.Arrow
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.Reader
import Control.Category hiding ((.), id)
import qualified Control.Category as Cat
import Control.Concurrent 
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.Semigroup
import Data.Unique

import Prelude hiding (until)

import System.IO
import System.Random

import UI.MCurses hiding (io)

type IOMC = MC IO

data Signal a b where
    Signal :: SignalStep a b -> IOMC () -> Signal a b

newtype SignalStep a b = SignalStep { stepSignal :: a -> IOMC (b, Maybe (SignalStep a b))}

runSignal :: Signal a b -> [a] -> IOMC [b]
runSignal (Signal step fini) xs = do
    ys <- runSteps step xs
    fini
    return ys
    where
    runSteps _ [] = return []
    runSteps (SignalStep f) (x:xs) = do
        (y, mstep) <- f x
        case mstep of
            Nothing -> return [y]
            Just step' -> (y:) `liftM` runSteps step' xs

instance Category SignalStep where
    id = SignalStep (\x -> return (x, Just Cat.id))
    SignalStep f' . SignalStep f = SignalStep $ \x -> do
        (y, mstep) <- f x
        (z, mstep') <- f' y
        return (z, liftM2 (Cat..) mstep' mstep)

instance Category Signal where
    id = Signal Cat.id (return ())
    Signal s' fin' . Signal s fin = Signal (s' <<< s) (fin >> fin')

instance Arrow SignalStep where
    arr f = SignalStep $ \x -> return (f x, Just (arr f))
    first (SignalStep f) = SignalStep $ \(x, d) -> do
        (y, mstep) <- f x
        return ((y,d), first <$> mstep)

instance Arrow Signal where
    arr f = Signal (arr f) (return ())
    first (Signal f fini) = Signal (first f) fini

instance ArrowLoop SignalStep where
    loop (SignalStep f) = SignalStep $ \x -> do
        rec ((y, c), mstep) <- f (x, c)
        case mstep of
            Nothing -> return (y, Just $ loop (SignalStep f))
            Just step -> stepSignal (loop step) x

instance ArrowLoop Signal where
    loop (Signal step fini) = Signal (loop step) fini

delay :: a -> Signal a a
delay x = Signal (step x) (return ())
    where
    step x' = SignalStep $ \y -> return (x', Just $ step y)

trans :: Signal ByteString ByteString
trans = Signal step (drawByteString stdWindow "done!" >> liftIO (threadDelay 500000))
    where
    step = SignalStep $ \x -> do moveCursor stdWindow 1 1 
                                 erase stdWindow
                                 drawByteString stdWindow x
                                 render
                                 waitInput
                                 if BS.length x > 1 then
                                     return (BS.drop 1 x, Just step)
                                 else
                                     return ("", Nothing)

m1 = proc () -> do
    rec is <- delay "string strung" -< os
        os <- trans -< is
    returnA -< is
