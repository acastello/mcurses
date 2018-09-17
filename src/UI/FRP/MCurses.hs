{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, GADTs, RankNTypes, ImpredicativeTypes #-}

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
    Const :: b -> Signal a b
    Pure :: (a -> (b, Signal a b)) -> Signal a b
    Trans :: (a -> IOMC (b, Signal a b)) -> Signal a b

stepSignal :: Signal a b -> a -> IOMC (b, Signal a b)
stepSignal (Const b) _ = return (b, Const b)
stepSignal (Pure f) x = return (f x)
stepSignal (Trans f) x = f x

runSignal :: Signal a a -> a -> IOMC ()
runSignal sig x = do (y, sig') <- stepSignal sig x
                     runSignal sig' y

runSignals :: Signal a b -> [a] -> IOMC [b]
runSignals _ [] = return []
runSignals sig (x:xs) = do (y, sig') <- stepSignal sig x
                           (y:) `liftM` runSignals sig' xs
    
instance Category Signal where
    id = Trans $ \x -> return (x, Cat.id)
    sig2 . sig1 = Trans $ \x -> do
        (y1, sig1') <- stepSignal sig1 x
        (y2, sig2') <- stepSignal sig2 y1
        return (y2, sig2' Cat.. sig1')
         
instance Arrow Signal where
    arr f = Trans (\x -> return (f x, arr f))
    first sig = Trans $ \(x,d) -> do
        (y,sig') <- stepSignal sig x
        return ((y,d), first sig')

counter = proc reset -> do
    rec
        out <- returnA -< if reset then 0 else next
        next <- delay 0 -< out + 1
    returnA -< out

-- fillCounters <- 
powersf :: Num a => a -> [a] -> ([a], [a])
powersf x l = (l, x : fmap (*x) l)

powers :: ArrowLoop a => a Integer [Integer]
powers = loop $ arr $ uncurry powersf

instance ArrowLoop Signal where
    loop sig = Trans $ \x -> (fst ***! loop) 
                            `liftM` 
                               (mfix $ \ ~((_, d), _) -> stepSignal sig (x, d))
    
delay :: a -> Signal a a
delay x = Pure $ \x' -> x `seq` (x, delay x')

-- delay :: a -> Signal a a 
-- delay x' = mkSFN $ \x -> (x', delay x)
-- 
-- mkPureN :: (a -> (b, Signal a b)) -> Signal a b
-- mkPureN f = loop
--     where
--     loop = Pure $ \x -> lstrict (f x)
-- 
-- mkSFN :: (a -> (b, Signal a b)) -> Signal a b
-- mkSFN f = mkPureN (lstrict . f)
-- 
-- lstrict :: (a, b) -> (a, b)
-- lstrict (x, y) = x `seq` (x, y)


(***!) :: (a -> c) -> (b -> d) -> ((a, b) -> (c, d))
(***!) f g (x', y') =
    let (x, y) = (f x', g y')
    in x `seq` (x, y)
-- 
-- instance Functor Signal where
--     fmap f (Pure returner) = Pure (f `liftM` returner)
--     fmap f (Gen s trans) = Gen s (liftM f . trans)
-- 
-- instance Applicative Signal where
--     pure = Pure . return
-- 
-- scan :: a -> (a -> b -> a) -> Signal b -> Signal a
-- scan initValue accumF sig = case sig of
--     Pure ret = Pure (return initValue) `Gen` (\i 
--     Gen source trans = Gen source (\x -> 
-- 
window :: Int -> Int -> Signal ByteString ByteString
window delayn dropn = Trans $ \s -> consum s =<< iniW
    where 
    iniW = do 
        y <- liftIO (randomRIO (0.2,0.8))
        x <- liftIO (randomRIO (0.2,0.8))
        w <- newWindow (Height/4) (Width/4) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
    consum str win = do rend win str
                        str' <- tran win str
                        return (str', signaler win)
    signaler win = Trans $ \str -> do rend win str
                                      str' <- tran win str
                                      return $ str' `seq` (str', signaler win)
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay (delayn * 100000)
    tran _ str = do 
        liftIO $ hPrint stderr $ "signaler: " <> str
        c <- liftIO $ randomRIO ('A','z')
        return (BS.drop dropn str <> BS.singleton c)

m1 :: Signal a ByteString
m1 = proc _ -> do
    rec 
        is <- delay "string strung" -< os
        os <- window 9 2 -< is
    returnA -< os

-- input :: Curs IO () (Signal ByteString)
-- input = arr (\() -> foreverSignal getByteString)
-- 
-- runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
-- runCurs (Curs op) i = runMC $ do
--     runKleisli (runReader op) (i, waitInput)
-- 
-- act :: Monad m => Curs m (MC m output) output
-- act = Curs $ (A.lift) (Kleisli id)
-- 
-- actM :: Monad m => Curs m (m output) output
-- actM = Curs $ (A.lift) (Kleisli lift)

