{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}

module UI.FRP.MCurses where

import Control.Arrow
import Control.Arrow.Operations hiding (delay)
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.State
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS
import Data.Functor.Identity
import Data.List as L
import Data.Semigroup

import Debug.Trace

import Prelude hiding (until)

-- import qualified UI.MCurses as C

import System.IO
import System.IO.Unsafe
import System.Random

import UI.MCurses hiding (io)

data Asd = Asd
newtype Curs m a b = Curs (InputsReader (MCArrow m) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type InputsReader = ReaderArrow (SignalSource Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

data Signal val = Signal val (SignalSource val)
type SignalSource val = IOMC (Maybe (Signal val))

instance Semigroup (Signal val) where
    Signal x op <> sig' = Signal x ((Just . maybe sig' (<> sig')) `liftM` op)

-- instance Monoid (Signal val) where
--     mempty = Signal (return Nothing)
-- 
-- instance Functor Signal where
--     fmap f = mapSignal (return . f)
-- 
-- instance Applicative Signal where
--     pure x = Signal (return (Just (x, mempty)))
-- 
mapSignal :: (a -> b) -> Signal a -> Signal b
mapSignal f (Signal val op) = Signal (f val) (fmap (mapSignal f) `liftM` op)

mapSignalM :: (a -> IOMC b) -> Signal a -> IOMC (Signal b)
mapSignalM f (Signal val op) = do
    y <- f val
    return $ Signal y (op >>= mapM (mapSignalM f))

foreverSignal :: a -> Signal a
foreverSignal x = Signal x (return $ Just (foreverSignal x))

foreverSource :: (IOMC a) -> SignalSource a
foreverSource op = do x <- op
                      return $ Just (Signal x (foreverSource op))

-- zipS :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
-- zipS f (Signal op) (Signal op') = Signal $ do
--     m <- op
--     m' <- op'
--     return $ liftM2 (\(x, s) (y, s') -> (f x y, zipS f s s')) m m'
-- 
takeS :: Int -> Signal a -> Signal a
takeS n s @ (Signal x op) | n <= 1 = Signal x (return Nothing)
                          | otherwise = Signal x $ do
                              fmap (takeS (n-1)) `liftM` op
                            

input :: MonadIO m => Curs m () (Signal Input)
input = (Curs readState >>^ (\s -> waitInput >>= \i -> return (Signal i s))) >>> act

-- until :: Signal val -> (val -> Bool) -> Signal val
-- until = flip cut
-- 
cat :: a -> Signal a -> Signal a
cat a sig = Signal a (return $ Just sig)

-- cut :: (val -> Bool) -> Signal val -> Signal val
-- cut cmp (Signal op) = Signal $ do
--     eith <- op
--     return $ case eith of
--         Nothing -> Nothing
--         Just (val, sig) | cmp val -> Nothing
--                         | otherwise -> Just (val, cut cmp sig)

runSig :: Signal val -> IOMC [val]
runSig (Signal x op) = (x:) `liftM` (op >>= maybe (return []) runSig)

signaler :: Curs IO (Signal ByteString) (Signal ByteString)
signaler = (\s -> cons s =<< init) ^>> act 
    where 
    init = do 
        y <- liftIO (randomRIO (0,1))
        x <- liftIO (randomRIO (0,1))
        w <- newWindow (Height/5) (Width/5) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
    cons ml win = mapSignalM (repl win) ml
    repl win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win (BS.drop 1 str)
        liftIO (threadDelay 125000)
        render
        return (BS.drop 1 str)

a1 :: Curs IO () [ByteString]
a1 = proc () -> do
    rec os <- signaler -< takeS 5 ("string" `cat` os)
    act -< runSig os

runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, foreverSource waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

-- test :: Writer [ByteString] [ByteString]
test = mfix f where
    f strs = do print strs
                return ("string" : (BS.drop 1 <$> L.take 5 strs))
