{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, GADTs, RankNTypes, ImpredicativeTypes #-}

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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS
import Data.Functor.Identity
import Data.List as L
import Data.Semigroup
import Data.Unique

import Debug.Trace

import Prelude hiding (until)

-- import qualified UI.MCurses as C

import System.IO
import System.IO.Unsafe
import System.Random

import UI.MCurses hiding (io)

newtype Curs m a b = Curs (InputsReader (MCArrow m) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type InputsReader = ReaderArrow (Signal Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

newtype Signal val = Signal (SignalSource val)

type SignalSource val = IOMC (Maybe (SignalTup val)) 
type SignalTup val = (SignalCell val, Signal val)
data SignalCell val = ConstCell val | InitCell Unique (IOMC ())

-- instance Semigroup (Signal val) where
--     (<>) = mappend
-- 
-- instance Monoid (Signal val) where
--     mempty = Signal (return ()) (return Nothing)
-- 
-- instance Functor Signal where
--     fmap f = mapSignal (return . f)
-- 
-- instance Applicative Signal where
--     pure x = Signal (return ()) (return (Just (x, mempty)))
-- 
-- mapSignal :: (a -> IOMC b) -> Signal a -> Signal b
-- mapSignal f (Signal op) = Signal $ op >>= mapM
--     (\(x, ml) -> do y <- f x
--                     return (y, mapSignal f ml))
-- 
mapCells :: (SignalCell a -> IOMC (SignalCell b)) -> Signal a -> Signal b
mapCells f (Signal op) = Signal $ op >>= mapM trans
    where
    trans (c, sig) = f c >>= \c' -> return (c', mapCells f sig)

mapSignalWithInit :: (a -> IOMC (IOMC (), IOMC b)) -> Signal a -> IOMC (Signal b)
mapSignalWithInit genOp sig = do
    u <- liftIO newUnique 
    return (map' u sig)
    where
    map' u (Signal op) = Signal (op >>= trans u)
    trans _ Nothing = return Nothing
    trans u (Just (ConstCell x, sig)) = do 
        (ini, f) <- genOp x
        return $ Just $ (,) (InitCell u ini) $ Signal $ do
            y <- f 
            return $ Just (ConstCell y, map' u sig)
    trans u (Just (InitCell u' ini, sigNext @ (Signal op'))) 
        | u /= u' = return $ Just (InitCell u' ini, map' u sigNext)
        | u == u' = op' >>= trans u 

foreverSignal :: (IOMC a) -> Signal a
foreverSignal op = Signal $ do 
    x <- op
    return $ Just (ConstCell x, foreverSignal op)

cat :: a -> Signal a -> Signal a
cat x sig = Signal (return (Just (ConstCell x, sig)))

until :: Signal a -> (a -> Bool) -> Signal a
until s f = untilM s (return . f)

untilM :: Signal a -> (a -> IOMC Bool) -> Signal a
untilM (Signal op) cmp = Signal $ op >>= \m -> case m of
    Nothing -> return Nothing
    Just (c @ InitCell {}, sig) -> return $ Just (c, untilM sig cmp)
    Just (c @ (ConstCell x), sig) -> do 
        b <- cmp x
        return $ if b then Nothing 
                      else Just (c, untilM sig cmp)

runSig :: Signal a -> IOMC [a]
runSig (Signal op) = op >>= maybe (return []) f
    where
    f (c, sig) = liftM2 (++) (f' c) (runSig sig) 
    f' (ConstCell x) = return [x]
    f' (InitCell _ ini) = ini >> return []

-- debugSig :: Signal a -> IOMC [a]
-- debugSig (Signal op) = op >>= maybe (return []) f
--     where
--     f (c, sig) = liftM2 (++) (f' c) (debugSig sig)
--     f' (ConstCell x) = liftIO (BS.hPutStrLn stderr "Const") >> return [x]
--     f' (InitCell ini) = liftIO (BS.hPutStrLn stderr "Init") >> ini >> return []
-- 
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
    cons sig win = mapSignalWithInit (\str -> return (rend win str, 
                                                      tran win str)) sig
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay 200000
    tran win str = do 
        return (BS.drop 1 str)

m1 :: Curs IO () [ByteString]
m1 = proc () -> do
    rec os <- signaler -< "string" `cat` is
        is <- signaler -< (os `until` (== ""))
    act -< runSig os

input :: Curs IOMC () (Signal ByteString)
input = arr (\() -> foreverSignal getByteString)

runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, foreverSignal waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

