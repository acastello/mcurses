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

-- import qualified UI.MCurses as C

import System.IO
import System.Random

import UI.MCurses hiding (io)

newtype Curs m a b = Curs (InputsReader (MCArrow m) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type InputsReader = ReaderArrow (Signal Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

newtype Signal val = Signal { unSignal :: SignalSource val }

type SignalSource val = IOMC (Maybe (SignalTup val)) 
type SignalTup val = (SignalCell val, Signal val)
data SignalCell val = ConstCell val | InitCell Unique (IOMC ())

instance Semigroup (Signal val) where
    (<>) = mappend

instance Monoid (Signal val) where
    mempty = Signal (return Nothing)
    mappend = attach


instance Functor Signal where
    fmap = mapSignal

instance Applicative Signal where
    pure x = Signal (return (Just (ConstCell x, mempty)))

-- mapSignal :: (a -> IOMC b) -> Signal a -> Signal b
-- mapSignal f (Signal op) = Signal $ op >>= mapM
--     (\(x, ml) -> do y <- f x
--                     return (y, mapSignal f ml))
-- 
mapCells :: (SignalCell a -> IOMC (SignalCell b)) -> Signal a -> Signal b
mapCells f (Signal op) = Signal $ op >>= mapM trans
    where
    trans (c, sig) = f c >>= \c' -> return (c', mapCells f sig)

mapSignal :: (a -> b) -> Signal a -> Signal b
mapSignal f s = mapMSignal (return . f) s

mapMSignal :: (a -> IOMC b) -> Signal a -> Signal b
mapMSignal f = mapCells g
    where
    g (ConstCell x) = ConstCell `liftM` f x
    g (InitCell u op) = return (InitCell u op)

mapSignalWithInit :: (a -> IOMC (IOMC (), IOMC b)) -> Signal a -> IOMC (Signal b)
mapSignalWithInit genOp sig = do
    u <- liftIO newUnique 
    return (map' u sig)
    where
    map' u (Signal op) = Signal (op >>= trans u)
    trans _ Nothing = return Nothing
    trans u (Just (ConstCell x, nextSig)) = do 
        (ini, f) <- genOp x
        return $ Just $ (,) (InitCell u ini) $ Signal $ do
            y <- f 
            return $ Just (ConstCell y, map' u nextSig)
    trans u (Just (InitCell u' ini, sigNext @ (Signal op'))) 
        | u /= u' = return $ Just (InitCell u' ini, map' u sigNext)
        | otherwise = op' >>= trans u 

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

zipSignals :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipSignals f s s' = zipMSignals (\x y -> return (f x y)) s s'

zipMSignals :: (a -> b -> IOMC c) -> Signal a -> Signal b -> Signal c
zipMSignals f (Signal op) (Signal op') = Signal $ do
    mtup <- op
    mtup' <- op'
    mstep mtup mtup'
    where
    mstep Nothing _ = return Nothing
    mstep _ Nothing = return Nothing
    mstep (Just t) (Just t') = step t t'
    step (InitCell u ini, s) (InitCell u' ini', s') = return $ Just $ 
        (,) (InitCell u ini) $ Signal $ return $ Just $ 
            (,) (InitCell u' ini') $ Signal $ do
                unSignal (zipMSignals f s s')
    step (ConstCell x, s) (ConstCell x', s') = do 
        y <- f x x'
        return $ Just (ConstCell y, zipMSignals f s s')
    step (InitCell u ini, s) tup' = return $ Just $ 
        (,) (InitCell u ini) $ Signal $ do
            mtup <- unSignal s
            mstep mtup (Just tup')
    step tup (InitCell u ini, s) = return $ Just $ 
        (,) (InitCell u ini) $ Signal $ do
            mtup' <- unSignal s
            mstep (Just tup) mtup'

attach :: Signal a -> Signal a -> Signal a
attach (Signal op) sig = Signal $ op >>= \m -> case m of
    Nothing -> unSignal sig
    Just (x, sig') -> return $ Just (x, attach sig' sig)

runSig :: Signal a -> IOMC [a]
runSig (Signal op) = op >>= maybe (return []) f
    where
    f (c, sig) = liftM2 (++) (f' c) (runSig sig) 
    f' (ConstCell x) = return [x]
    f' (InitCell _ ini) = ini >> return []

debugSig :: Signal a -> IOMC [a]
debugSig (Signal op) = op >>= maybe (return []) f
    where
    f (c, sig) = liftM2 (++) (f' c) (debugSig sig)
    f' (ConstCell x) = liftIO (BS.hPutStrLn stderr "Const") >> return [x]
    f' (InitCell _ ini) = liftIO (BS.hPutStrLn stderr "Init") >> ini >> return []

signaler :: Int -> Curs IO (Signal ByteString) (Signal ByteString)
signaler n = (\s -> consum s =<< iniW) ^>> act 
    where 
    iniW = do 
        y <- liftIO (randomRIO (0,1))
        x <- liftIO (randomRIO (0,1))
        w <- newWindow (Height/5) (Width/5) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
    consum sig win = mapSignalWithInit (\str -> return (rend win str, 
                                                        tran win str)) sig
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ BS.hPutStrLn stderr str
        liftIO $ threadDelay (n * 100000)
    tran _ str = do 
        return (BS.drop 1 str <> BS.pack [BS.head str])

m1 :: Curs IO () [ByteString]
m1 = proc () -> do
    rec os <- signaler 5 -< is
        is <- signaler 5 -< pure "string" <> os
    act -< debugSig (os `until` ((== 0) . BS.length))

input :: Curs IOMC () (Signal ByteString)
input = arr (\() -> foreverSignal getByteString)

runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, foreverSignal waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

