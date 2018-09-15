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

type InputsReader = ReaderArrow (SignalSource Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

<<<<<<< HEAD
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
=======
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

signaler :: Int -> Int -> Curs IO (Signal ByteString) (Signal ByteString)
signaler delayn dropn = (\s -> consum s =<< iniW) ^>> act 
>>>>>>> test-frp
    where 
    iniW = do 
        y <- liftIO (randomRIO (0.2,0.8))
        x <- liftIO (randomRIO (0.2,0.8))
        w <- newWindow (Height/4) (Width/4) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
<<<<<<< HEAD
    cons ml win = mapSignalM (repl win) ml
    repl win str = do 
=======
    consum sig win = mapSignalWithInit (\str -> return (rend win str, 
                                                        tran win str)) sig
    rend win str = do 
>>>>>>> test-frp
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
<<<<<<< HEAD
        return (BS.drop 1 str)

a1 :: Curs IO () [ByteString]
a1 = proc () -> do
    rec os <- signaler -< takeS 5 ("string" `cat` os)
    act -< runSig os

=======
        liftIO $ threadDelay (delayn * 100000)
    tran _ str = do 
        liftIO $ hPrint stderr $ "signaler: " <> str
        return (BS.drop dropn str)

m1 :: Curs IO () [ByteString]
m1 = proc () -> do
    rec os <- signaler 9 2 -< pure "string strung" <> is
        let is = mapMSignal (\x -> do c <- liftIO (randomRIO ('A','C')) 
                                      liftIO $ hPrint stderr c
                                      return (x <> BS.singleton c)) os
        -- let is = mapMSignal (return . (<> ">")) os
    act -< runSig (os `until` ((== 0) . BS.length))

input :: Curs IO () (Signal ByteString)
input = arr (\() -> foreverSignal getByteString)

runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
>>>>>>> test-frp
runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, foreverSource waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

<<<<<<< HEAD
-- test :: Writer [ByteString] [ByteString]
test = mfix f where
    f strs = do print strs
                return ("string" : (BS.drop 1 <$> L.take 5 strs))
=======
>>>>>>> test-frp
