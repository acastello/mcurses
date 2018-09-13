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

type InputsReader = ReaderArrow (Signal Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

newtype Signal val = Signal (IOMC (Maybe (val, Signal val))) 

instance Semigroup (Signal val) where
    (<>) = mappend

instance Monoid (Signal val) where
    mempty = Signal (return Nothing)

instance Functor Signal where
    fmap f = mapSignal (return . f)

instance Applicative Signal where
    pure x = Signal (return (Just (x, mempty)))

mapSignal :: (a -> IOMC b) -> Signal a -> Signal b
mapSignal f (Signal op) = Signal $ op >>= mapM
    (\(x, ml) -> do y <- f x
                    return (y, mapSignal f ml))

consumeSignal :: (a -> IOMC (IOMC b)) -> Signal a -> IOMC (Signal b)
consumeSignal f (Signal op) = op >>= \m -> case m of
    Nothing -> return mempty
    Just (x, sig) -> do op' <- f x
                        return $ Signal $ do y <- op'
                                             sig' <- consumeSignal f sig
                                             return $ Just (y, sig')

foreverSignal :: (IOMC a) -> Signal a
foreverSignal op = Signal $ do x <- op
                               return $ Just (x, foreverSignal op)



input :: MonadIO m => Curs m () (Signal ByteString)
input = arr (\_ -> foreverSignal getByteString)

type ML m x = [m x]
type L x = ML (MC IO) x

catL :: Monad m => x -> ML m x -> ML m x
catL x xs = return x : xs

mapL :: Monad m => (x -> m y) -> ML m x -> ML m y
mapL f xs = fmap (>>= f) xs

runL :: Monad m => ML m x -> m [x]
runL xs = sequence xs

cat :: val -> Signal val -> Signal val
cat x preS = Signal (return $ Just (x, preS))

until :: Signal val -> (val -> Bool) -> Signal val
until = flip cut

cut :: (val -> Bool) -> Signal val -> Signal val
cut cmp (Signal op) = Signal $ do
    eith <- op
    return $ case eith of
        Nothing -> Nothing
        Just (val, sig) | cmp val -> Nothing
                        | otherwise -> Just (val, cut cmp sig)

runSig :: Signal val -> IOMC [val]
runSig (Signal op) = op >>= \eith -> case eith of
    Nothing -> return []
    Just (val, sig) -> (val:) `liftM` runSig sig

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
    cons ml win = consumeSignal (repl win) ml
    repl win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay 200000
        return (return (BS.drop 1 str))

signaler' :: Curs IO (L ByteString) (L ByteString)
signaler' = arr (mapL f) 
    where
    f bs = do
        liftIO $ BS.appendFile "asd" (bs <> "\n")
        case BS.uncons bs of
            Nothing -> return ""
            Just (_, bs') -> return bs'

signaler'' :: MonadFix m => Kleisli m (ML m ByteString) (ML m ByteString)
signaler'' = arr (mapL f)
    where
    f bs = do 
        case BS.uncons bs of
            Nothing -> return ""
            Just (_, bs') -> return bs'

m1 :: Curs IO () [ByteString]
m1 = proc () -> do
    is <- input -< ()
    rec os <- signaler -< "string" `cat` is
    act -< runSig (cut ((==0) . BS.length) os)

runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    runKleisli (runReader op) (i, foreverSignal waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

