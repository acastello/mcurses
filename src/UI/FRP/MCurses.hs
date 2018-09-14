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

newtype Curs m a b = Curs (InputsReader (MCArrow m) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type InputsReader = ReaderArrow (Signal Input)
type MCArrow m = Kleisli (MC m)

type IOMC = MC IO

newtype Signal val = Signal (SignalSource val)

type SignalSource val = IOMC (Maybe (SignalWrap val)) 
type SignalWrap val = (SignalValue val, Signal val)
data SignalValue val = SimpleValue val 
                     | InitializedValue (IOMC ()) (IOMC val)
                     

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
-- map

mapSignalWithInit :: (a -> IOMC (IOMC (), IOMC b)) -> Signal a -> Signal b
mapSignalWithInit f (Signal op) = Signal (op >>= mapM step)
    where
    step x = undefined

-- foreverSignal :: (IOMC a) -> Signal a
-- foreverSignal op = Signal $ do 
--     x <- op
--     return $ Just (return (), x, foreverSignal op)
-- 
-- runSig :: Signal a -> IOMC [a]
-- runSig (Signal op) = do

-- input :: MonadIO m => Curs m () (Signal ByteString)
-- input = arr (\_ -> foreverSignal getByteString)

-- signaler :: Curs IO (Signal ByteString) (Signal ByteString)
-- signaler = (\s -> cons s `liftM` init) ^>> act 
--     where 
--     init = do 
--         y <- liftIO (randomRIO (0,1))
--         x <- liftIO (randomRIO (0,1))
--         w <- newWindow (Height/5) (Width/5) 
--                        (Absolute y * Height) (Absolute x * Width)
--         drawBorder w
--         render
--         return w
--     cons sig win = mapSignalWithInit (\x -> (rend win x, tran win x)) sig
--     rend win str = do 
--         erase win
--         drawBorder win
--         moveCursor win 1 1
--         drawByteString win str
--         render
--         liftIO $ threadDelay 200000
--     tran win str = do 
--         return (BS.drop 1 str)

-- m1 :: Curs IO () [ByteString]
-- m1 = proc () -> do
--     is <- input -< ()
--     rec os <- signaler -< "string" `cat` is
--     act -< runSig (cut ((==0) . BS.length) os)
-- 
-- runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
-- runCurs (Curs op) i = runMC $ do
--     runKleisli (runReader op) (i, foreverSignal waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

