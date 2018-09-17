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

-- window :: Int -> Curs IO (Signal ByteString) (Signal ByteString)
-- window n = (\s -> consum s =<< iniW) ^>> act 
--     where 
--     iniW = do 
--         y <- liftIO (randomRIO (0.2,0.8))
--         x <- liftIO (randomRIO (0.2,0.8))
--         w <- newWindow (Height/5) (Width/5) 
--                        (Absolute y * Height) (Absolute x * Width)
--         drawBorder w
--         render
--         return w
--     consum sig win = mapSignalWithInit (\str -> return (rend win str, 
--                                                         tran win str)) sig
--     rend win str = do 
--         erase win
--         drawBorder win
--         moveCursor win 1 1
--         drawByteString win str
--         render
--         liftIO $ threadDelay (n * 100000)
--     tran _ str = do 
--         return (BS.drop 1 str)
-- 
-- m1 :: Curs IO () [ByteString]
-- m1 = proc () -> do
--     rec os <- signaler 4 -< pure "string" <> is
--         let is = mapMSignal (\x -> liftIO (randomRIO ('A','z')) >>= \c -> return (x <> BS.singleton c)) os
--     act -< runSig (os `until` ((== 0) . BS.length))

-- runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
-- runCurs (Curs op) i = runMC $ do
--     runKleisli (runReader op) (i, foreverSignal waitInput)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

