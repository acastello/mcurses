{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}

module UI.FRP.MCurses where

import Control.Arrow
import Control.Arrow.Operations hiding (delay)
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.State
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as BS
import Data.List as L

import UI.MCurses hiding (io)
-- import qualified UI.MCurses as C

import System.IO
import System.IO.Unsafe
import System.Random

data Asd = Asd
newtype Curs m a b = Curs (StateArrow Asd (Kleisli (MC m)) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

data Signal val = Signal val [Signal val]

scan :: b -> (b -> a -> b) -> Signal a -> Signal b
scan initV accumF (Signal val ss) = 
    Signal initV (scan (accumF initV val) accumF <$> ss)

signaler :: Curs IO (Signal ByteString) (Signal ByteString)
signaler = (\s -> init >>= flip repl s) ^>> act 
    where init = do y <- liftIO (randomRIO (0,1))
                    x <- liftIO (randomRIO (0,1))
                    w <- newWindow (Height/5) (Width/5) 
                                   (Absolute y * Height) (Absolute x * Width)
                    drawBorder w
                    render
                    return w
          repl win (Signal str ss) = do 
              moveCursor win 0 0
              erase win
              drawBorder win
              drawByteString win str
              render
              x <- getByteString
              xs <- unsafeInterleaveMC (repl win `mapM` ss)
              when (L.null ss) $ do
                  delWindow win
              return (Signal x xs)

dsa :: Curs IO () (Signal ByteString)
dsa = proc () -> do
    rec let is = Signal "type" [os]
        os <- signaler -< is
    returnA -< os

asd :: Show a => [a] -> IO [a]
asd [] = return []
asd (x:xs) = do
    print x
    ys <- unsafeInterleaveIO $ asd xs
    return (x : ys)

feed :: Curs IO Integer [Integer]
feed = rep ^>> actM
    where
    rep x = do o <- randomRIO (-1, 1)
               xs <- unsafeInterleaveIO $ rep x
               return (x + o : xs)

delayf :: [Int] -> IO Int
delayf x = do
    o <- randomRIO (-1,1)
    print o
    threadDelay 333333
    return o

delay :: Curs IO (Int, [Int]) [Int]
delay = proc (x, y) -> do
    o <- iof delayf -< y
    let z = (x + o) : y
    returnA -< z
    -- io (threadDelay 500000) -< ()
    -- returnA -< 1

runCurs :: (MonadIO m, MonadMask m) => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    fst <$> runKleisli (runState op) (i, Asd)

act :: Monad m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: Monad m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

iof :: MonadIO m => (a -> IO b) -> Curs m a b
iof f = Curs $ StateArrow $ Kleisli $ \(x, s) -> do
    r <- liftIO (f x)
    return (r, s)

io :: MonadIO m => IO b -> Curs m () b
io = iof . pure
