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

signaler :: Curs IO [ByteString] [ByteString]
signaler = repl ^>> act 
    where repl [] = return []
          repl (str:strss) = do moveCursor stdWindow 0 0
                                erase stdWindow
                                drawByteString stdWindow str
                                x <- getByteString
                                xs <- unsafeInterleaveMC (repl strss)
                                return (x:xs)
                        

dsa :: Curs IO () [ByteString] 
dsa = proc () -> do
    rec os <- signaler -< L.takeWhile (/= "q") ("start typing" : os)
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

act :: MonadIO m => Curs m (MC m output) output
act = Curs $ (A.lift) (Kleisli id)

actM :: MonadIO m => Curs m (m output) output
actM = Curs $ (A.lift) (Kleisli lift)

iof :: MonadIO m => (a -> IO b) -> Curs m a b
iof f = Curs $ StateArrow $ Kleisli $ \(x, s) -> do
    r <- liftIO (f x)
    return (r, s)

io :: MonadIO m => IO b -> Curs m () b
io = iof . pure
