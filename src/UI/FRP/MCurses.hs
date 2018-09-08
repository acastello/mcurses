{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving #-}

module UI.FRP.MCurses where

import Control.Arrow
import Control.Arrow.Operations hiding (delay)
import Control.Arrow.Transformer.State
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class

import UI.MCurses hiding (io)
-- import qualified UI.MCurses as C

import System.IO.Unsafe
import System.Random

data Asd = Asd
newtype Curs m a b = Curs (StateArrow Asd (Kleisli (MC m)) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)


feed :: Curs IO Integer [Integer]
feed = proc x -> do
    o <- iof randomRIO -< (-1, 1)
    xs <- feed -< x
    returnA -< x + o : xs

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

runCurs :: MonadIO m => Curs m a b -> a -> m b
runCurs (Curs op) i = runMC $ do
    fst <$> runKleisli (runState op) (i, Asd)

asd :: Curs IO Int [Int]
asd = proc x -> do
    rec y <- delay -< (x, y)
    returnA -< y

dsa :: (ArrowLoop a, Num b) => a b [b]
dsa = proc x -> do
    rec let y = x : fmap (*x) y
    returnA -< y

iof :: MonadIO m => (a -> IO b) -> Curs m a b
iof f = Curs $ StateArrow $ Kleisli $ \(x, s) -> do
    r <- liftIO (f x)
    return (r, s)

io :: MonadIO m => IO b -> Curs m () b
io = iof . pure
