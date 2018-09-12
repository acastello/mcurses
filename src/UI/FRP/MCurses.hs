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
import Data.Functor.Identity
import Data.List as L

import Debug.Trace

import UI.MCurses hiding (io)
-- import qualified UI.MCurses as C

import System.IO
import System.IO.Unsafe
import System.Random

data Asd = Asd
newtype Curs m a b = Curs (StateArrow Asd (Kleisli (MC m)) a b)
    deriving (Functor, Applicative, Category, Arrow, ArrowChoice, ArrowApply, 
              ArrowLoop)

type IOMC = MC IO

newtype MList val = MList (IOMC (Maybe (val, MList val))) 

instance Semigroup (MList val) where
    (<>) = mappend

instance Monoid (MList val) where
    mempty = MList (return Nothing)

instance Functor MList where
    fmap f = mapMList (return . f)

instance Applicative MList where
    pure x = MList (return (Just (x, mempty)))

mapMList :: (a -> IOMC b) -> MList a -> MList b
mapMList f (MList op) = MList $ op >>= mapM
    (\(x, ml) -> do y <- f x
                    return (y, mapMList f ml))

type ML m x = [m x]
type L x = ML (MC IO) x

catL :: Monad m => x -> ML m x -> ML m x
catL x xs = return x : xs

mapL :: Monad m => (x -> m y) -> ML m x -> ML m y
mapL f xs = fmap (>>= f) xs

runL :: Monad m => ML m x -> m [x]
runL xs = sequence xs

data Signal val = Signal (IOMC ()) (MList val)

cat :: val -> MList val -> MList val
cat x preS = MList (return $ Just (x, preS))

cut :: (val -> Bool) -> MList val -> MList val
cut cmp (MList op) = MList $ do
    eith <- op
    return $ case eith of
        Nothing -> Nothing
        Just (val, sig) | cmp val -> Nothing
                        | otherwise -> Just (val, cut cmp sig)

runSig :: MList val -> IOMC [val]
runSig (MList op) = op >>= \eith -> case eith of
    Nothing -> return []
    Just (val, sig) -> (val:) `liftM` runSig sig

signaler :: Curs IO (MList ByteString) (MList ByteString)
signaler = (\s -> (flip cons s) `liftM` init) ^>> act 
    where 
    init = do 
        y <- liftIO (randomRIO (0,1))
        x <- liftIO (randomRIO (0,1))
        w <- newWindow (Height/5) (Width/5) 
                       (Absolute y * Height) (Absolute x * Width)
        drawBorder w
        render
        return w
    cons win ml = mapMList (repl win) ml
    repl win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        getByteString
        return $ case BS.uncons str of
            Nothing -> "q"
            Just (_, bs') -> bs'

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

a3 :: MonadFix m => Kleisli m () [ByteString]
a3 = proc () -> do
    rec os <- signaler'' -< "type here" `catL` (L.take 20 os)
    Kleisli runL -< os

a1 :: Curs IO () (MList ByteString)
a1 = proc () -> do
    rec os <- signaler -< cut (== "q") ("type here" `cat` os)
    returnA -< os

a2 :: Curs IO () [ByteString]
a2 = proc () -> do
    rec os <- signaler' -< "type here" `catL` os
    act -< runL os

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

testfix :: IO [ByteString]
testfix = mfix (\xs -> BS.appendFile "asd" (xs <> "\n") >> return ("some text" : (BS.drop 1 <$> xs)))
