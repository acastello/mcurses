{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, GADTs, RankNTypes, ImpredicativeTypes #-}

module UI.FRP.MCurses where

import Control.Arrow
import qualified Control.Arrow.Transformer as A
import Control.Arrow.Transformer.Reader
import Control.Category hiding ((.), id)
import Control.Concurrent 
import Control.Monad
import Control.Monad.IO.Class
import Control.Wire hiding (id, (.))

import Data.ByteString.Char8 as BS
import Data.List as L
import Data.Semigroup
import Data.Unique

import FRP.Netwire hiding (id, (.))

import Prelude hiding (until)

import System.IO
import System.Random

import UI.MCurses hiding (io)

type Curs = Wire () () (MC IO) 

asd :: Curs String String
asd = mkGen_ $ \str -> do
    c <- liftIO $ randomRIO ('A','z')
    return $ Right (c : str)

window :: Int -> Int -> Curs ByteString ByteString
window delayn dropn = mkGen startit
    where 
    startit () str = do win <- iniW
                        y <- repl win str
                        return (y, mkGen_ $ repl win)
    iniW = do 
        y <- liftIO (randomRIO (0.2,0.8))
        x <- liftIO (randomRIO (0.2,0.8))
        newWindow (Height/4) (Width/4) 
                  (Absolute y * Height) (Absolute x * Width)
    repl win str = rend win str >> (Right `liftM` tran win str)
    rend win str = do 
        erase win
        drawBorder win
        moveCursor win 1 1
        drawByteString win str
        render
        liftIO $ threadDelay (delayn * 100000)
    tran _ str = do 
        liftIO $ hPrint stderr $ "window: " <> str
        ch <- liftIO (randomRIO ('A','z'))
        return (BS.drop dropn $ str <> BS.singleton ch)
                                          
m1 :: Curs () ByteString
m1 = proc () -> do
    rec 
        is <- delay "string strung" -< fs
        os <- window 3 2 -< is
        ev <- noLonger ((> 0) . BS.length) -< os
        fs <- until -< (os, ev)
    returnA -< os

runCurs :: Curs () a -> IO [a]
runCurs = runMC . f
    where
    f wir = do (eith, wir') <- stepWire wir () (Right ())
               case eith of Left _ -> return []
                            Right x -> (x:) `liftM` f wir'

-- input :: Curs IO () (Signal ByteString)
-- input = arr (\() -> foreverSignal getByteString)
-- 
