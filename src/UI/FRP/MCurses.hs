{-# LANGUAGE Arrows                       #-}
{-# Language GeneralizedNewtypeDeriving   #-}
{-# Language GADTs                        #-} 
{-# Language RankNTypes                   #-}
{-# Language ImpredicativeTypes           #-}
{-# Language FlexibleInstances            #-}

module UI.FRP.MCurses where

import           Control.Arrow
import qualified Control.Arrow.Transformer as A
import           Control.Arrow.Transformer.Reader
import qualified Control.Category
import           Control.Category hiding ((.), id)
import           Control.Concurrent 
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class

import           Data.ByteString.Char8 hiding (putStrLn)
import qualified Data.ByteString.Char8 as BS
import           Data.List as L
import           Data.Semigroup
import           Data.Unique

import           Prelude hiding (until)

import           System.IO
import           System.Random

import           UI.MCurses hiding (io)

data Signal m a b where
    Id :: Signal m a a
    Pure :: (a -> b) -> Signal m a b
    Trans :: (a -> m b) -> Signal m a b
    Loop :: Signal m (a, d) (b, d) -> Signal m a b
    Gen :: m b -> Signal m a b

instance Category (Signal IO) where
    id = Id
    (.) = dot
        where
        Id       `dot` sig = sig
        sig      `dot` Id  = sig
        Pure op2 `dot` Pure op1 = Pure (op2 . op1)
        Pure op  `dot` sig = Trans (\x -> op `liftM` runSignal sig x)
        Trans op `dot` sig = Trans (\x -> runSignal sig x >>= op)
        Gen g    `dot` sig = Trans (\x -> runSignal sig x >> g)
        sig'     `dot` sig = Trans (\x -> runSignal sig x >>= runSignal sig')

instance Arrow (Signal IO) where
    arr = Pure
    first Id         = Id
    first (Pure op)  = Pure (\(a,d) -> (op a, d))
    first (Trans op) = Trans (\(a,d) -> liftM2 (,) (op a) (return d))
    first (Loop sig) = Loop $ proc ((a,e),d) -> do (b,d') <- sig -< (a,d)
                                                   returnA -< ((b,e),d')
    first (Gen g)    = Trans (\(_,d) -> liftM2 (,) g (return d))

instance ArrowLoop (Signal IO) where
    loop = Loop

runSignal :: Signal IO a b -> a -> IO b
runSignal sig x 
  | Id        <- sig = putStrLn "Id" >> return x
  | Pure op   <- sig = putStrLn "Pure" >> return (op x)
  | Trans op  <- sig = putStrLn "Trans" >> op x
  | Gen op    <- sig = putStrLn "Gen" >> op
  | Loop sig' <- sig = putStrLn "Loop" >> 
                         (fst `liftM` mfix (\t -> runSignal sig' (x, snd t)))

p :: Show a => Signal IO a ()
p = Trans print

t :: (Num a, Random a) => Signal IO a a
t = Trans $ \x -> do y <- randomRIO (0,100)
                     return (x+y)

m1 :: Signal IO Int [Int]
m1 = proc x -> do
    p -< x
    rec y <- t -< x
        ys <- returnA -< y : (*2) `fmap` ys
    returnA -< ys
