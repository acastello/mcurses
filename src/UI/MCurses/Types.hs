{-# LANGUAGE CPP, CApiFFI, GeneralizedNewtypeDeriving #-}

module UI.MCurses.Types 
  ( module UI.MCurses.Types
  , module UI.MCurses.Internal ) where

#define ICAPI(NAME) foreign import capi "ncurses.h NAME" 

-- import Data.IORef

import Data.Tree (Tree, Forest)
import UI.MCurses.Internal

newtype Window = Window
  { win_pointer :: Either CWindowPtr CPanelPtr
  } deriving (Eq, Show, Ord)

type WinTree = Tree Window
type WinForest = Forest Window

win_wptr :: Window -> IO CWindowPtr
win_wptr w = case win_pointer w of
    Left ptr -> return ptr
    Right pptr -> c_panel_window pptr

type Relatives = (Relative, Relative, Relative, Relative)
type Dimensions = (Int, Int, Int, Int)

data Relative
  = Width | Height 
  | Constant Double
  | Neg Relative
  | Add Relative Relative
  | Sub Relative Relative
  | Mult Relative Relative
  | Div Relative Relative
  | Pow Relative Double

instance Show Relative where
    show Height = "Height"
    show Width = "Width"
    show (Constant x) = show x
    show (Neg (Neg x)) = show x
    show (Neg (Constant x)) = "-" ++ show x
    show (Neg x) = "-(" ++ show x ++ ")"
    show (Add x y) = show x ++ " + " ++ show y
    show (Sub x y) = show x ++ " - " ++ case y of
        Add _ _ -> "(" ++ show y ++ ")"
        Sub _ _ -> "(" ++ show y ++ ")"
        _ -> show y 
    show (Mult x y) = show x ++ " * " ++ case y of
        Add _ _ -> "(" ++ show y ++ ")"
        Sub _ _ -> "(" ++ show y ++ ")"
        _ -> show y 
    show (Div x y) = show x ++ " / " ++ case y of
        Add _ _ -> "(" ++ show y ++ ")"
        Sub _ _ -> "(" ++ show y ++ ")"
        Mult _ _ -> "(" ++ show y ++ ")"
        _ -> show y 
    show (Pow (Constant x) p) = show x ++ "^" ++ show p
    show (Pow x p) = "(" ++ show x ++ ")^" ++ show p
  
instance Num Relative where
    fromInteger = Constant . fromInteger
    (+) = Add
    (-) = Sub
    (*) = Mult
    negate x = Neg x
    abs = undefined
    signum = undefined

instance Fractional Relative where
    fromRational = Constant . fromRational
    (/) = Div

calcRelative :: Int -> Int -> Relative -> Int
calcRelative h w = round . casef where
    casef dim = case dim of
        Width       -> fi w
        Height      -> fi h
        Constant x  -> x
        Neg x       -> negate (casef x)
        Add x y     -> casef x + casef y
        Sub x y     -> casef x - casef y
        Mult x y    -> casef x * casef y
        Div x y     -> casef x / casef y
        Pow x e     -> casef x ** e

data CursorMode
  = CursorInvisible
  | CursorVisible
  | CursorUnknown Int
  deriving (Show, Eq)
  
instance Enum CursorMode where
    toEnum 0 = CursorInvisible
    toEnum 1 = CursorVisible
    toEnum n = CursorUnknown n

    fromEnum CursorInvisible = 0
    fromEnum CursorVisible = 1
    fromEnum (CursorUnknown n) = n

data Input 
  = CharPress Char
  | KeyPress Int
  | MouseInput Click
  | ScreenResized 
    deriving (Show, Eq)

data Click = Click { click_y :: Int, click_x :: Int, click_state :: C_MMASK }
    deriving (Show, Eq)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

