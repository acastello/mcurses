{-# LANGUAGE CPP, CApiFFI, InterruptibleFFI, GeneralizedNewtypeDeriving #-}

module UI.MCurses.Internal where

#define ICAPI(NAME) foreign import capi safe "curses.h NAME" 
#define ICAPIP(NAME) foreign import capi "panel.h NAME" 

import Control.Monad

import Data.Bits

import Foreign
import Foreign.C

import Numeric

import Text.Printf


data {-# CTYPE "panel.h" "PANEL" #-} C_PANEL = C_PANEL
type CPanelPtr = Ptr C_PANEL
type CPanelForeignPtr = ForeignPtr C_PANEL

data {-# CTYPE "curses.h" "WINDOW" #-} C_PAD = C_PAD
    deriving Show

type CPadPtr = Ptr C_PAD

data {-# CTYPE "curses.h" "WINDOW" #-} C_WINDOW = C_WINDOW
    deriving Show

type CWindowPtr = Ptr C_WINDOW
type CWindowForeignPtr = ForeignPtr C_WINDOW

type ColorID = Int

data {-# CTYPE "curses.h" "MEVENT" #-} MEVENT = MEVENT CShort CInt CInt CInt C_MMASK
    deriving (Show)

instance Storable MEVENT where
    sizeOf _ = 20
    alignment = sizeOf
    peek ptr = liftM5 MEVENT
        (peekByteOff ptr 0)
        (peekByteOff ptr 4)
        (peekByteOff ptr 8)
        (peekByteOff ptr 12)
        (peekByteOff ptr 16)
    poke ptr (MEVENT i x y z b) = do
        pokeByteOff ptr 0 i
        pokeByteOff ptr 4 x
        pokeByteOff ptr 8 y
        pokeByteOff ptr 12 z
        pokeByteOff ptr 16 b

newtype {-# CTYPE "curses.h" "mmask_t" #-} C_MMASK = C_MMASK { unMMASK :: CInt }
    deriving (Eq, Storable, Bits)

instance Show C_MMASK where
    show (C_MMASK i) = showHex i ""

type CHType = Int
-- newtype {-# CTYPE "curses.h" "chtype" #-} CHType = CHType Int

-- instance Show CHType where
--     show (CHType i) = printf "%X" i

ICAPI(value A_ALTCHARSET) c_AALTCHARSET  :: CInt
ICAPI(value A_BLINK)      c_ABLINK       :: CInt
ICAPI(value A_BOLD)       c_ABOLD        :: CInt
ICAPI(value A_CHARTEXT)   c_ACHARTEXT    :: CInt
ICAPI(value A_COLOR)      c_ACOLOR       :: CInt
ICAPI(value A_DIM)        c_ADIM         :: CInt
ICAPI(value A_HORIZONTAL) c_AHORIZONTAL  :: CInt
ICAPI(value A_INVIS)      c_AINVIS       :: CInt
ICAPI(value A_ITALIC)     c_AITALIC      :: CInt
ICAPI(value A_LEFT)       c_ALEFT        :: CInt
ICAPI(value A_LOW)        c_ALOW         :: CInt
ICAPI(value A_NORMAL)     c_ANORMAL      :: CInt
ICAPI(value A_PROTECT)    c_APROTECT     :: CInt
ICAPI(value A_REVERSE)    c_AREVERSE     :: CInt
ICAPI(value A_RIGHT)      c_ARIGHT       :: CInt
ICAPI(value A_STANDOUT)   c_ASTANDOUT    :: CInt
ICAPI(value A_TOP)        c_ATOP         :: CInt
ICAPI(value A_UNDERLINE)  c_AUNDERLINE   :: CInt
ICAPI(value A_VERTICAL)   c_AVERTICAL    :: CInt

data Attribute 
  = AttrColor Color
  | AttrBold
  | AttrBlink
  | AttrDim
  | AttrReverse
  | AttrStandout
  | AttrUnderline
  deriving (Show, Eq)

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | DefaultColor
  deriving (Show, Eq) 

instance Enum Color where
    toEnum c 
      | c == c_BLACK   = Black   
      | c == c_RED     = Red     
      | c == c_GREEN   = Green   
      | c == c_YELLOW  = Yellow  
      | c == c_BLUE    = Blue    
      | c == c_MAGENTA = Magenta 
      | c == c_CYAN    = Cyan    
      | c == c_WHITE   = White   
      | c == -1        = DefaultColor 
      | otherwise      = error $ printf "No such color: %d" c
    fromEnum c = case c of
        Black   -> c_BLACK
        Red     -> c_RED
        Green   -> c_GREEN
        Yellow  -> c_YELLOW
        Blue    -> c_BLUE
        Magenta -> c_MAGENTA
        Cyan    -> c_CYAN
        White   -> c_WHITE
        DefaultColor  -> -1

ICAPI(value COLOR_BLACK)    c_BLACK   :: Int
ICAPI(value COLOR_RED)      c_RED     :: Int
ICAPI(value COLOR_GREEN)    c_GREEN   :: Int
ICAPI(value COLOR_YELLOW)   c_YELLOW  :: Int
ICAPI(value COLOR_BLUE)     c_BLUE    :: Int
ICAPI(value COLOR_MAGENTA)  c_MAGENTA :: Int
ICAPI(value COLOR_CYAN)     c_CYAN    :: Int
ICAPI(value COLOR_WHITE)    c_WHITE   :: Int

ICAPI(value ERR)            c_ERR           :: CInt
ICAPI(value KEY_CODE_YES)   c_KEY_CODE_YES  :: CInt
ICAPI(value KEY_MOUSE)      c_KEY_MOUSE     :: Int
ICAPI(value KEY_RESIZE)     c_KEY_RESIZE    :: Int

ICAPI(value ALL_MOUSE_EVENTS)       c_ALL_MOUSE_EVENTS        :: C_MMASK
ICAPI(value REPORT_MOUSE_POSITION)  c_REPORT_MOUSE_POSITION   :: C_MMASK

ICAPI(value BUTTON1_PRESSED)          c_BUTTON1_PRESSED         :: C_MMASK
ICAPI(value BUTTON1_RELEASED)         c_BUTTON1_RELEASED        :: C_MMASK
ICAPI(value BUTTON1_CLICKED)          c_BUTTON1_CLICKED         :: C_MMASK
ICAPI(value BUTTON1_DOUBLE_CLICKED)   c_BUTTON1_DOUBLE_CLICKED  :: C_MMASK
ICAPI(value BUTTON1_TRIPLE_CLICKED)   c_BUTTON1_TRIPLE_CLICKED  :: C_MMASK

ICAPI(value BUTTON2_PRESSED)          c_BUTTON2_PRESSED         :: C_MMASK
ICAPI(value BUTTON2_RELEASED)         c_BUTTON2_RELEASED        :: C_MMASK
ICAPI(value BUTTON2_CLICKED)          c_BUTTON2_CLICKED         :: C_MMASK
ICAPI(value BUTTON2_DOUBLE_CLICKED)   c_BUTTON2_DOUBLE_CLICKED  :: C_MMASK
ICAPI(value BUTTON2_TRIPLE_CLICKED)   c_BUTTON2_TRIPLE_CLICKED  :: C_MMASK

ICAPI(value BUTTON3_PRESSED)          c_BUTTON3_PRESSED         :: C_MMASK
ICAPI(value BUTTON3_RELEASED)         c_BUTTON3_RELEASED        :: C_MMASK
ICAPI(value BUTTON3_CLICKED)          c_BUTTON3_CLICKED         :: C_MMASK
ICAPI(value BUTTON3_DOUBLE_CLICKED)   c_BUTTON3_DOUBLE_CLICKED  :: C_MMASK
ICAPI(value BUTTON3_TRIPLE_CLICKED)   c_BUTTON3_TRIPLE_CLICKED  :: C_MMASK

ICAPI(value BUTTON4_PRESSED)          c_BUTTON4_PRESSED         :: C_MMASK
ICAPI(value BUTTON4_RELEASED)         c_BUTTON4_RELEASED        :: C_MMASK
ICAPI(value BUTTON4_CLICKED)          c_BUTTON4_CLICKED         :: C_MMASK
ICAPI(value BUTTON4_DOUBLE_CLICKED)   c_BUTTON4_DOUBLE_CLICKED  :: C_MMASK
ICAPI(value BUTTON4_TRIPLE_CLICKED)   c_BUTTON4_TRIPLE_CLICKED  :: C_MMASK

ICAPI(value BUTTON5_PRESSED)          c_BUTTON5_PRESSED         :: C_MMASK
ICAPI(value BUTTON5_RELEASED)         c_BUTTON5_RELEASED        :: C_MMASK
ICAPI(value BUTTON5_CLICKED)          c_BUTTON5_CLICKED         :: C_MMASK
ICAPI(value BUTTON5_DOUBLE_CLICKED)   c_BUTTON5_DOUBLE_CLICKED  :: C_MMASK
ICAPI(value BUTTON5_TRIPLE_CLICKED)   c_BUTTON5_TRIPLE_CLICKED  :: C_MMASK

ICAPI(value BUTTON_SHIFT)   c_BUTTON_SHIFT  :: C_MMASK
ICAPI(value BUTTON_CTRL)    c_BUTTON_CTRL   :: C_MMASK
ICAPI(value BUTTON_ALT)     c_BUTTON_ALT    :: C_MMASK

ICAPI(value stdscr)   c_stdscr        :: CWindowPtr

ICAPI(&COLS)          c_COLS          :: Ptr CInt
ICAPI(&LINES)         c_LINES         :: Ptr CInt
ICAPI(&COLORS)        c_COLORS        :: Ptr CInt
ICAPI(&COLOR_PAIRS)   c_COLOR_PAIRS   :: Ptr CInt

ICAPI(cbreak)         c_cbreak        :: IO CInt
ICAPI(curs_set)       c_curs_set      :: Int -> IO CInt
ICAPI(delwin)         c_delwin        :: CWindowPtr -> IO CInt
ICAPI(derwin)         c_derwin        :: CWindowPtr -> Int -> Int -> Int -> Int -> IO CWindowPtr
ICAPI(doupdate)       c_doupdate      :: IO CInt
ICAPI(endwin)         c_endwin        :: IO CInt
ICAPI(getbegx)        c_getbegx       :: CWindowPtr -> IO Int
ICAPI(getbegy)        c_getbegy       :: CWindowPtr -> IO Int
ICAPI(getcurx)        c_getcurx       :: CWindowPtr -> IO Int
ICAPI(getcury)        c_getcury       :: CWindowPtr -> IO Int
ICAPI(getmaxy)        c_getmaxy       :: CWindowPtr -> IO Int
ICAPI(getmaxx)        c_getmaxx       :: CWindowPtr -> IO Int
ICAPI(getmouse)       c_getmouse      :: Ptr MEVENT -> IO CInt
ICAPI(initscr)        c_initscr       :: IO ()
ICAPI(init_pair)      c_init_pair     :: Int -> Int -> Int -> IO CInt
ICAPI(wrefresh)       c_wrefresh      :: CWindowPtr -> IO CInt
ICAPI(wnoutrefresh)   c_wnoutrefresh  :: CWindowPtr -> IO CInt
ICAPI(keypad)         c_keypad        :: CWindowPtr -> Bool -> IO CInt
ICAPI(meta)           c_meta          :: CWindowPtr -> Bool -> IO CInt
ICAPI(mouseinterval)  c_mouseinterval :: CInt -> IO CInt
ICAPI(mousemask)      c_mousemask     :: C_MMASK -> Ptr C_MMASK -> IO C_MMASK
ICAPI(mvwin)          c_mvwin         :: CWindowPtr -> Int -> Int -> IO CInt
ICAPI(newpad)         c_newpad        :: Int -> Int -> IO CWindowPtr
ICAPI(newwin)         c_newwin        :: Int -> Int -> Int -> Int -> IO CWindowPtr
ICAPI(nocbreak)       c_nocbreak      :: IO CInt
ICAPI(noecho)         c_noecho        :: IO CInt
ICAPI(noraw)          c_noraw         :: IO CInt
ICAPI(pnoutrefresh)   c_pnoutrefresh  :: CWindowPtr -> Int -> Int -> Int -> Int -> Int -> Int -> IO CInt
ICAPI(resizeterm)     c_resizeterm    :: Int -> Int -> CInt
ICAPI(subwin)         c_subwin        :: CWindowPtr -> Int -> Int -> Int -> Int -> IO CWindowPtr
ICAPI(start_color)    c_start_color   :: IO CInt
ICAPI(use_default_colors)   c_use_default_colors  :: IO CInt
ICAPI(touchwin)       c_touchwin      :: CWindowPtr -> IO CInt
ICAPI(untouchwin)     c_untouchwin    :: CWindowPtr -> IO CInt
ICAPI(waddwstr)       c_waddwstr      :: CWindowPtr -> CWString -> IO CInt
ICAPI(has_mouse)      c_hasmouse      :: IO Bool
foreign import capi interruptible "curses.h wget_wch" 
                           c_wget_wch :: CWindowPtr -> Ptr CInt -> IO CInt
foreign import capi interruptible "curses.h wgetnstr"
                      c_wgetnstr :: CWindowPtr -> CString -> Int -> IO CInt
-- foreign import capi safe "curses.h ungetch" c_ungetch :: Int -> IO CInt
ICAPI(ungetch)        c_ungetch       :: Int -> IO CInt
-- ICAPI(wget_wch)       c_wget_wch      :: CWindowPtr -> Ptr CInt -> IO CInt
-- ICAPI(wbkgd)         c_wbkgd        ::
-- ICAPI(wbkgrndset)    c_wbkgrndset   ::
-- ICAPI(wclear)        c_wclear       ::
-- ICAPI(wclrtoeol)     c_wclrtoeol    ::
-- ICAPI(wenclose)      c_wenclose     ::
-- ICAPI(wmove)         c_wmove        ::
-- ICAPI(wnoutrefresh)  c_wnoutrefresh ::
-- ICAPI(wrefresh)      c_wrefresh     ::
-- ICAPI(wresize)       c_wresize      :: CWindowPtr -> Int -> Int -> IO CInt
ICAPI(wtimeout)       c_wtimeout      :: CWindowPtr -> Int -> IO ()
-- ICAPI(wtouchln)      c_wtouchln     ::

-- Clearing
ICAPI(wclear)         c_wclear        :: CWindowPtr -> IO CInt
ICAPI(werase)         c_werase        :: CWindowPtr -> IO CInt
-- Drawing
ICAPI(wmove)          c_wmove         :: CWindowPtr -> Int -> Int -> IO CInt
ICAPI(waddnstr)       c_waddnstr      :: CWindowPtr -> CString -> Int
                                                    -> IO CInt
-- ICAPI(waddnwstr)      c_waddnwstr     :: CWindowPtr -> Int      -> Int 
--                                                     -> CWString -> Int
--                                                     -> IO CInt
ICAPI(wborder)        c_wborder       :: CWindowPtr -> CHType -> CHType 
                                                    -> CHType -> CHType 
                                                    -> CHType -> CHType 
                                                    -> CHType -> CHType 
                                                    -> IO CInt
-- WINDOW modification
ICAPI(wresize)        c_wresize       :: CWindowPtr -> Int -> Int -> IO CInt

-- PANEL creation/deletion 
ICAPIP(new_panel)     c_new_panel     :: CWindowPtr -> IO CPanelPtr
ICAPIP(del_panel)     c_del_panel     :: CPanelPtr -> IO CInt
-- PANEL movement
ICAPIP(move_panel)    c_move_panel    :: CPanelPtr -> Int -> Int -> IO CInt
ICAPIP(replace_panel) c_replace_panel :: CPanelPtr -> CWindowPtr -> IO CInt
ICAPIP(bottom_panel)  c_bottom_panel  :: CPanelPtr -> IO CInt
ICAPIP(top_panel)     c_top_panel     :: CPanelPtr -> IO CInt
-- PANEL visibility
ICAPIP(hide_panel)    c_hide_panel    :: CPanelPtr -> IO CInt
ICAPIP(show_panel)    c_show_panel    :: CPanelPtr -> IO CInt
-- PANEL retrieval
ICAPIP(panel_window)  c_panel_window  :: CPanelPtr -> IO CWindowPtr
ICAPIP(panel_hidden)  c_panel_hidden  :: CPanelPtr -> IO CInt
ICAPIP(panel_above)   c_panel_above   :: CPanelPtr -> IO CPanelPtr
ICAPIP(panel_below)   c_panel_below   :: CPanelPtr -> IO CPanelPtr
-- PANEL rendering
ICAPIP(update_panels) c_update_panels :: IO ()
