{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, 
    CApiFFI, TemplateHaskell #-}

module UI.MCurses
  ( module UI.MCurses 
  , module UI.MCurses.Types
  , liftIO 
  , MonadTrans (..), 
  ) where

#ifdef DEBUG
#define DCALL(FCALL) ($([|FCALL|] >>= formatCall) >>= \(str,r) -> \
                        (liftIO $ hPutStrLn stderr (padFormatCall $ words str)) >> return r)
#define PUTS(STR) (liftIO $ hPutStrLn stderr (STR))
#else
#define DCALL(FCALL) (FCALL)
#define PUTS(STR) (return ())
#endif
#define DRCCALL(NAME, FCALL) (checkRC NAME =<< DCALL(FCALL))

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Reader 
import Control.Monad.State

import Data.Bifunctor
import Data.Bits
import Data.Char
-- import Data.Either
import Data.List as L
import Data.Maybe
import Data.Map as M
import Data.Tree as T

import Foreign hiding (void)
import Foreign.C

import System.IO
import System.Posix.Signals

import UI.MCurses.Internal
#ifdef DEBUG
import UI.MCurses.Internal.TH
#endif
import UI.MCurses.Types

data CursesEnv = CursesEnv 
    { ce_tree         :: Tree Window 
    , ce_zorder       :: [Window]
    , ce_ncolors      :: Int
    , ce_npairs       :: Int
    , ce_colormap     :: [((Color, Color), Int)]
    , ce_hw           :: (Int, Int)
    , ce_dimensions   :: Map Window (Dimension, Dimension, Dimension, Dimension)
    , ce_iptr         :: Ptr CInt
    , ce_mptr         :: Ptr MEVENT
    } 

instance Show CursesEnv where
    show = show . ce_colormap

getEnv :: Monad m => MC m CursesEnv 
getEnv = MC get

initCursesEnv :: IO CursesEnv
initCursesEnv = do
    ncolors <- fi <$> peek c_COLORS
    npairs <- fi <$> peek c_COLOR_PAIRS
    width <- fi <$> peek c_COLS
    height <- fi <$> peek c_LINES
    iptr <- new c_ERR
    mptr <- calloc
    return $ CursesEnv
      (pure stdWindow)  []    ncolors               -- tree    zorder   ncolors
      npairs            []    (height, width)       -- npairs  colormap hw
      (M.singleton stdWindow (Height, Width, 0, 0)) -- dimensions
      iptr              mptr                        -- iptr    mptr

data KeyboardInterrupt = KeyboardInterrupt
    deriving (Show)

instance Exception KeyboardInterrupt

newtype CursesFailed = CursesFailed String
    deriving (Show)

instance Exception CursesFailed 

getPairID :: MonadIO m => Color -> Color -> MC m ColorID
getPairID DefaultColor DefaultColor = return 0
getPairID fore back = do
    env @ CursesEnv { ce_npairs = npairs, ce_colormap = cmap } <- getEnv
    case L.lookup (fore, back) cmap of
        Just i -> return i
        Nothing -> do
            let lcmap = L.length cmap
                i = if lcmap < npairs then 
                        lcmap + 1
                    else if npairs > 0 then
                        snd $ L.last cmap
                    else 
                        0
            when (i > 0) $ do
                io $ DRCCALL("getPairID, init_pair",
                             c_init_pair i (fromEnum fore) (fromEnum back))
                let newcmap = if lcmap < npairs then
                                  ((fore, back), i) : cmap
                              else
                                  ((fore, back), i) : L.init cmap
                MC $ put env { ce_colormap = newcmap }
            return i

newtype MC m a = MC { unCurses :: StateT CursesEnv m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadThrow, MonadCatch, 
              MonadMask)

instance MonadIO m => MonadIO (MC m) where
    liftIO = MC . liftIO

instance MonadTrans MC where
    lift = MC . lift

mapMC :: (m (a, s) -> n (b, s)) -> MC m a -> MC n b
mapMC = undefined

runMC :: (MonadIO m, MonadMask m) => MC m a -> m a
runMC op = bracket pre post curs 
    where
    curs _ = do
        io $ do 
            DCALL(c_initscr)
            DRCCALL("runMC, wclear",      c_wclear c_stdscr)
            DRCCALL("runMC, start_color", c_start_color)
            DRCCALL("runMC, use_default_colors", c_use_default_colors)
            DRCCALL("runMC, noecho",      c_noecho)
            DRCCALL("runMC, curs_set",    c_curs_set (0 :: Int))
            DCALL(c_mouseinterval (50 :: CInt))
            DRCCALL("runMC, cbreak",      c_cbreak)
            DCALL(c_mousemask (c_ALL_MOUSE_EVENTS .|. c_REPORT_MOUSE_POSITION) nullPtr)
            DRCCALL("runMC, keypad",      c_keypad c_stdscr True)
            DRCCALL("runMC, meta",        c_meta c_stdscr True)
        evalStateT cleanUpAfter =<< io initCursesEnv
    cleanUpAfter = do r <- unCurses op
                      cenv <- get
                      forM_ (ce_tree cenv) (unCurses . delWindow)
                      return r
    pre = io $ do tid <- myThreadId
                  installHandler keyboardSignal 
                      (Catch $ throwTo tid KeyboardInterrupt) Nothing
    post h = io $ do installHandler keyboardSignal h Nothing
                     c_endwin

render :: MonadIO m => MC m ()
render = void $ MC $ io $ do
--     c_wnoutrefresh c_stdscr
    c_update_panels
    c_doupdate

newWindowUnder :: MonadIO m => Window -> Dimension -> Dimension -> Dimension 
                               -> Dimension -> MC m Window
newWindowUnder parent hd wd yd xd = do
    f <- calcDimensionF parent
    win <- io $ do winp <- c_newwin (f hd False) (f wd True) 
                                    (f yd False) (f xd True)
                   panp <- c_new_panel winp
                   c_keypad winp True
                   return (Window (Right panp))
    underWindow_ (pure win :) parent
    MC $ modify (\e -> e { ce_dimensions = M.insert win (hd, wd, yd, xd) 
                                                    (ce_dimensions e) 
                         , ce_zorder = win : ce_zorder e })
    return win

newWindow :: MonadIO m => Dimension -> Dimension -> Dimension -> Dimension 
                                    -> MC m Window
newWindow = newWindowUnder stdWindow

newRegion :: MonadIO m => Window -> Dimension -> Dimension -> Dimension 
                                 -> Dimension -> MC m Window
newRegion parent hd wd yd xd = do
    f <- calcDimensionF parent
    win <- io $ do pwinp <- win_wptr parent
                   winp <- c_subwin pwinp (f hd False) (f wd True)
                                          (f yd False) (f xd True)
                   return (Window $ Left winp)
    underWindow_ (pure win :) parent
    MC $ modify' (\e -> e { ce_dimensions = M.insert win (hd, wd, yd, xd)
                                                         (ce_dimensions e)
                          , ce_zorder = win : ce_zorder e }) 
    return win

delWindow :: MonadIO m => Window -> MC m ()
delWindow win 
  | win == stdWindow = return ()
  | otherwise = do
      mall_wins <- withWindow (\x -> (x, Nothing)) win
      forM_ mall_wins $ mapM_ $ \w -> do
          io $ case win_pointer w of
              Left wp -> DRCCALL("delWindow, c_delwin", c_delwin wp)
              Right pp -> do wp <- c_panel_window pp
                             DRCCALL("delWindow, c_del_panel", c_del_panel pp)
                             DRCCALL("delWindow, c_delwin", c_delwin wp)
          MC $ modify' $ \ce -> 
              ce { ce_zorder = L.delete w (ce_zorder ce) }

stdWindow :: Window 
stdWindow = Window (Left c_stdscr) 

findUnder :: Monad m => Window -> MC m (Forest Window)
findUnder win = MC $ (maybe [] id . under . ce_tree <$> get) 
    where under (Node win' st) | win == win' = Just st
                               | otherwise   = L.foldl step Nothing st
          step (Just x) _ = Just x
          step Nothing nod = under nod

withWindow :: MonadIO m => (Tree Window -> (b, Maybe (Tree Window))) 
                         -> Window -> MC m (Maybe b)
withWindow f win = MC $ do
    st @ CursesEnv { ce_tree = tree } <- get
    let (e, mnewtree) = go tree
        newtree = maybe tree id mnewtree
    put st { ce_tree = newtree }
    return e
    where
 -- go :: Tree Window -> (Maybe b, Maybe (Tree Window))
    go t @ (T.Node x ys)
      | x == win  = first Just (f t)
      | otherwise = second (Just . T.Node x) (accumgo ys)
 -- accumgo :: T.Forest Window -> (Maybe b, T.Forest Window)
    accumgo ys = 
        let (e', mforest) = mapAccumR (\e t -> if not (isNothing e) then
                                                   (e, Just t)
                                               else
                                                   go t) Nothing ys
        in (e', catMaybes mforest)

withWindow_ :: MonadIO m => (Tree Window -> Maybe (Tree Window)) -> Window 
               -> MC m ()
withWindow_ f = void . withWindow ((,) () . f)

underWindow :: MonadIO m => (T.Forest Window -> (b, T.Forest Window)) 
                          -> Window -> MC m (Maybe b)
underWindow f = withWindow (\t -> (\fo -> Just t { subForest = fo }) 
                <$> (f $ subForest t))

underWindow_ :: MonadIO m => (T.Forest Window -> T.Forest Window) -> Window -> MC m ()
underWindow_ f = void . underWindow ((,) () . f)

deg :: Eq a => a -> Tree a -> (Tree a -> (b, Maybe (Tree a))) -> (Maybe b, Tree a)
deg win tree f = let (e, st) = accumgo (subForest tree) in (e, tree { subForest = st })
    where
 -- go :: Tree Window -> (Maybe b, Maybe (Tree Window))
    go t @ (T.Node x ys)
      | x == win  = first Just (f t)
      | otherwise = second (Just . T.Node x) (accumgo ys)
 -- accumgo :: T.Forest Window -> (Maybe b, T.Forest Window)
    accumgo ys = 
        let (e', mforest) = mapAccumR (\e t -> if not (isNothing e) then
                                                   (e, Just t)
                                               else
                                                   go t) Nothing ys
        in (e', catMaybes mforest)

underdeg :: Eq a => a -> Tree a -> (Forest a -> (b, Forest a)) -> (Maybe b, Tree a)
underdeg win tree f = deg win tree (\t -> (\fo -> Just t { subForest = fo })
                      <$> (f $ subForest t))

parentTree :: Monad m => Window -> MC m (Maybe WinTree)
parentTree win = ptree . ce_tree <$> getEnv
    where
    ptree n @ (Node w ws) | win `L.elem` (rootLabel <$> ws)  = Just n
                          | otherwise = L.foldl step Nothing ws 
    step (Just t) _ = Just t
    step Nothing f = ptree f
parentWin :: MonadIO m => Window -> MC m (Maybe Window)
parentWin win = do
    if win == stdWindow then
        return Nothing
    else do
        s <- getEnv
        return (go $ ce_tree s)
    where
    go (T.Node x ys) = 
        if win `elem` (T.rootLabel <$> ys) then 
            Just x 
        else 
            join $ listToMaybe (go <$> ys)

childrenWins :: MonadIO m => Window -> MC m (T.Forest Window)
childrenWins win = maybe mempty id `liftM` underWindow (\x -> (x, x)) win

parentWin_ :: MonadIO m => Window -> MC m Window
parentWin_ w = maybe stdWindow id <$> parentWin w 

screenDimensions :: MonadIO m => MC m (Int, Int, Int, Int)
screenDimensions = do
    (h, w) <- ce_hw <$> getEnv
    return (h, w, 0, 0)

calcDimensionF :: MonadIO m => Window -> MC m (Dimension -> Bool -> Int)
calcDimensionF pwin = io $ do
    win <- win_wptr pwin
    y0 <- c_getbegy win
    x0 <- c_getbegx win
    y1 <- c_getmaxy win
    x1 <- c_getmaxx win
    return $ calcDimensionFromAbs (y1 - y0, x1 - x0, y0, x0)

calcDimensionFromAbs :: Absolutes -> Dimension -> Bool -> Int
calcDimensionFromAbs (h, w, _, _) d horz =
        let n = calcDimension h w d
        in if horz then max 0 $ min w n else max 0 $ min h n 

calcDimensionsF :: MonadIO m => Window -> MC m (Dimensions -> Absolutes)
calcDimensionsF parent = do
    abss <- windowDimensions parent
    return (calcDimensionsFromAbs abss)

calcDimensionsFromAbs :: Absolutes -> Dimensions -> Absolutes
calcDimensionsFromAbs abss @ (ph, pw, py, px) (hd, wd, yd, xd) =
    let f = calcDimensionFromAbs abss
        (h, w, y, x) = (f hd False, f wd True, f yd False, f xd True)
    in (h, w, py + if y + h > ph then ph - h else y
            , px + if x + w > pw then pw - w else x)

setCursorMode :: MonadIO m => CursorMode -> MC m ()
setCursorMode mode = io $ do
    void $ c_curs_set (fromEnum mode)

drawBorder :: MonadIO m => Window -> MC m ()
drawBorder win = io $ do
    winp <- win_wptr win
    void $ c_wborder winp 0 0 0 0 0 0 0 0

moveWindow :: MonadIO m => Window -> Dimension -> Dimension -> MC m ()
moveWindow win ydim xdim = do
    parent <- maybe stdWindow id <$> parentWin win
    (ph, pw, py, px) <- windowDimensions parent
    MC $ modify' $ \cenv @ CursesEnv { ce_dimensions = cedims } -> 
        cenv { ce_dimensions = 
                  M.adjust (\(ceH, ceW, _, _) -> (ceH, ceW, ydim, xdim)) 
                           win cedims }
    (_, _, y, x) <- windowDimensions win
    io $ case win_pointer win of
        Left wptr -> DRCCALL("mvwin", c_mvwin wptr y x)
        Right pptr -> DRCCALL("move_panel", c_move_panel pptr y x)

resizeWindow :: MonadIO m => Window -> Dimension -> Dimension -> MC m ()
resizeWindow win h w = MC $ do
    return $ undefined win h w

adjustSize :: MonadIO m => (Dimensions -> Absolutes) -> Window -> MC m Absolutes
adjustSize f win = MC $ get >>= \cenv -> io $ do
    let dims = maybe (Height, Width, 0, 0) id 
                     (M.lookup win (ce_dimensions cenv))
    return (f dims)

windowDimensions :: MonadIO m => Window -> MC m (Int, Int, Int, Int)
windowDimensions win
  | win == stdWindow = screenDimensions
  | otherwise = do
        f <- calcDimensionsF =<< parentWin_ win
        dimmap <- ce_dimensions <$> getEnv
        let dims = maybe (Height, Width, 0, 0) id $ M.lookup win dimmap
        return (f dims)

getInput :: MonadIO m => Maybe Int -> MC m (Maybe Input)
getInput mdelay = MC $ get >>= \cenv -> io $ do
    wptr <- win_wptr (L.head $ ce_zorder cenv)
    DCALL(c_wtimeout wptr (maybe (-1) id mdelay)) 
    rc <- c_wget_wch wptr (ce_iptr cenv)
    if rc == c_ERR then
        return Nothing
    else do
        code <- fi <$> peek (ce_iptr cenv)
        liftM Just $ 
            if code == c_KEY_MOUSE then
                parseMouse (ce_mptr cenv)
            else if code == c_KEY_RESIZE then
                return ScreenResized
            else if rc == 0 then
                parseChar code 
            else
                parseKey code
    where
    parseKey :: Int -> IO Input
    parseKey code = return (KeyPress code)
    parseMouse :: Ptr MEVENT -> IO Input
    parseMouse mptr = do c_getmouse mptr
                         MEVENT _ x y _ b <- peek mptr
                         return $ MouseInput (Click (fi x) (fi y) b)
    parseChar :: Int -> IO Input
    parseChar code = return (CharPress $ chr $ code)
            
waitInput :: MonadIO m => MC m Input
waitInput = maybe (fail "getInput: Nothing") return =<< getInput Nothing

checkRC :: MonadIO m => String -> CInt -> m ()
checkRC name code
  | code == c_ERR = fail $ name ++ ": return code = ERR"
  | otherwise = return ()

io :: MonadIO m => IO a -> m a
io = liftIO

test1 :: MC IO Input
test1 = do
    win <- newWindow (Height/2) (Width/2) 2 2
    drawBorder win 
    render
    getInput (Just 500)
    moveWindow win 4 0
    render
    getInput (Just 500)
    moveWindow win 5 8
    render
    waitInput

testResize :: MC IO Input
testResize = do
    win <- newWindow (Height - 4) (Width - 4) 2 2
    drawBorder win
    render
    getInput (Just 1000)
    moveWindow win 3 3
    render
    waitInput

main :: IO ()
main = print =<< runMC test1
