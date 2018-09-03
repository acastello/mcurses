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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
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
import System.Posix.Signals.Exts

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
    , ce_dimensions   :: Map Window Props
    , ce_iptr         :: Ptr CInt
    , ce_mptr         :: Ptr MEVENT
    , ce_hev          :: Maybe Input
    } 

instance Show CursesEnv where
    show = show . ce_colormap

getEnv :: Monad m => MC m CursesEnv 
getEnv = MC get

putEnv :: Monad m => CursesEnv -> MC m ()
putEnv = MC . put

modifyEnv :: Monad m => (CursesEnv -> CursesEnv) -> MC m ()
modifyEnv = MC . modify'

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
      iptr              mptr  Nothing               -- iptr    mptr     hev

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

newWindowUnder :: MonadIO m => Window -> Prop -> Prop -> Prop 
                               -> Prop -> MC m Window
newWindowUnder parent hd wd yd xd = do
    f <- calcPropsF parent
    let (h, w, y, d) = f (hd, wd, yd, xd)
    win <- io $ do winp <- DCALL(c_newwin h w y d)
                   panp <- DCALL(c_new_panel winp)
                   c_keypad winp True
                   return (Window (Right panp))
    underWindow_ (pure win :) parent
    MC $ modify (\e -> e { ce_dimensions = M.insert win (hd, wd, yd, xd) 
                                                    (ce_dimensions e) 
                         , ce_zorder = win : ce_zorder e })
    return win

newWindow :: MonadIO m => Prop -> Prop -> Prop -> Prop 
                                    -> MC m Window
newWindow = newWindowUnder stdWindow

newRegion :: MonadIO m => Window -> Prop -> Prop -> Prop 
                                 -> Prop -> MC m Window
newRegion parent hd wd yd xd = do
    f <- calcPropF parent
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

winTree :: Monad m => Window -> MC m (Maybe WinTree)
winTree win = ptree . ce_tree <$> getEnv
    where
    ptree n @ (Node w ws) | win == w  = Just n
                          | otherwise = L.foldl step Nothing ws
    step (Just t) _ = Just t
    step Nothing f = ptree f

parentTree :: Monad m => Window -> MC m (Maybe WinTree)
parentTree win | win == stdWindow = Just . Node stdWindow . (:[]) . ce_tree 
                                            <$> getEnv
               | otherwise        = ptree . ce_tree <$> getEnv
    where
    ptree n @ (Node _ ws) | win `L.elem` (rootLabel <$> ws)  = Just n
                          | otherwise = L.foldl step Nothing ws 
    step (Just t) _ = Just t
    step Nothing f = ptree f

parentWin :: Monad m => Window -> MC m (Maybe Window)
parentWin win = fmap rootLabel <$> parentTree win

childrenWins :: MonadIO m => Window -> MC m (T.Forest Window)
childrenWins win = maybe mempty id `liftM` underWindow (\x -> (x, x)) win

parentWin_ :: Monad m => Window -> MC m Window
parentWin_ w = maybe stdWindow id <$> parentWin w 

screenProps :: MonadIO m => MC m (Int, Int, Int, Int)
screenProps = do
    (h, w) <- ce_hw <$> getEnv
    return (h, w, 0, 0)

calcPropF :: MonadIO m => Window -> MC m (Prop -> Bool -> Int)
calcPropF pwin = io $ do
    win <- win_wptr pwin
    y0 <- c_getbegy win
    x0 <- c_getbegx win
    y1 <- c_getmaxy win
    x1 <- c_getmaxx win
    return $ calcPropFromAbs (y1 - y0, x1 - x0, y0, x0)

calcPropFromAbs :: Dimensions -> Prop -> Bool -> Int
calcPropFromAbs (h, w, _, _) d horz =
        let n = calcProp h w d
        in if horz then max 0 $ min w n else max 0 $ min h n 

calcPropsF :: MonadIO m => Window -> MC m (Props -> Dimensions)
calcPropsF parent = do
    abss <- windowDimensions parent
    return (calcPropsFromAbs abss)

calcPropsFromAbs :: Dimensions -> Props -> Dimensions
calcPropsFromAbs abss @ (ph, pw, py, px) (hd, wd, yd, xd) =
    let f = calcPropFromAbs abss
        (h, w, y, x) = (f hd False, f wd True, f yd False, f xd True)
    in (h, w, py + if y + h > ph then ph - h else y
            , px + if x + w > pw then pw - w else x)

setCursorMode :: MonadIO m => CursorMode -> MC m ()
setCursorMode mode = io $ do
    void $ c_curs_set (fromEnum mode)

moveCursor :: MonadIO m => Window -> Int -> Int -> MC m ()
moveCursor win y x = io $ do ptr <- win_wptr win
                             DRCCALL("wmove", c_wmove ptr y x)

drawByteString :: MonadIO m => Window -> ByteString -> MC m ()
drawByteString win str = io $ do
    ptr <- win_wptr win
    BS.useAsCStringLen str $ \(sptr, n) -> do
        DRCCALL("waddnstr", c_waddnstr ptr sptr n)

drawBorder :: MonadIO m => Window -> MC m ()
drawBorder win = io $ do
    winp <- win_wptr win
    void $ c_wborder winp 0 0 0 0 0 0 0 0

moveWindow :: MonadIO m => Window -> Prop -> Prop -> MC m ()
moveWindow win yp xp = do
    adjustProps win (\(h, w, _, _) -> (h, w, yp, xp))
    readjustWin win

resizeWindow :: MonadIO m => Window -> Prop -> Prop -> MC m ()
resizeWindow win h w = do
    adjustProps win (\(_, _, y, x) -> (h, w, y, x))
    readjustWin win

recalcWin :: MonadIO m => Window -> MC m ()
recalcWin win | win == stdWindow  = undefined
              | otherwise         = parentTree win >>= mapM_ undefined

io_movewindow :: Window -> Int -> Int -> IO ()
io_movewindow w y x = case win_pointer w of
    Left wptr -> DRCCALL("mvwin", c_mvwin wptr y x)
    Right pptr -> DRCCALL("move_panel", c_move_panel pptr y x)

adjustProps :: Monad m => Window -> (Props -> Props) -> MC m ()
adjustProps w f = MC $ modify' $ 
        \e -> e { ce_dimensions = M.adjust f w (ce_dimensions e) }

readjustWin :: MonadIO m => Window -> MC m ()
readjustWin top = do mnode <- parentTree top
                     forM_ mnode $ \(Node w ws) -> do
                         f <- calcPropsF w 
                         rtree f `mapM_` (L.find ((== top) . rootLabel) ws)
    where 
    rtree f (Node win ws) = do
        (h,  w,  y,  x) <- windowDimensions win
        ndims @ (nh, nw, ny, nx) <- f <$> windowProps win
        when (y /= ny || x /= nx) $ io $ do
            io_movewindow win ny nx

        when (h /= nh || w /= nw) $ io $ do
            ptr <- win_wptr win
            DRCCALL("wresize", c_wresize ptr nh nw)
                
        forM_ ws (rtree (calcPropsFromAbs ndims))


adjustSize :: MonadIO m => (Props -> Dimensions) -> Window -> MC m Dimensions
adjustSize f win = MC $ get >>= \cenv -> io $ do
    let dims = maybe (Height, Width, 0, 0) id 
                     (M.lookup win (ce_dimensions cenv))
    return (f dims)

windowProps :: MonadIO m => Window -> MC m Props
windowProps w = do pmap <- ce_dimensions <$> getEnv
                   return (maybe (Height, Width, 0, 0) id (M.lookup w pmap))

windowDimensions :: MonadIO m => Window -> MC m Dimensions
windowDimensions win = io $ do p <- win_wptr win 
                               liftM4 (,,,) (c_getmaxy p) (c_getmaxx p)
                                            (c_getbegy p) (c_getbegx p)

getFocus :: Monad m => MC m Window
getFocus = maybe stdWindow fst . L.uncons . ce_zorder <$> getEnv

checkInput :: MonadIO m => CWindowPtr -> MC m (Maybe Input)
checkInput wptr = MC $ get >>= \cenv -> io $ do
    rc <- DCALL(c_wget_wch wptr (ce_iptr cenv))
    code <- fi <$> peek (ce_iptr cenv)
    PUTS("rc, code = " ++ show (rc, code))
    if rc == c_ERR then
        return Nothing
    else do
        liftM Just $ 
            if rc == 0 then
                parseChar code 
            else if code == c_KEY_MOUSE then
                parseMouse (ce_mptr cenv)
            else if code == c_KEY_RESIZE then
                return ScreenResized
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

getInput :: MonadIO m => Maybe Int -> MC m (Maybe Input)
getInput (Just n) | n < 0 = getInput Nothing
getInput mdelay = do
    cenv <- getEnv
    case ce_hev cenv of
      Just ev -> do putEnv cenv { ce_hev = Nothing }
                    return (Just ev)
      Nothing -> do
        wptr <- io $ win_wptr (maybe stdWindow fst $ L.uncons $ ce_zorder cenv)
        io $ DCALL(c_wtimeout wptr (maybe (-1) id mdelay)) 
        in0 <- checkInput wptr
        if in0 == Just ScreenResized || maybe False (< 250) mdelay then do
            when (in0 == Just ScreenResized) $ do
                readjustWin stdWindow
            return in0
        else do 
            io $ DCALL(c_wtimeout wptr 0)
            in1 <- checkInput wptr
            if in1 == Just ScreenResized then do
                putEnv cenv { ce_hev = in0 }
                readjustWin stdWindow
                return in1
            else if isNothing in1 then
                return in0
            else do
                putEnv cenv { ce_hev = in1 }
                return in0
            
waitInputUpTo :: MonadIO m => Int -> MC m (Maybe Input)
waitInputUpTo n = getInput (Just n)

waitInput :: MonadIO m => MC m Input
waitInput = maybe (fail "waitInput: Nothing") return =<< getInput Nothing

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
    moveWindow win 4 8
    render
    getInput (Just 500)
    moveWindow win 5 (Width/2)
    render
    waitInput
    render
    waitInput
    render
    waitInput

testResize :: MC IO ()
testResize = do
    win <- newWindow (Height / 2) (Width / 2) (Height / 4) (Width / 4)
    drawBorder win
    moveCursor win 1 1 
    drawByteString win "123456789abcdef123456789abcdef"
    render
    testEvs

testEvs :: MC IO ()
testEvs = step 
    where
    step = do
        render 
        mev <- getInput (Just 2000)
        when (mev /= Just (CharPress 'q')) $ do
            forM mev $ \ev -> PUTS(show ev)
            step

-- testraw :: IO ()
testraw = do
    DCALL(c_initscr)
    DCALL(c_start_color)
    DCALL(c_use_default_colors)
    DCALL(c_noecho)
    DCALL(c_curs_set 0)
    DCALL(c_cbreak)
    DCALL(c_wtimeout c_stdscr (-1))
    DCALL(c_keypad c_stdscr True)
    DCALL(c_meta c_stdscr True)
    r <- alloca $ \ptr -> replicateM 5 $ do ev <- c_wget_wch c_stdscr ptr
                                            code <- peek ptr
                                            return (ev, code)

    DCALL(c_endwin)
    return r
