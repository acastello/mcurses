{-# LANGUAGE  CPP                           #-} 
{-# LANGUAGE  GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE  CApiFFI                       #-}
{-# LANGUAGE  TemplateHaskell               #-}
{-# LANGUAGE  RankNTypes                    #-}

module UI.MCurses
  ( module UI.MCurses 
  , module UI.MCurses.Types
  , liftIO 
  , MonadTrans (..)
  , MonadMask 
  ) where

#ifdef DEBUG
#define DCALL(FCALL) ($([|FCALL|] >>= formatCall) >>= \(_str,_r) -> \
          (liftIO $ hPutStrLn stderr (padFormatCall $ words _str)) >> return _r)
#define PUTS(STR) (liftIO $ hPutStrLn stderr (STR))
#else
#define DCALL(FCALL) (FCALL)
#define PUTS(STR) (return ())
#endif
#define DRCCALL(NAME, FCALL) (checkRC NAME =<< DCALL(FCALL))

import Control.Concurrent
-- import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader 
import Control.Monad.State

import Data.Bifunctor
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
-- import Data.Either
import Data.IORef
import Data.List as L
import Data.Maybe
import Data.Map as M
import Data.Tree as T

import Foreign hiding (void)
import Foreign.C

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import System.IO
import System.IO.Unsafe
import System.Posix.Signals
import System.Posix.Signals.Exts

import UI.MCurses.Internal
#ifdef DEBUG
import UI.MCurses.Internal.TH
#endif
import UI.MCurses.Types

data WindowStatus = WindowStatus
    { _ws_rdims  :: RDimensions
    , _ws_dims   :: Dimensions
    } 
makeLenses ''WindowStatus

data CursesEnv = CursesEnv 
    { _ce_tree         :: Tree Window 
    , _ce_zorder       :: [Window]
    , _ce_ncolors      :: Int
    , _ce_npairs       :: Int
    , _ce_colormap     :: [((Color, Color), Int)]
    , _ce_hw           :: (Int, Int)
    , _ce_statuses     :: Map Window WindowStatus
    , _ce_iptr         :: Ptr CInt
    , _ce_mptr         :: Ptr MEVENT
    , _ce_finis        :: IORef [Either CWindowPtr CPanelPtr]
    , _ce_heldev       :: Maybe Input
    } 
makeLenses ''CursesEnv

instance Show CursesEnv where
    show = show . _ce_colormap

getEnv :: Monad m => MC m CursesEnv 
getEnv = MC get

putEnv :: Monad m => CursesEnv -> MC m ()
putEnv = MC . put

modifyEnv :: Monad m => (CursesEnv -> CursesEnv) -> MC m ()
modifyEnv = MC . modify'

windowStatus :: Monad m => Window -> MC m WindowStatus
windowStatus win = getEnv >>= \ce -> case M.lookup win (_ce_statuses ce) of
    Nothing -> fail $ "accessing undefined window: " ++ show win
    Just ws -> return ws 

-- modifyWindowStatus :: Monad m => Window -> (WindowStatus -> WindowStatus) -> MC m ()
-- modifyWindowStatus win f = MC $ modify' $ 
--           \e -> e { _ce_statuses = f (_ce_statuses e) }

initCursesEnv :: IO CursesEnv
initCursesEnv = do
    ncolors <- fi `liftM` peek c_COLORS
    npairs <-  fi `liftM` peek c_COLOR_PAIRS
    width <-   fi `liftM` peek c_COLS
    height <-  fi `liftM` peek c_LINES
    let wstatuses = M.singleton stdWindow (WindowStatus undefined undefined)
    iptr <- new c_ERR
    mptr <- calloc
    ioref <- newIORef []
    return $ CursesEnv
      (pure stdWindow)  []       ncolors           -- tree     zorder   ncolors
      npairs            []       (height, width)   -- npairs   colormap hw
      wstatuses         iptr     mptr              -- statuses iptr     mptr
      ioref             Nothing                    -- finis    heldev

data KeyboardInterrupt = KeyboardInterrupt
    deriving (Show)

instance Exception KeyboardInterrupt

newtype CursesFailed = CursesFailed String
    deriving (Show)

instance Exception CursesFailed 

getPairID :: MonadIO m => Color -> Color -> MC m ColorID
getPairID DefaultColor DefaultColor = return 0
getPairID fore back = do
    env @ CursesEnv { _ce_npairs = npairs, _ce_colormap = cmap } <- getEnv
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
                ce_colormap .= newcmap
            return i

newtype MC m a = MC { unCurses :: StateT CursesEnv m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadState CursesEnv,
              MonadThrow, MonadCatch, MonadMask)

instance MonadIO m => MonadIO (MC m) where
    liftIO = MC . liftIO

instance MonadTrans MC where
    lift = MC . lift

mapMC :: (m (a, s) -> n (b, s)) -> MC m a -> MC n b
mapMC = undefined

runMC :: (MonadIO m, MonadMask m) => MC m a -> m a
runMC op = bracket pre post curs 
    where
    curs (_,env) = do
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
        evalStateT (unCurses op) env
    pre = io $ do tid <- myThreadId
                  h <- installHandler keyboardSignal 
                      (Catch $ throwTo tid KeyboardInterrupt) Nothing
                  (,) h `liftM` initCursesEnv
    post (h,ce) = io $ do installHandler keyboardSignal h Nothing
                          finisl <- readIORef (_ce_finis ce) 
                          mapM_ (either c_delwin c_del_panel) finisl
                          c_endwin

render :: MonadIO m => MC m ()
render = void $ MC $ io $ do
--     c_wnoutrefresh c_stdscr
    c_update_panels
    c_doupdate

newWindowUnder :: MonadIO m => Window -> RDimension -> RDimension -> RDimension 
                               -> RDimension -> MC m Window
newWindowUnder parent hd wd yd xd = do
    cenv <- getEnv
    f <- calcRDimensionsF parent
    let rdims = (hd, wd, yd, xd)
    let dims @ (h, w, y, d) = f rdims
    win <- io $ do winp <- DCALL(c_newwin h w y d)
                   panp <- DCALL(c_new_panel winp)
                   c_keypad winp True
                   modifyIORef (_ce_finis cenv) ([Left winp, Right panp] ++)
                   return (Window (Right panp))
    underWindow_ (pure win :) parent
    MC $ do ce_statuses %= M.insert win (WindowStatus rdims dims)
            ce_zorder %= (win :)

    return win

newWindow :: MonadIO m => RDimension -> RDimension -> RDimension -> RDimension 
                                    -> MC m Window
newWindow = newWindowUnder stdWindow

newRegion :: MonadIO m => Window -> RDimension -> RDimension -> RDimension 
                                 -> RDimension -> MC m Window
newRegion parent hd wd yd xd = do
    f <- calcRDimensionsF parent
    let rdims = (hd, wd, yd, xd)
        dims @ (h,w,y,x) = f rdims
    swin <- io $ do pwinp <- win_wptr parent
                    swinp <- DCALL(c_subwin pwinp h w y x)
                    return (Window $ Left swinp)
    underWindow_ (pure swin :) parent
    MC $ do ce_statuses %= M.insert swin (WindowStatus rdims dims)
            ce_zorder %= (swin :)
    return swin

delWindow :: MonadIO m => Window -> MC m ()
delWindow win 
  | win == stdWindow = return ()
  | otherwise = do
      mall_wins <- withWindow (\x -> (x, Nothing)) win
      finis <- _ce_finis `liftM` getEnv
      forM_ mall_wins $ mapM_ $ \w -> do
          io $ case win_pointer w of
              Left wp -> do DRCCALL("delWindow, c_delwin", c_delwin wp)
                            modifyIORef finis (L.delete $ Left wp)
              Right pp -> do wp <- c_panel_window pp
                             DRCCALL("delWindow, c_del_panel", c_del_panel pp)
                             DRCCALL("delWindow, c_delwin", c_delwin wp)
                             modifyIORef finis (L.\\ [Left wp, Right pp])
          ce_zorder %= L.delete w


stdWindow :: Window 
stdWindow = Window (Left c_stdscr) 

findUnder :: Monad m => Window -> MC m (Forest Window)
findUnder win = MC $ (maybe [] id . under . _ce_tree <$> get) 
    where under (Node win' st) | win == win' = Just st
                               | otherwise   = L.foldl step Nothing st
          step (Just x) _ = Just x
          step Nothing nod = under nod

withWindow :: MonadIO m => (Tree Window -> (b, Maybe (Tree Window))) 
                         -> Window -> MC m (Maybe b)
withWindow f win = MC $ do
    st @ CursesEnv { _ce_tree = tree } <- get
    let (e, mnewtree) = go tree
        newtree = maybe tree id mnewtree
    put st { _ce_tree = newtree }
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
winTree win = ptree . _ce_tree <$> getEnv
    where 
    ptree n @ (Node w ws) | win == w  = Just n
                          | otherwise = L.foldl step Nothing ws
    step (Just t) _ = Just t
    step Nothing f = ptree f

parentTree :: Monad m => Window -> MC m (Maybe WinTree)
parentTree win | win == stdWindow = Just . Node stdWindow . (:[]) . _ce_tree 
                                            <$> getEnv
               | otherwise        = ptree . _ce_tree <$> getEnv
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

screenRDimensions :: MonadIO m => MC m (Int, Int, Int, Int)
screenRDimensions = do
    (h, w) <- _ce_hw <$> getEnv
    return (h, w, 0, 0)

calcRDimensionF :: MonadIO m => Window -> MC m (RDimension -> Bool -> Int)
calcRDimensionF pwin = io $ do
    win <- win_wptr pwin
    y0 <- c_getbegy win
    x0 <- c_getbegx win
    y1 <- c_getmaxy win
    x1 <- c_getmaxx win
    return $ calcRDimensionFromAbs (y1 - y0, x1 - x0, y0, x0)

calcRDimensionFromAbs :: Dimensions -> RDimension -> Bool -> Int
calcRDimensionFromAbs (h, w, _, _) d horz =
        let n = calcRDimension h w d
        in if horz then max 0 $ min w n else max 0 $ min h n 

calcRDimensionsF :: MonadIO m => Window -> MC m (RDimensions -> Dimensions)
calcRDimensionsF parent = do
    abss <- windowDimensions parent
    return (calcRDimensionsFromAbs abss)

calcRDimensionsFromAbs :: Dimensions -> RDimensions -> Dimensions
calcRDimensionsFromAbs abss @ (ph, pw, py, px) (hd, wd, yd, xd) =
    let f = calcRDimensionFromAbs abss
        (h, w, y, x) = (f hd False, f wd True, f yd False, f xd True)
    in (h, w, py + if y + h > ph then ph - h else y
            , px + if x + w > pw then pw - w else x)

setCursorMode :: MonadIO m => CursorMode -> MC m ()
setCursorMode mode = io $ do
    void $ c_curs_set (fromEnum mode)

moveCursor :: MonadIO m => Window -> Int -> Int -> MC m ()
moveCursor win y x = io $ do ptr <- win_wptr win
                             DRCCALL("wmove", c_wmove ptr y x)

erase :: MonadIO m => Window -> MC m ()
erase win = io $ do ptr <- win_wptr win
                    DRCCALL("werase", c_werase ptr)
drawByteString :: MonadIO m => Window -> ByteString -> MC m ()
drawByteString win str = io $ do
    ptr <- win_wptr win
    BS.useAsCStringLen str $ \(sptr, n) -> do
        DRCCALL("waddnstr", c_waddnstr ptr sptr n)

drawBorder :: MonadIO m => Window -> MC m ()
drawBorder win = io $ do
    winp <- win_wptr win
    void $ c_wborder winp 0 0 0 0 0 0 0 0

newtype Editing a = Editing (StateT WindowStatus (ReaderT Window IO) a)
    deriving (Functor, Applicative, Monad, MonadIO,
              MonadState WindowStatus, MonadReader Window)

onWindow :: MonadIO m => Window -> Editing a -> MC m a
onWindow win (Editing ops) = do 
    stu <- windowStatus win
    (x, stu') <- liftIO $ runReaderT (runStateT ops stu) win
    ce_statuses %= M.insert win stu'
    when (_ws_rdims stu' /= _ws_rdims stu) $ do
        readjustWin win
    return x 

infixr 0 ~>
(~>) :: MonadIO m => Window -> Editing a -> MC m a
(~>) = onWindow

move :: RDimension -> RDimension -> Editing ()
move y x = do 
    wptr <- io . win_wptr =<< ask
    ws_rdims._3 .= y
    ws_rdims._4 .= x

moveWindow :: MonadIO m => Window -> RDimension -> RDimension -> MC m ()
moveWindow win yp xp = do
    adjustRDimensions win (\(h, w, _, _) -> (h, w, yp, xp))
    readjustWin win

resizeWindow :: MonadIO m => Window -> RDimension -> RDimension -> MC m ()
resizeWindow win h w = do
    adjustRDimensions win (\(_, _, y, x) -> (h, w, y, x))
    readjustWin win

recalcWin :: MonadIO m => Window -> MC m ()
recalcWin win | win == stdWindow  = undefined
              | otherwise         = parentTree win >>= mapM_ undefined

adjustRDimensions :: Monad m => Window -> (RDimensions -> RDimensions) -> MC m ()
adjustRDimensions w f = do
    ce_statuses %= M.adjust (ws_rdims %~ f) w

readjustWin :: MonadIO m => Window -> MC m ()
readjustWin top = do mnode <- parentTree top
                     forM_ mnode $ \(Node w ws) -> do
                         f <- calcRDimensionsF w 
                         rtree f `mapM_` (L.find ((== top) . rootLabel) ws)
    where 
    rtree f (Node win ws) = do
        (h,  w,  y,  x) <- windowDimensions win
        ndims @ (nh, nw, ny, nx) <- f <$> windowRDimensions win
        when (y /= ny || x /= nx) $ io $ do
            io_movewindow win ny nx

        when (h /= nh || w /= nw) $ io $ do
            ptr <- win_wptr win
            DRCCALL("wresize", c_wresize ptr nh nw)
                
        forM_ ws (rtree (calcRDimensionsFromAbs ndims))

windowRDimensions :: MonadIO m => Window -> MC m RDimensions
windowRDimensions w = _ws_rdims `liftM` windowStatus w 

windowDimensions :: MonadIO m => Window -> MC m Dimensions
windowDimensions win = io $ do p <- win_wptr win 
                               liftM4 (,,,) (c_getmaxy p) (c_getmaxx p)
                                            (c_getbegy p) (c_getbegx p)

getFocus :: Monad m => MC m Window
getFocus = maybe stdWindow fst . L.uncons . _ce_zorder <$> getEnv

checkInput :: MonadIO m => CWindowPtr -> MC m (Maybe Input)
checkInput wptr = MC $ get >>= \cenv -> io $ do
    rc <- DCALL(c_wget_wch wptr (_ce_iptr cenv))
    code <- fi <$> peek (_ce_iptr cenv)
    PUTS("rc, code = " ++ show (rc, code))
    if rc == c_ERR then
        return Nothing
    else do
        liftM Just $ 
            if rc == 0 then
                parseChar code 
            else if code == c_KEY_MOUSE then
                parseMouse (_ce_mptr cenv)
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
    case _ce_heldev cenv of
      Just ev -> do putEnv cenv { _ce_heldev = Nothing }
                    return (Just ev)
      Nothing -> do
        wptr <- io $ win_wptr (maybe stdWindow fst $ L.uncons $ _ce_zorder cenv)
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
                putEnv cenv { _ce_heldev = in0 }
                readjustWin stdWindow
                return in1
            else if isNothing in1 then
                return in0
            else do
                putEnv cenv { _ce_heldev = in1 }
                return in0
            
waitInputUpTo :: MonadIO m => Int -> MC m (Maybe Input)
waitInputUpTo n = getInput (Just n)

waitInput :: MonadIO m => MC m Input
waitInput = maybe (fail "waitInput: Nothing") return =<< getInput Nothing

getByteString :: MonadIO m => MC m ByteString
getByteString = liftM BS.pack step 
    where
    step = do 
        ev <- waitInput
        case ev of 
            CharPress c | c == '\n' -> return []
                        | otherwise -> (c:) `liftM` step
            _ -> step

io_movewindow :: Window -> Int -> Int -> IO ()
io_movewindow w y x = case win_pointer w of
    Left wptr -> DRCCALL("mvwin", c_mvwin wptr y x)
    Right pptr -> DRCCALL("move_panel", c_move_panel pptr y x)

checkRC :: MonadIO m => String -> CInt -> m ()
checkRC name code
  | code == c_ERR = fail $ name ++ ": return code = ERR"
  | otherwise = return ()

unsafeInterleaveMC :: MC IO a -> MC IO a
unsafeInterleaveMC (MC (StateT op)) = MC $ StateT $ 
                                      \x -> unsafeInterleaveIO (op x)

io :: MonadIO m => IO a -> m a
io = liftIO

test1 :: MonadIO m => MC m Input
test1 = do
    win <- newWindow 10 30 2 4
    drawBorder win
    render
    waitInput
    win ~> do move 3 5
    render
    waitInput

testRegion1 :: MC IO Input
testRegion1 = do
    win <- newWindow 10 30 2 4
    drawBorder win
    sw <- newRegion win 8 28 1 1
    render
    moveCursor sw 1 1 
    drawByteString sw "asdasd"
    waitInput
    moveCursor sw 1 1 
    drawByteString sw "ewqewq"
    waitInput
