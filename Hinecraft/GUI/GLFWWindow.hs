{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.GUI.GLFWWindow 
  ( GLFWHandle
  , UIMode (..)
    --
  , initGLFW
  , pollGLFW
  , getDeltTime
  , setUIMode
  , togleUIMode
  , getWindowSize
  , swapBuff
  , getExitReqGLFW
  , exitGLFW
  , toggleFullScreenMode
    --
  , getSystemKeyOpe
  , getMoveKeyOpe
  , getScreenModeKeyOpe
    --
  , getScrollMotion
  , getCursorMotion
  , getCursorPosition
  , BottonMode (..)
  , setMouseBtnMode
  , getButtonClick
  )
  where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad ( replicateM, when )
import Data.IORef

data GLFWHandle = GLFWHandle
  { winHdl :: IORef (Maybe GLFW.Window,Bool)
  , winSize :: (Int,Int)
  , mouseStat :: MouseStatusHdl
  , keyStat :: KeyStatusHdl
  , exitFlg :: ProgCtrlStat
  , uiMode :: IORef UIMode
  , oldTime :: IORef Double
  }

data UIMode = Mode2D | Mode3D
  deriving (Eq,Show)

type ProgCtrlStat = IORef Bool 

initGLFW :: (Int,Int) -> Bool -> IO GLFWHandle
initGLFW winSize' fullScreenSW = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor (3::Int)
  --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor (3::Int)
  --GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Compat
  --GLFW.windowHint $ GLFW.WindowHint'RefreshRate (60::Int)
  --
  exitFlg' <- newIORef False
  uiMode' <- newIORef Mode2D

  keyStat' <- createKeyStatHdl
  mouseStat' <- createMouseStatHdl
  --
  oldTime' <- newIORef 0.0 
  --
  --GLFW.swapInterval 0
  win <- createDefWindow winSize' fullScreenSW
                         exitFlg' keyStat' uiMode' mouseStat' 
  return GLFWHandle 
    { winHdl = win
    , winSize = winSize'
    , mouseStat = mouseStat'
    , keyStat = keyStat'
    , exitFlg = exitFlg'
    , uiMode = uiMode'
    , oldTime = oldTime'
    }

setCallBacktoWin :: GLFW.Window
                 -> ProgCtrlStat
                 -> KeyStatusHdl -> IORef UIMode
                 -> MouseStatusHdl
                 -> IO ()
setCallBacktoWin win' exitFlg' keyStat' uiMode' mouseStat' = do
  GLFW.setWindowCloseCallback win' (Just $ finishGLFW exitFlg')
  GLFW.setKeyCallback win' (Just (keyPress keyStat'))
  GLFW.setCursorPosCallback win'
                       (Just (setCursorMotion uiMode' mouseStat'))
  GLFW.setMouseButtonCallback win' (Just (setButtonClick mouseStat'))
  GLFW.setScrollCallback win' (Just (setScrollMotion mouseStat'))

createDefWindow :: (Int, Int) -> Bool -> ProgCtrlStat
                -> KeyStatusHdl -> IORef UIMode
                -> MouseStatusHdl
                -> IO (IORef (Maybe GLFW.Window, Bool))
createDefWindow (wWidth,wHight) isFullScr exitFlg' keyStat'
                uiMode' mouseStat' = do
  win <- if isFullScr
    then do
      moni <- GLFW.getPrimaryMonitor 
      GLFW.createWindow wWidth wHight hinecraftTitle moni Nothing
    else 
      GLFW.createWindow wWidth wHight hinecraftTitle Nothing Nothing
  GLFW.makeContextCurrent win
  case win of
    Just win' -> -- Callback
      setCallBacktoWin win' exitFlg' keyStat' uiMode' mouseStat'
    Nothing -> return ()
  newIORef (win, isFullScr)

hinecraftTitle :: String
hinecraftTitle = "Hinecraft"

toggleFullScreenMode :: GLFWHandle -> IO ()
toggleFullScreenMode glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of
    (Just win,scmd) -> do
      (wWidth,wHight) <- getWindowSize glfwHdl
      GLFW.windowShouldClose win
      GLFW.destroyWindow win
      newwin' <- if scmd 
        then
          GLFW.createWindow wWidth wHight hinecraftTitle Nothing Nothing
        else do
          moni <- GLFW.getPrimaryMonitor 
          GLFW.createWindow wWidth wHight hinecraftTitle moni Nothing

      GLFW.makeContextCurrent newwin'

      case newwin' of
        Just win' -> -- Callback
          setCallBacktoWin win' exitFlg' keyStat' uiMode' mouseStat'
        Nothing -> return ()
      writeIORef (winHdl glfwHdl) (newwin',not scmd)
      return ()
    (Nothing,_) -> return ()
  where
    mouseStat' = mouseStat glfwHdl
    keyStat' = keyStat glfwHdl
    exitFlg' = exitFlg glfwHdl
    uiMode' = uiMode glfwHdl

setUIMode :: GLFWHandle -> UIMode -> IO ()
setUIMode glfwHdl mode = do
  writeIORef ui' mode
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of
    (Just win,_) -> case mode of
      Mode2D -> -- 2D
        GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
      Mode3D -> 
        GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
    (Nothing,_) -> return ()
  where
    !ui' = uiMode glfwHdl

togleUIMode :: GLFWHandle -> IO ()
togleUIMode glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of
    (Just win,_) -> do
      md <- readIORef ui'
      nmd <- case md of
        Mode3D -> 
          GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
            >> return Mode2D
        Mode2D -> 
          GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
            >> return Mode3D
      writeIORef ui' nmd
    (Nothing,_) -> return ()
  where
    ui' = uiMode glfwHdl

pollGLFW :: IO ()
pollGLFW = GLFW.pollEvents

swapBuff :: GLFWHandle -> IO ()
swapBuff glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of
    (Just win,_) -> GLFW.swapBuffers win
    (Nothing,_) -> return ()

getExitReqGLFW :: GLFWHandle -> IO Bool
getExitReqGLFW glfwHdl = readIORef $ exitFlg glfwHdl

getDeltTime :: GLFWHandle -> IO Double
getDeltTime glfwHdl = do 
  Just newTime' <- GLFW.getTime  
  oldTime' <- readIORef $ oldTime glfwHdl 
  writeIORef (oldTime glfwHdl) newTime'
  return $! newTime' - oldTime'

getWindowSize :: GLFWHandle -> IO (Int,Int)
getWindowSize glfwHdl = 
  return $ winSize glfwHdl
{-  winStat <- readIORef $ winHdl glfwHdl
  case winStat of
    (Just w,_) -> GLFW.getFramebufferSize w
    (Nothing,_) -> return (0,0)
-}

exitGLFW :: GLFWHandle -> IO ()
exitGLFW glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of 
    (Just win',_) -> do
      GLFW.destroyWindow win'
      GLFW.terminate
    (Nothing,_) -> return ()

finishGLFW :: ProgCtrlStat -> GLFW.WindowCloseCallback
finishGLFW exitflg _ = do
  writeIORef exitflg True
  return ()

-- ##################### Keybord ###########################
data KeyStatusHdl = KeyStatusHdl
  { fKey     :: IORef Int
  , bKey     :: IORef Int 
  , rKey     :: IORef Int
  , lKey     :: IORef Int
  , jmpKey   :: IORef Int
  , tolKey   :: IORef Int
  , escKey   :: IORef Int
  , tabKey   :: IORef Int
  , scrMdKey :: IORef Int
  , dbg1     :: IORef Int
  , dbg2     :: IORef Int
  }

getSystemKeyOpe :: GLFWHandle -> IO (Bool, Bool)
getSystemKeyOpe glfwHdl = do
  esc <- readIORef (escKey opeKeyS)
  writeIORef (escKey opeKeyS) 0
  tol <- readIORef (tolKey opeKeyS)
  writeIORef (tolKey opeKeyS) 0
  return (esc > 0, tol > 0)
  where
    opeKeyS = keyStat glfwHdl   

getScreenModeKeyOpe :: GLFWHandle -> IO Bool
getScreenModeKeyOpe glfwHdl = do
  ope <- readIORef (scrMdKey opeKeyS)
  writeIORef (scrMdKey opeKeyS) 0
  return (ope > 0)
  where
    opeKeyS = keyStat glfwHdl   

getMoveKeyOpe :: GLFWHandle -> IO (Int, Int, Int, Int, Int)
getMoveKeyOpe glfwHdl = do 
  f <- readIORef (fKey opeKeyS)
  b <- readIORef (bKey opeKeyS)
  l <- readIORef (lKey opeKeyS)
  r <- readIORef (rKey opeKeyS) 
  jmp <- readIORef (jmpKey opeKeyS)  
  writeIORef (jmpKey opeKeyS) 1
  return (f,b,l,r,jmp)
  where
    opeKeyS = keyStat glfwHdl   

{-
getDebugKey :: ViewCtrl -> IO (Bool, Bool)
getDebugKey viewCtrl = do
  d1 <- readIORef (dbg1 opeKeyS)
  writeIORef (dbg1 opeKeyS) 0
  d2 <- readIORef (dbg2 opeKeyS)
  writeIORef (dbg2 opeKeyS) 0
  return ( d1 > 0, d2 > 0)
  where
    opeKeyS = keyStat viewCtrl  
-}

createKeyStatHdl :: IO KeyStatusHdl
createKeyStatHdl = do
  sl <- replicateM 10 (newIORef (0 :: Int))
  return KeyStatusHdl
      { fKey      = head sl 
      , bKey      = sl!!1
      , rKey      = sl!!2
      , lKey      = sl!!3
      , jmpKey    = sl!!4
      , tolKey    = sl!!5
      , escKey    = sl!!6
      , tabKey    = sl!!7
      , scrMdKey  = sl!!8
      , dbg1      = sl!!9
      , dbg2      = sl!!10
      } 

keyPress :: KeyStatusHdl -> GLFW.KeyCallback
keyPress s _ GLFW.Key'Escape _ GLFW.KeyState'Pressed  _
  = writeIORef (escKey s) 1
--keyPress s _ GLFW.Key'Escape _ GLFW.KeyState'Released _
--  = writeIORef (escKey s) 0 
keyPress s _ GLFW.Key'W      _ GLFW.KeyState'Pressed  _
  = writeIORef (fKey s) 1  
keyPress s _ GLFW.Key'W      _ GLFW.KeyState'Released _
  = writeIORef (fKey s) 0 
keyPress s _ GLFW.Key'S      _ GLFW.KeyState'Pressed  _
  = writeIORef (bKey s) 1
keyPress s _ GLFW.Key'S      _ GLFW.KeyState'Released _
  = writeIORef (bKey s) 0 
keyPress s _ GLFW.Key'A      _ GLFW.KeyState'Pressed  _
  = writeIORef (lKey s) 1
keyPress s _ GLFW.Key'A      _ GLFW.KeyState'Released _
  = writeIORef (lKey s) 0 
keyPress s _ GLFW.Key'D      _ GLFW.KeyState'Pressed  _
  = writeIORef (rKey s) 1
keyPress s _ GLFW.Key'D      _ GLFW.KeyState'Released _
  = writeIORef (rKey s) 0 
--keyPress s _ GLFW.Key'E      _ GLFW.KeyState'Pressed  _
--  = writeIORef (tolKey s) 1
keyPress s _ GLFW.Key'E      _ GLFW.KeyState'Released _
  = writeIORef (tolKey s) 1 
keyPress s _ GLFW.Key'Space  _ GLFW.KeyState'Pressed  _
  = writeIORef (jmpKey s) 2
keyPress s _ GLFW.Key'Space  _ GLFW.KeyState'Released _
  = writeIORef (jmpKey s) 0 
--keyPress s _ GLFW.Key'Tab    _ GLFW.KeyState'Pressed  _
--  = writeIORef (tabKey s) 1
--keyPress s _ GLFW.Key'Tab    _ GLFW.KeyState'Released _
--  = writeIORef (tabKey s) 1
keyPress s _ GLFW.Key'Up     _ GLFW.KeyState'Released _
  = writeIORef (dbg1 s) 1 
keyPress s _ GLFW.Key'Down   _ GLFW.KeyState'Released _
  = writeIORef (dbg2 s) 1 
keyPress s _ GLFW.Key'F11    _ GLFW.KeyState'Released _
  = writeIORef (scrMdKey s) 1
keyPress _ _ _               _ _                      _
  = return () 

-- ##################### Mouse ############################
data MouseStatusHdl = MouseStatusHdl
  { deltMove :: IORef (Double,Double)
  , btn1Clk  :: IORef Bool
  , btn2Clk  :: IORef Bool
  , btn3Clk  :: IORef Bool
  , scrlMove :: IORef (Double,Double)
  , btnMode  :: IORef BottonMode
  }

data BottonMode = REdgeMode | StateMode
  deriving (Eq,Show)

createMouseStatHdl :: IO MouseStatusHdl
createMouseStatHdl = do
  dltmv <- newIORef (0,0)
  btn1 <- newIORef False
  btn2 <- newIORef False
  btn3 <- newIORef False
  scr <- newIORef (0,0)
  btnMd <- newIORef REdgeMode
  return MouseStatusHdl {
    deltMove = dltmv
  , btn1Clk  = btn1
  , btn2Clk  = btn2
  , btn3Clk  = btn3
  , scrlMove = scr
  , btnMode  = btnMd
  }

setScrollMotion :: MouseStatusHdl -> GLFW.ScrollCallback
setScrollMotion opeMus _ x y = 
  modifyIORef (scrlMove opeMus) (\ (xo,yo) -> (xo + x, yo + y))

getScrollMotion :: GLFWHandle -> IO (Int, Int) 
getScrollMotion glfwHdl = do
  (x,y) <- readIORef $ scrlMove opeMus
  writeIORef (scrlMove opeMus) (0,0)
  return (floor x, floor y)
  where
    opeMus = mouseStat glfwHdl

setCursorMotion :: IORef UIMode -> MouseStatusHdl
                -> GLFW.CursorPosCallback
setCursorMotion mode opeMus w x y = do
  m' <- readIORef mode
  case m' of
    Mode2D -> return () 
    Mode3D -> do
      (wsx,wsy) <- GLFW.getWindowSize w
      let cx = fromIntegral wsx / 2.0
          cy = fromIntegral wsy / 2.0
      GLFW.setCursorPos w cx cy 
      modifyIORef (deltMove opeMus) (\ (xo,yo) -> ( xo + cx -x
                                                  , yo + cy -y))

getCursorMotion :: GLFWHandle -> IO (Double, Double)
getCursorMotion glfwHdl = do
  (dx,dy) <- readIORef (deltMove opeMus)
  writeIORef (deltMove opeMus) (0,0)
  return (dx,dy)
  where
    opeMus = mouseStat glfwHdl

getCursorPosition :: GLFWHandle -> IO (Double, Double)
getCursorPosition glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of 
    (Just w,_) -> do
      (_,h) <- getWindowSize glfwHdl
      fmap (\ (x,y) -> (x,fromIntegral h - y))
         $ GLFW.getCursorPos w
    (Nothing,_) -> return (0,0)

setMouseBtnMode :: GLFWHandle -> BottonMode -> IO ()
setMouseBtnMode glfwHdl = writeIORef (btnMode opeMusS) 
  where
    opeMusS = mouseStat glfwHdl 

getButtonClick :: GLFWHandle -> IO (Double,Double,Bool,Bool,Bool)
getButtonClick glfwHdl = do
  winStat <- readIORef $ winHdl glfwHdl
  case winStat of 
    (Just w,_) -> do
      (_,h) <- getWindowSize glfwHdl
      (x,y) <- fmap (\ (x,y) -> (x,fromIntegral h - y))
         $ GLFW.getCursorPos w
      btn1 <- readIORef (btn1Clk opeMusS)
      btn2 <- readIORef (btn2Clk opeMusS)
      btn3 <- readIORef (btn3Clk opeMusS)
      btnMd <- readIORef (btnMode opeMusS) 
      when (btnMd == REdgeMode) $ do
          writeIORef (btn1Clk opeMusS) False
          writeIORef (btn2Clk opeMusS) False
          writeIORef (btn3Clk opeMusS) False
      return (x,y,btn1, btn2, btn3)
    (Nothing,_) -> return (0.0,0.0,False, False, False)
  where
    opeMusS = mouseStat glfwHdl 

-- | button press  
setButtonClick :: MouseStatusHdl -> GLFW.MouseButtonCallback
setButtonClick s _ GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _
  = writeIORef (btn1Clk s) True 
setButtonClick s _ GLFW.MouseButton'1 GLFW.MouseButtonState'Released _
  = writeIORef (btn1Clk s) False
setButtonClick s _ GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _
  = writeIORef (btn2Clk s) True 
setButtonClick s _ GLFW.MouseButton'2 GLFW.MouseButtonState'Released _
  = writeIORef (btn2Clk s) False
setButtonClick s _ GLFW.MouseButton'3 GLFW.MouseButtonState'Pressed _
  = writeIORef (btn3Clk s) True 
setButtonClick s _ GLFW.MouseButton'3 GLFW.MouseButtonState'Released _
  = writeIORef (btn3Clk s) False
setButtonClick _ _ _                  _                             _
  = return () 

