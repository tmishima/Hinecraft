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
    --
  , getSystemKeyOpe
  , getMoveKeyOpe
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
  { winHdl :: Maybe GLFW.Window
  , mouseStat :: MouseStatusHdl
  , keyStat :: KeyStatusHdl
  , exitFlg :: ProgCtrlStat
  , uiMode :: IORef UIMode
  , oldTime :: IORef Double
  }

data UIMode = Mode2D | Mode3D
  deriving (Eq,Show)

type ProgCtrlStat = IORef Bool 

initGLFW :: (Int,Int) -> IO GLFWHandle
initGLFW (wWidth,wHight) = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor (3::Int)
  --GLFW.windowHint $ GLFW.WindowHint'RefreshRate (60::Int)
  win <- GLFW.createWindow wWidth wHight "Hinecraft" Nothing Nothing
  exitFlg' <- newIORef False
  uiMode' <- newIORef Mode2D

  keyStat' <- createKeyStatHdl
  mouseStat' <- createMouseStatHdl
  --
  oldTime' <- newIORef 0.0 
  --
  GLFW.makeContextCurrent win
  case win of
    Just win' -> do
      -- Callback
      GLFW.setWindowCloseCallback win' (Just $ finishGLFW exitFlg')
      GLFW.setKeyCallback win' (Just (keyPress keyStat'))
      GLFW.setCursorPosCallback win'
                           (Just (setCursorMotion uiMode' mouseStat'))
      GLFW.setMouseButtonCallback win' (Just (setButtonClick mouseStat'))
      GLFW.setScrollCallback win' (Just (setScrollMotion mouseStat'))
    Nothing -> return ()

  --GLFW.swapInterval 5
  return GLFWHandle 
    { winHdl = win
    , mouseStat = mouseStat'
    , keyStat = keyStat'
    , exitFlg = exitFlg'
    , uiMode = uiMode'
    , oldTime = oldTime'
    }

setUIMode :: GLFWHandle -> UIMode -> IO ()
setUIMode glfwHdl mode = do
  writeIORef ui' mode
  case winHdl glfwHdl of
    Just win -> case mode of
      Mode2D -> -- 2D
        GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
      Mode3D -> 
        GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
    Nothing -> return ()
  where
    ui' = uiMode glfwHdl

togleUIMode :: GLFWHandle -> IO ()
togleUIMode glfwHdl = 
  case winHdl glfwHdl of
    Just win -> do
      md <- readIORef ui'
      nmd <- case md of
        Mode3D -> 
          GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
            >> return Mode2D
        Mode2D -> 
          GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
            >> return Mode3D
      writeIORef ui' nmd
    Nothing -> return ()
  where
    ui' = uiMode glfwHdl


pollGLFW :: IO ()
pollGLFW = GLFW.pollEvents

swapBuff :: GLFWHandle -> IO ()
swapBuff glfwHdl = case winHdl glfwHdl of
  Just win -> GLFW.swapBuffers win
  Nothing -> return ()

getExitReqGLFW :: GLFWHandle -> IO Bool
getExitReqGLFW glfwHdl = readIORef $ exitFlg glfwHdl

getDeltTime :: GLFWHandle -> IO Double
getDeltTime glfwHdl = do 
  Just newTime' <- GLFW.getTime  
  oldTime' <- readIORef $ oldTime glfwHdl 
  writeIORef (oldTime glfwHdl) newTime'
  return $ newTime' - oldTime'

getWindowSize :: GLFWHandle -> IO (Int,Int)
getWindowSize glfwHdl = case win of
  Just w -> GLFW.getFramebufferSize w
  Nothing -> return (0,0)
  where
    win = winHdl glfwHdl

exitGLFW :: GLFWHandle -> IO ()
exitGLFW glfwHdl = case win of 
    Just win' -> do
      GLFW.destroyWindow win'
      GLFW.terminate
    Nothing -> return ()
  where
    win = winHdl glfwHdl

finishGLFW :: ProgCtrlStat -> GLFW.WindowCloseCallback
finishGLFW exitflg _ = do
  writeIORef exitflg True
  return ()

-- ##################### Keybord ###########################
data KeyStatusHdl = KeyStatusHdl
  { fKey   :: IORef Int
  , bKey   :: IORef Int 
  , rKey   :: IORef Int
  , lKey   :: IORef Int
  , jmpKey :: IORef Int
  , tolKey :: IORef Int
  , escKey :: IORef Int
  , tabKey :: IORef Int
  , dbg1   :: IORef Int
  , dbg2   :: IORef Int
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
      { fKey   = head sl 
      , bKey   = sl!!1
      , rKey   = sl!!2
      , lKey   = sl!!3
      , jmpKey = sl!!4
      , tolKey = sl!!5
      , escKey = sl!!6
      , tabKey = sl!!7
      , dbg1   = sl!!8
      , dbg2   = sl!!9
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
getCursorPosition glfwHdl = case winHdl glfwHdl of 
    Just w -> do
      (_,h) <- GLFW.getFramebufferSize w
      fmap (\ (x,y) -> (x,fromIntegral h - y))
         $ GLFW.getCursorPos w
    Nothing -> return (0,0)

setMouseBtnMode :: GLFWHandle -> BottonMode -> IO ()
setMouseBtnMode glfwHdl = writeIORef (btnMode opeMusS) 
  where
    opeMusS = mouseStat glfwHdl 

getButtonClick :: GLFWHandle -> IO (Double,Double,Bool,Bool,Bool)
getButtonClick glfwHdl = case winHdl glfwHdl of 
    Just w -> do
      (_,h) <- GLFW.getFramebufferSize w
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
    Nothing -> return (0.0,0.0,False, False, False)
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

