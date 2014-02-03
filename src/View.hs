module View where

import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Maybe ( fromJust )
import Control.Monad ( {-unless,void,when,-} replicateM )

type ProgCtrlStat = IORef Bool 

data RunMode = TitleMode | PlayMode
  deriving (Eq,Show)

data GLFWHandle = GLFWHandle
  { winHdl :: Maybe GLFW.Window
  , mouseStat :: MouseStatusHdl
  , keyStat :: KeyStatusHdl
  , exitFlg :: ProgCtrlStat
  , runMode :: IORef RunMode
  , oldTime :: IORef Double
  }





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
      { fKey   = sl!!0 
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
keyPress s _ GLFW.Key'Escape _ GLFW.KeyState'Pressed  _  = writeIORef (escKey s) 1
--keyPress s _ GLFW.Key'Escape _ GLFW.KeyState'Released _  = writeIORef (escKey s) 0 
keyPress s _ GLFW.Key'W      _ GLFW.KeyState'Pressed  _  = writeIORef (fKey s) 1  
keyPress s _ GLFW.Key'W      _ GLFW.KeyState'Released _  = writeIORef (fKey s) 0 
keyPress s _ GLFW.Key'S      _ GLFW.KeyState'Pressed  _  = writeIORef (bKey s) 1
keyPress s _ GLFW.Key'S      _ GLFW.KeyState'Released _  = writeIORef (bKey s) 0 
keyPress s _ GLFW.Key'A      _ GLFW.KeyState'Pressed  _  = writeIORef (lKey s) 1
keyPress s _ GLFW.Key'A      _ GLFW.KeyState'Released _  = writeIORef (lKey s) 0 
keyPress s _ GLFW.Key'D      _ GLFW.KeyState'Pressed  _  = writeIORef (rKey s) 1
keyPress s _ GLFW.Key'D      _ GLFW.KeyState'Released _  = writeIORef (rKey s) 0 
keyPress s _ GLFW.Key'E      _ GLFW.KeyState'Pressed  _  = writeIORef (tolKey s) 1
keyPress s _ GLFW.Key'E      _ GLFW.KeyState'Released _  = writeIORef (tolKey s) 0 
keyPress s _ GLFW.Key'Space  _ GLFW.KeyState'Pressed  _  = writeIORef (jmpKey s) 2
keyPress s _ GLFW.Key'Space  _ GLFW.KeyState'Released _  = writeIORef (jmpKey s) 0 
--keyPress s _ GLFW.Key'Tab    _ GLFW.KeyState'Pressed  _  = writeIORef (tabKey s) 1
--keyPress s _ GLFW.Key'Tab    _ GLFW.KeyState'Released _  = writeIORef (tabKey s) 1
keyPress s _ GLFW.Key'Up     _ GLFW.KeyState'Released _  = writeIORef (dbg1 s) 1 
keyPress s _ GLFW.Key'Down   _ GLFW.KeyState'Released _  = writeIORef (dbg2 s) 1 
keyPress _ _ _               _ _                      _  = return () 


-- ##################### Mouse ############################
data MouseStatusHdl = MouseStatusHdl
  { deltMove :: IORef (Int,Int)
  , btn1Clk  :: IORef Bool
  , btn2Clk  :: IORef Bool
  , btn3Clk  :: IORef Bool
  , scrlMove :: IORef (Double,Double)
  }

createMouseStatHdl :: IO MouseStatusHdl
createMouseStatHdl = do
  dltmv <- newIORef (0,0)
  btn1 <- newIORef False
  btn2 <- newIORef False
  btn3 <- newIORef False
  scr <- newIORef (0,0)
  return MouseStatusHdl {
    deltMove = dltmv
  , btn1Clk  = btn1
  , btn2Clk  = btn2
  , btn3Clk  = btn3
  , scrlMove = scr
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

setCursorMotion :: IORef RunMode -> MouseStatusHdl -> GLFW.CursorPosCallback
setCursorMotion mode opeMus w x y = do
  m' <- readIORef mode
  case m' of
    TitleMode -> return ()
    PlayMode -> do
      (wsx,wsy) <- GLFW.getWindowSize w
      let cx = fromIntegral wsx / 2.0
          cy = fromIntegral wsy / 2.0
      GLFW.setCursorPos w cx cy 
      modifyIORef (deltMove opeMus) (\ (xo,yo) -> (xo + floor (cx-x), yo + floor (cy-y)))

getCursorMotion :: GLFWHandle -> IO (Int, Int)
getCursorMotion glfwHdl = do
  (dx,dy) <- readIORef (deltMove opeMus)
  writeIORef (deltMove opeMus) (0,0)
  return (dx,dy)
  where
    opeMus = mouseStat glfwHdl

getButtonClick :: GLFWHandle -> IO (Double,Double,Bool,Bool,Bool)
getButtonClick glfwHdl = do
  (x,y) <- GLFW.getCursorPos $ fromJust win 
  btn1 <- readIORef (btn1Clk opeMusS)
  writeIORef (btn1Clk opeMusS) False
  btn2 <- readIORef (btn2Clk opeMusS)
  writeIORef (btn2Clk opeMusS) False
  btn3 <- readIORef (btn3Clk opeMusS)
  writeIORef (btn3Clk opeMusS) False
  return (x,y,btn1, btn2, btn3)
  where
    win = winHdl glfwHdl
    opeMusS = mouseStat glfwHdl 

-- | button press  
setButtonClick :: MouseStatusHdl -> GLFW.MouseButtonCallback
setButtonClick s _ GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = writeIORef (btn1Clk s) True 
setButtonClick s _ GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _ = writeIORef (btn2Clk s) True 
setButtonClick s _ GLFW.MouseButton'3 GLFW.MouseButtonState'Pressed _ = writeIORef (btn3Clk s) True 
setButtonClick _ _ _                  _                             _ = return () 


