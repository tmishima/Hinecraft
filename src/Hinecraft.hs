import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Data.IORef
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( unless,void,when,replicateM )
import Data.Maybe ( fromJust )
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Codec.Picture as CdP
import qualified Data.Vector.Storable as Vct
import System.Directory ( getHomeDirectory )
import Graphics.Rendering.FTGL as Ft
import Data.Array.IO

initGL :: IO ()
initGL = do
  tu <- get maxTextureUnit
  tm <- get maxTextureSize
  lt <- get maxLights
  Dbg.traceIO $ unwords [ "\n max texutere unit =", show tu
                        , "\n max texture size =", show tm
                        , "\n max lights =" , show lt
                        ]

  texture Texture2D $= Enabled
  shadeModel        $= Smooth
  clearColor        $= Color4 0 0 0 0.0
  clearDepth        $= 1.0
  depthFunc         $= Just Less

  --
  lineSmooth        $= Enabled
  lineWidth         $= 10.0
  blend             $= Enabled
  blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
  --blendFunc         $= (SrcAlpha, OneMinusConstantAlpha)
  alphaFunc         $= Just (Greater, 0.2)
  --lightModelAmbient $= Color4 0.1 0.1 0.1 0.2
  --lighting        $= Enabled
  colorMaterial     $= Just (FrontAndBack, AmbientAndDiffuse)
  --colorMaterial     $= Just (GL.Front, AmbientAndDiffuse)

  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

data RunMode = TitleMode | PlayMode
  deriving (Eq,Show)
data ViewMode = V2DMode | V3DTitleMode | V3DMode
  deriving (Eq,Show)

type ProgCtrlStat = IORef Bool 
data GLFWHandle = GLFWHandle
  { winHdl :: Maybe GLFW.Window
  , mouseStat :: MouseStatusHdl
  , keyStat :: KeyStatusHdl
  , exitFlg :: ProgCtrlStat
  , runMode :: IORef RunMode
  }

initGLFW :: (Int,Int) -> IO GLFWHandle
initGLFW (wWidth,wHight) = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  --
  win <- GLFW.createWindow wWidth wHight "Hinecraft" Nothing Nothing
  exitFlg' <- newIORef False
  runMode' <- newIORef TitleMode

  keyStat' <- createKeyStatHdl
  mouseStat' <- createMouseStatHdl
  --
  GLFW.makeContextCurrent win
  case win of
    Just win' -> do
      -- Callback
      GLFW.setWindowCloseCallback win' (Just $ finishGLFW exitFlg')
      GLFW.setKeyCallback win' (Just (keyPress keyStat'))
      GLFW.setCursorPosCallback win' (Just (setCursorMotion runMode' mouseStat'))
      GLFW.setMouseButtonCallback win' (Just (setButtonClick mouseStat'))
      GLFW.setScrollCallback win' (Just (setScrollMotion mouseStat'))
    Nothing -> return ()

  return GLFWHandle 
    { winHdl = win
    , mouseStat = mouseStat'
    , keyStat = keyStat'
    , exitFlg = exitFlg'
    , runMode = runMode'
    }

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
setScrollMotion opeMus _ x y = do
  modifyIORef (scrlMove opeMus) (\ (xo,yo) -> (xo + x, yo + y))

{-
getScrollMotion :: ViewCtrl -> IO (Int, Int) 
getScrollMotion viewCtrl = do
  (x,y) <- readIORef $ scrlMove opeMusS
  writeIORef (scrlMove opeMusS) (0,0)
  return (floor x, floor y)
  where
    opeMusS = musStat viewCtrl
-}
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
{-
getCursorMotion :: ViewCtrl -> IO (Int, Int)
getCursorMotion viewCtrl = do
  (dx,dy) <- readIORef (deltMove opeMusS)
  writeIORef (deltMove opeMusS) (0,0)
  return (dx,dy)
  where
    opeMusS = musStat viewCtrl
-}

-- | button press  
setButtonClick :: MouseStatusHdl -> GLFW.MouseButtonCallback
setButtonClick s _ GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = writeIORef (btn1Clk s) True 
setButtonClick s _ GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _ = writeIORef (btn2Clk s) True 
setButtonClick s _ GLFW.MouseButton'3 GLFW.MouseButtonState'Pressed _ = writeIORef (btn3Clk s) True 
setButtonClick _ _ _                  _                             _ = return () 

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

createKeyStatHdl :: IO KeyStatusHdl
createKeyStatHdl = do
  sl <- mapM newIORef $ replicate 10 (0 :: Int)
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

getSystemKeyOpe :: GLFWHandle -> IO (Bool, Bool)
getSystemKeyOpe glfwHdl = do
  esc <- readIORef (escKey opeKeyS)
  tol <- readIORef (tolKey opeKeyS)
  return (esc > 0, tol > 0)
  where
    opeKeyS = keyStat glfwHdl   

{-
getMoveKeyOpe :: ViewCtrl -> IO (Int, Int, Int, Int, Int)
getMoveKeyOpe viewCtrl = do 
  f <- readIORef (fKey opeKeyS)
  b <- readIORef (bKey opeKeyS)
  l <- readIORef (lKey opeKeyS)
  r <- readIORef (rKey opeKeyS) 
  jmp <- readIORef (jmpKey opeKeyS)  
  writeIORef (jmpKey opeKeyS) 1
  return (f,b,l,r,jmp)
  where
    opeKeyS = keyStat viewCtrl  

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

finishGLFW :: ProgCtrlStat -> GLFW.WindowCloseCallback
finishGLFW exitflg _ = do
  writeIORef exitflg True
  return ()

exitGLFW :: GLFWHandle -> IO ()
exitGLFW glfwHdl = case win of 
    Just win' -> do
      GLFW.destroyWindow win'
      GLFW.terminate
    Nothing -> return ()
  where
    win = winHdl glfwHdl

setPerspective :: ViewMode -> Int -> Int -> IO ()
setPerspective viewMode' w h = do
  matrixMode $= Projection
  viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  loadIdentity
  
  case viewMode' of
    V2DMode -> ortho2D 0 (realToFrac w) 0 (realToFrac h)
    V3DMode -> perspective 60 (realToFrac w/realToFrac h) 0.05 300
    V3DTitleMode -> perspective 100 (realToFrac w/realToFrac h) 0.05 300
      -- perspective 45 ((realToFrac w)/(realToFrac h)) 0.05 300
      
  matrixMode $= Modelview 0
  loadIdentity

data GuiResource = GuiResource
  { backgroundBoxTexture :: [GLuint]
  , backgroundTitleTexture :: GLuint
  , widgetsTexture :: GLuint
  , widgetPlayBtnPos :: (GLfloat,GLfloat)
  , widgetPlayBtnSiz :: (GLfloat,GLfloat)
  , widgetExitBtnPos :: (GLfloat,GLfloat)
  , widgetExitBtnSiz :: (GLfloat,GLfloat)
  , font :: Ft.Font
  }

putTextLine :: Ft.Font -> Maybe (GLfloat,GLfloat,GLfloat)
            -> Maybe Int -> (GLfloat,GLfloat) -> String -> IO ()
putTextLine ft cl sz (x,y) str = preservingMatrix $ do
  texture Texture2D $= Disabled
  case cl of
    Just (r,g,b) -> color $ Color4 r g b 1.0
    _ -> return ()
  case sz of
    Just s -> void $ Ft.setFontFaceSize ft s 72 
    _ -> return ()
  rasterPos $ Vertex2 x y
  Ft.renderFont ft str Ft.Front

loadGuiResource :: FilePath -> IO GuiResource
loadGuiResource home = do
  tex' <- mapM (\ fn -> 
    Dbg.trace ("loadBackgroundPic : " ++ fn)
     $ loadTextures fn )
    [ bkgndPng0 , bkgndPng1 , bkgndPng2
    , bkgndPng3 , bkgndPng4 , bkgndPng5
    ]
  ttex' <- loadTextures bkgTtlPng
  wtex' <- loadTextures widPng
  font' <- Ft.createBitmapFont fontPath
  return GuiResource
    { backgroundBoxTexture = tex'
    , backgroundTitleTexture = ttex'
    , widgetsTexture = wtex'
    , font = font'
    , widgetPlayBtnPos = ( 365,768 - 410)
    , widgetPlayBtnSiz = (640, 62.5)
    , widgetExitBtnPos = (690, 768 - 614)
    , widgetExitBtnSiz = (155 * 2, 62.5)
    }
  where
    bkgndPng0 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_0.png"
    bkgndPng1 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_1.png"
    bkgndPng2 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_2.png"
    bkgndPng3 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_3.png"
    bkgndPng4 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_4.png"
    bkgndPng5 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_5.png"
    bkgTtlPng = home ++ "/.Hinecraft/textures/gui/title/hinecraft.png"
    widPng    = home ++ "/.Hinecraft/textures/gui/widgets.png"
    fontPath = "/usr/share/fonts/truetype/takao-mincho/TakaoPMincho.ttf" -- linux

loadTextures :: FilePath -> IO GLuint
loadTextures path = do
  Dbg.traceIO path
  Just (w,h,(ptr,off,_),t) <- rdImg
  Dbg.traceIO $ unwords ["Image w = ", show w, "Image h = ", show h]
  tex <- alloca $ \p -> do
            glGenTextures 1 p
            peek p
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest  = fromIntegral gl_NEAREST
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 4
      (fromIntegral w) (fromIntegral h)
      0 t gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glNearest
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glNearest

  return tex
  where
    showInfo n i = putStrLn $ unwords [n, show $ CdP.imageWidth i, show $ CdP.imageHeight i]
    getPtr i t = Just (CdP.imageWidth i
                     , CdP.imageHeight i
                     , Vct.unsafeToForeignPtr $ CdP.imageData i
                     , t )
    rdImg = do
      Right img <- CdP.readImage path
      case img of
        CdP.ImageY8     i -> showInfo "Y8"     i >> return Nothing
        CdP.ImageY16    i -> showInfo "Y16"    i >> return Nothing
        CdP.ImageYF     i -> showInfo "YF"     i >> return Nothing
        CdP.ImageYA8    i -> showInfo "YA8"    i >> return Nothing
        CdP.ImageYA16   i -> showInfo "YA16"   i >> return Nothing
        CdP.ImageRGB8   i -> showInfo "RGB8"   i
          >> return (getPtr i gl_RGB)
        CdP.ImageRGB16  i -> showInfo "RGB16"  i >> return Nothing
        CdP.ImageRGBF   i -> showInfo "RGBF"   i >> return Nothing
        CdP.ImageRGBA8  i -> showInfo "RGBA8"  i
          >> return (getPtr i gl_RGBA)
        CdP.ImageRGBA16 i -> showInfo "RGBA16" i >> return Nothing
        CdP.ImageYCbCr8 i -> showInfo "YCbCr8" i >> return Nothing
        CdP.ImageCMYK8  i -> showInfo "CMYK8"  i >> return Nothing
        CdP.ImageCMYK16 i -> showInfo "CMYK16" i >> return Nothing

main :: IO ()
main = bracket initHinecraft exitHinecraft runHinecraft

initHinecraft :: IO (GLFWHandle, GuiResource,WorldResource)
initHinecraft = do
  home <- getHomeDirectory
  Dbg.traceIO "Hinecraft Start"
  glfwHdl <- initGLFW (1366,768)

  guiRes <- loadGuiResource home
  wldRes <- loadWorldResouce home
  
  initGL
  return (glfwHdl,guiRes,wldRes)

exitHinecraft :: (GLFWHandle, GuiResource, WorldResource) -> IO ()
exitHinecraft (glfwHdl,_,_) = do
  exitGLFW glfwHdl
  Dbg.traceIO "Hinecraft End"

runHinecraft :: (GLFWHandle, GuiResource, WorldResource) -> IO ()
runHinecraft (glfwHdl,guiRes,wldRes) = 
  mainLoop useStat
  where
    useStat = UserStatus
      { userPos = (0.0,0.0,0.0)
      , userRot = (0.0,0.0,0.0) 
      , palletIndex = 0
      }
    mainLoop u' = do
      GLFW.pollEvents
      exitflg' <- readIORef $ exitFlg glfwHdl
      exitkey' <- mainProcess (glfwHdl,guiRes)
      drawView (glfwHdl,guiRes,wldRes,u')
      unless (exitflg' || exitkey') $ mainLoop u'

mainProcess :: (GLFWHandle, GuiResource) -> IO Bool
mainProcess (glfwHdl, guiRes) = do
  mous <- getButtonClick glfwHdl
  syskey <- getSystemKeyOpe glfwHdl
  mode' <- readIORef $ runMode glfwHdl
  (newMode, extflg) <- case mode' of
    TitleMode -> guiProcess guiRes mous -- ### 2D ###
    PlayMode -> playProcess syskey
  when (mode' /= newMode) $ writeIORef (runMode glfwHdl) newMode
  return extflg

playProcess :: (Bool,Bool) -> IO (RunMode,Bool)
playProcess (esc,_ {-tool-}) = if esc
                           then return (TitleMode, False)
                           else return (PlayMode, False)

guiProcess :: GuiResource -> (Double,Double,Bool,Bool,Bool)
           -> IO (RunMode,Bool)
guiProcess _   (_,_,False,_,_) = return (TitleMode, False)
guiProcess res (x,y,True,_,_) = return (chkPlaybtn,chkExitbtn)
  where
    plybtnPosOrgn = widgetPlayBtnPos res
    plybtnSize = widgetPlayBtnSiz res
    extbtnPosOrgn = widgetExitBtnPos res
    extbtnSize = widgetExitBtnSiz res
    exchg (a,b) = (realToFrac a, realToFrac b)
    chkRectIn (xo,yo) (w,h) =
       {- Dbg.trace ("2D clk = " ++ (show (x,y,xo,yo,w,h))) $ -}
       xo < x && x < (xo + w) && yo < (768 - y) && (768 - y) < (yo + h) 
    chkPlaybtn = if chkRectIn (exchg plybtnPosOrgn) (exchg plybtnSize)
      then PlayMode else TitleMode 
    chkExitbtn = chkRectIn (exchg extbtnPosOrgn) (exchg extbtnSize)

drawView :: (GLFWHandle, GuiResource, WorldResource, UserStatus) -> IO ()
drawView (glfwHdl, guiRes, wldRes, usrStat) = do
  clear [ColorBuffer,DepthBuffer]
  mode' <- readIORef $ runMode glfwHdl
  winSize <- GLFW.getFramebufferSize win

  case mode' of
    TitleMode -> do -- ### 2D ###
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
      drawTitle winSize guiRes
    PlayMode -> do
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
      drawPlay winSize guiRes wldRes usrStat
  flush
  GLFW.swapBuffers win
  where
    win = fromJust $ winHdl glfwHdl

data UserStatus = UserStatus
  { userPos :: (GLfloat,GLfloat,GLfloat)
  , userRot :: (GLfloat,GLfloat,GLfloat) 
  , palletIndex :: Int
  }
  deriving (Eq,Show)

data WorldResource = WorldResource
  { blockTexture :: GLuint
  }
  deriving (Eq,Show)

loadWorldResouce :: FilePath -> IO WorldResource
loadWorldResouce home = do
  btex' <- loadTextures blkPng 
  return $ WorldResource
    { blockTexture = btex'
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"


test = do
  arr <- newArray (1,10) 37 :: IO (IOUArray Int Int)
  a <- readArray arr 2
  writeArray arr 2 64
  b <- readArray arr 2 
  print (a,b)
  c <- genChunk (0,0)
  return ()

genWorldData = do
  cl <- newIORef =<< mapM genChunk [(0,0),(16,0),(0,16),(16,16)]
  return $ WorldData 
    { chunkList = cl
    }

data WorldData = WorldData
  { chunkList :: IORef [Chunk]
  }

data Chunk = Chunk
  { origin :: (Int,Int)
  , local :: [(Int,(IOUArray Int Int))]
  }

type BlockID = Int
voidBlockID = 0 :: BlockID
stoneBlockID = 1 :: BlockID
dirtBlockID = 2 :: BlockID

genChunk :: (Int,Int) -> IO Chunk
genChunk org = do
  arrt <- replicateM 4
    (newArray (0,16 * 16 * 16 - 1) voidBlockID) :: IO [(IOUArray Int Int)]
  arrs <- newArray (0,16 * 16 * 16 - 1) voidBlockID :: IO (IOUArray Int Int)
  mapM_ (\ i -> writeArray arrs i dirtBlockID) [0 .. 16 * 16 * 2 - 1]
  arrb <- replicateM 3
    (newArray (0,16 * 16 * 16 - 1) stoneBlockID) :: IO [(IOUArray Int Int)]
  return Chunk
    { origin = org
    , local = zip [0 .. ] $ concat [ arrb, arrs : arrt]
    } 






drawPlay :: (Int,Int) -> GuiResource -> WorldResource
         -> UserStatus -> IO ()
drawPlay (w,h) guiRes wldRes usrStat = do
  preservingMatrix $ do
    setPerspective V3DTitleMode w h
    scale 100.0 100.0 (100.0::GLfloat)
    drawBackGroundBox'

  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
    depthMask $= Disabled

    -- Pallet
    drawBackPlane (394,2) (578, 69) (Just widTex')
                  (0,0) (183/256,22/256) (1.0,1.0,1.0,1.0)

    drawBackPlane (394,2) (64, 64) (Just widTex')
                  (0,24/256) (22/256,22/256) (1.0,1.0,1.0,1.0)

    glPopAttrib 
    depthMask $= Enabled
    glEnable gl_DEPTH_TEST

  where
    widTex' = widgetsTexture guiRes
    (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex
    drawBackGroundBox' = preservingMatrix $ do
      color $ Color4 (180/255) (226/255) (255/255) (1.0::GLfloat)
      texture Texture2D $= Disabled
      renderPrimitive Quads $ do
        genSuf (0,0,-1) [p6,p7,p1,p0] -- Front
        genSuf (-1,0,0) [p4,p7,p1,p2] -- Right
        genSuf (1,0,0)  [p6,p5,p3,p0] -- Left
        genSuf (0,-1,0) [p5,p4,p7,p6] -- Top
        genSuf (0,1,0)  [p1,p0,p3,p2] -- Bottom
        genSuf (0,0,1)  [p4,p5,p3,p2] -- Back 
    genSuf :: (GLfloat,GLfloat,GLfloat)
           -> [(GLfloat,GLfloat,GLfloat)]
           -> IO ()
    genSuf (nx,ny,nz) ndlst = do
      normal $ Normal3 nx ny nz
      mapM_ (\ (x', y', z') -> do
               vertex (Vertex3 x' y' z')) ndlst


drawTitle :: (Int,Int) -> GuiResource -> IO ()
drawTitle (w,h) res = do
  preservingMatrix $ do
    setPerspective V3DTitleMode w h
    scale 10.0 10.0 (10.0::GLfloat)
    rotate 10 $ Vector3 1.0 0 (0::GLfloat)
    drawBackGroundBox bkgTex

  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
    depthMask $= Disabled

    putTextLine font' (Just (1,1,1)) (Just 20) (10,10) "Hinecraft 0.0.1" 

    -- White
    drawBackPlane (0,0) (fromIntegral w, fromIntegral h) Nothing
                  (0,0) (1,1) (1.0,1.0,1.0,0.30)
    -- Title 
    drawBackPlane (232,768 - 233) (508, 145) (Just titleTex)
                  (0,0) (0.61,0.172) (1.0,1.0,1.0,1.0)
    drawBackPlane (232 + 508 - 5, 768 - 233) (382, 145) (Just titleTex)
                  (0,0.176) (0.458,0.172) (1.0,1.0,1.0,1.0)


    -- Botton 

    drawBackPlane (xPlybtnPos, yPlybtnPos) (wPlybntSiz,hPlybtnSiz) (Just widTex)
                  (0,0.258) (0.78,0.078) (1.0,1.0,1.0,1.0)
    putTextLine font' (Just (1,1,1)) (Just 30) (600,768 - 410 + 20) "Single Play" 
    -- 
    drawBackPlane (xExtbtnPos, yExtbtnPos) (wExtbtnSiz / 2, hExtbtnSiz) (Just widTex)
                  (0,0.258) (0.20,0.078) (1.0,1.0,1.0,1.0)
    drawBackPlane (xExtbtnPos + wExtbtnSiz / 2, yExtbtnPos) (wExtbtnSiz / 2, hExtbtnSiz)
                  (Just widTex)
                  (0.58,0.258) (0.20,0.078) (1.0,1.0,1.0,1.0)
    putTextLine font' (Just (1,1,1)) (Just 30) (820,768 - 614 + 20) "Exit" 

    glPopAttrib 
    depthMask $= Enabled
    glEnable gl_DEPTH_TEST

  where
    bkgTex = backgroundBoxTexture res
    titleTex = backgroundTitleTexture res
    widTex = widgetsTexture res
    (xPlybtnPos,yPlybtnPos) = widgetPlayBtnPos res
    (wPlybntSiz,hPlybtnSiz) = widgetPlayBtnSiz res
    (xExtbtnPos,yExtbtnPos) = widgetExitBtnPos res
    (wExtbtnSiz,hExtbtnSiz) = widgetExitBtnSiz res
    font' = font res

type VertexPosition = (GLfloat,GLfloat,GLfloat)
blockNodeVertex :: ( VertexPosition, VertexPosition
                   , VertexPosition, VertexPosition
                   , VertexPosition, VertexPosition
                   , VertexPosition, VertexPosition
                   )
blockNodeVertex = 
  ( ( -0.5, -0.5, -0.5) -- ^ P0
  , (  0.5, -0.5, -0.5) -- ^ P1
  , (  0.5, -0.5,  0.5) -- ^ P2
  , ( -0.5, -0.5,  0.5) -- ^ P3
  , (  0.5,  0.5,  0.5) -- ^ P4
  , ( -0.5,  0.5,  0.5) -- ^ P5
  , ( -0.5,  0.5, -0.5) -- ^ P6
  , (  0.5,  0.5, -0.5) -- ~ P7
  )

drawBackGroundBox :: [GLuint] -> IO ()
drawBackGroundBox [ftex',rtex',batex',ltex',ttex',botex'] = 
  preservingMatrix $ do
    texture Texture2D $= Enabled 
    color $ Color4 0.7 0.7 0.7 (0.8::GLfloat)
    glBindTexture gl_TEXTURE_2D ftex'
    renderPrimitive Quads $ 
      genSuf (0,0,-1) $ zip [uv0,uv1,uv2,uv3] [p6,p7,p1,p0] -- Front
    glBindTexture gl_TEXTURE_2D rtex'
    renderPrimitive Quads $ 
      genSuf (-1,0,0) $ zip [uv1,uv0,uv3,uv2] [p4,p7,p1,p2] -- Right
    glBindTexture gl_TEXTURE_2D ltex'
    renderPrimitive Quads $ 
      genSuf (1,0,0) $ zip [uv1,uv0,uv3,uv2] [p6,p5,p3,p0] -- Left
    glBindTexture gl_TEXTURE_2D ttex'
    renderPrimitive Quads $ 
      genSuf (0,-1,0) $ zip [uv1,uv0,uv3,uv2] [p5,p4,p7,p6] -- Top
    glBindTexture gl_TEXTURE_2D botex'
    renderPrimitive Quads $ 
      genSuf (0,1,0) $ zip [uv1,uv0,uv3,uv2] [p1,p0,p3,p2] -- Bottom
    glBindTexture gl_TEXTURE_2D batex'
    renderPrimitive Quads $ 
      genSuf (0,0,1) $ zip [uv1,uv0,uv3,uv2] [p4,p5,p3,p2] -- Back  
  where
    genSuf :: (GLfloat,GLfloat,GLfloat)
           -> [((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
           -> IO ()
    genSuf (nx,ny,nz) ndlst = do
      normal $ Normal3 nx ny nz
      mapM_ (\ ((u,v),(x', y', z')) -> do
               glTexCoord2f u v
               vertex (Vertex3 x' y' z')) ndlst
    --uvLst = [uv0,uv1,uv2,uv3]
    (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex
    uv0 = (0.0,0.0)
    uv1 = (1.0,0.0)
    uv2 = (1.0,1.0)
    uv3 = (0.0,1.0)

drawBackPlane :: (GLfloat,GLfloat) -> (GLfloat,GLfloat)
              -> Maybe GLuint -> (GLfloat,GLfloat) -> (GLfloat,GLfloat)
              -> (GLfloat,GLfloat,GLfloat,GLfloat) -> IO ()
drawBackPlane (xo,yo) (w,h) tex' (u0,v0) (tw,th) (r,g,b,a) =
  preservingMatrix $ do
    color $ Color4 r g b a 
    case tex' of
      Just t -> do
        texture Texture2D $= Enabled 
        glBindTexture gl_TEXTURE_2D t
        renderPrimitive Quads $ do
          glTexCoord2f u0 (v0 + th)
          vertex $ Vertex2 xo  (yo::GLfloat)
          glTexCoord2f (u0 + tw) (v0 + th)
          vertex $ Vertex2 (xo + w) (yo::GLfloat)
          glTexCoord2f (u0 + tw) v0
          vertex $ Vertex2 (xo + w) (yo + h)
          glTexCoord2f u0 v0
          vertex $ Vertex2 xo  (yo + h)
      Nothing -> do
        texture Texture2D $= Disabled
        renderPrimitive Quads $ do
          vertex $ Vertex2 xo  (yo::GLfloat)
          vertex $ Vertex2 (xo + w) (yo::GLfloat)
          vertex $ Vertex2 (xo + w) (yo + h)
          vertex $ Vertex2 xo  (yo + h)


{-

  GLFW.setFramebufferSizeCallback win' (Just $ resizeWinCallbk runMode' )
  GL.lighting          $= GL.Disabled

        -> FilePath -- Dome Mqofile Path
       -> FilePath -- Dome Texture Path
       -> FilePath -- Icon Texture Path
       -> FilePath -- Font Path
       -> IO ViewCtrl
domeMqoFilePath domeTextureFile sysIconTextureFile fontPath 
  --
  --GLFW.swapInterval    $= 5  --
  
  -- OpenGL initial setting
  initGLParam
  initSunlight

  --

  --font' <- Ft.createTextureFont fontPath
  font' <- Ft.createBitmapFont fontPath
  domeData' <- loadDomeData domeMqoFilePath domeTextureFile
  cur <- gen3dCursol 
  Just ot <- GLFW.getTime
  ot' <- newIORef ot
  blkList' <- newIORef []
  sysIcontex <- loadTextures sysIconTextureFile 
  uiState <- initUIState
  return ViewCtrl
    { window = win
    , keyStat = opeKeyStat
    , musStat = opeMouseStat
    , font = font'
    , curObj = cur
    , oldTime = ot'
    , domeData = domeData'
    , View.blkList = blkList' 
    , sysIcon = sysIcontex
    , uiState = uiState 
    }

resizeWinCallbk :: IORef RunMode -> GLFW.FramebufferSizeCallback
resizeWinCallbk runMode' win w 0 = resizeWinCallbk runMode' win w 1 -- prevent divide by zero
resizeWinCallbk runMode' _   w h = do
  m' <- readIORef runMode'
  setPerspective m' w h

      -- OpenGL Viewport.
      (w,h) <- GLFW.getFramebufferSize win'
      setPerspective TitleMode w h
-}



