module View
  ( RunMode (..)
  , GLFWHandle (..)
  , GuiResource (..)
  , ViewMode (..)
  , loadGuiResource
  , initGL
  , initGLFW
  , exitGLFW
  , getDeltTime
  , getButtonClick
  , getSystemKeyOpe
  , getCursorMotion
  , getScrollMotion
  , getMoveKeyOpe
  , loadTextures
  , setPerspective
  , gen3dCursol
  , drawBackPlane
  , getVertexList
  , putTextLine
  , drawBackGroundBox
  ) where
-- Font
import Graphics.Rendering.FTGL as Ft

-- OpenGL
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Codec.Picture as CdP
import qualified Data.Vector.Storable as Vct

-- GLFW
import qualified Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Maybe ( fromJust )

-- Common
import Debug.Trace as Dbg
import Control.Monad ( {-unless,void,when,-} void, replicateM )
import Types

-- Define
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

type VertexPosition = (GLfloat,GLfloat,GLfloat)

-- ##################### OpenGL ###########################
data ViewMode = V2DMode | V3DTitleMode | V3DMode
  deriving (Eq,Show)

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

getVertexList :: Surface -> [(GLfloat,GLfloat,GLfloat)]
getVertexList f = case f of
  STop    -> [p7,p6,p5,p4]
  SBottom -> [p0,p1,p2,p3] 
  SFront ->  [p6,p7,p1,p0]
  SBack ->   [p4,p5,p3,p2]
  SRight ->  [p7,p4,p2,p1]
  SLeft ->   [p5,p6,p0,p3]
  where
    (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex

genUVList :: [(GLfloat,GLfloat)] 
genUVList = [uv0,uv1,uv2,uv3]
  where
    uv0 = (0.0,0.0) ; uv1 = (1.0,0.0)
    uv2 = (1.0,1.0) ; uv3 = (0.0,1.0)

setPerspective :: ViewMode -> Int -> Int -> IO ()
setPerspective viewMode' w h = do
  matrixMode $= Projection
  viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  loadIdentity
  
  case viewMode' of
    V2DMode -> ortho2D 0 (realToFrac w) 0 (realToFrac h)
    V3DMode -> perspective 60 (realToFrac w/realToFrac h) 0.05 300
    V3DTitleMode -> perspective 100 (realToFrac w/realToFrac h) 0.05 300
      
  matrixMode $= Modelview 0
  loadIdentity

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

-- | 最小単位のブロックを囲う枠を描画する
-- Private
--
gen3dCursol :: IO () 
gen3dCursol = renderPrimitive LineLoop
  $ drawCurLine $ map ajust [p5,p4,p7,p6] -- BackFace
  where
    drawCurLine = mapM_ (\ (x, y, z) -> vertex (Vertex3 x y z)) 
    extnd v = if v > 0 then v + 0.05 else  v - 0.05
    (_,_,_,_,p4,p5,p6,p7) = blockNodeVertex
    ajust (a,b,c) = ( extnd a, extnd b, extnd c)

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

-- ##################### Font(Text) #######################

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

drawBackGroundBox :: [GLuint] -> IO ()
drawBackGroundBox [ftex',rtex',batex',ltex',ttex',botex'] = 
  preservingMatrix $ do
    texture Texture2D $= Enabled 
    color $ Color4 0.7 0.7 0.7 (0.8::GLfloat)
    mapM_ (\ (tex, nor, f) -> do
      glBindTexture gl_TEXTURE_2D tex
      renderPrimitive Quads $ genSuf nor f)
        [ (ftex',(0,0,-1), SFront)
        , (rtex',(-1,0,0), SRight)
        , (ltex',(1,0,0),  SLeft)
        , (ttex',(0,-1,0), STop)
        , (botex',(0,1,0), SBottom)
        , (batex',(0,0,1), SBack)
        ]
  where
    genSuf :: (GLfloat,GLfloat,GLfloat) -> Surface -> IO ()
    genSuf (nx,ny,nz) f = do
      normal $ Normal3 nx ny nz
      mapM_ (\ ((u,v),(x', y', z')) -> do
               glTexCoord2f u v
               vertex (Vertex3 x' y' z')) 
           $ (zip genUVList . getVertexList) f

-- ##################### GLFW #############################

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
  oldTime' <- newIORef 0.0 
  --
  GLFW.makeContextCurrent win
  case win of
    Just win' -> do
      -- Callback
      GLFW.setWindowCloseCallback win' (Just $ finishGLFW exitFlg')
      GLFW.setKeyCallback win' (Just (keyPress keyStat'))
      GLFW.setCursorPosCallback win'
                           (Just (setCursorMotion runMode' mouseStat'))
      GLFW.setMouseButtonCallback win' (Just (setButtonClick mouseStat'))
      GLFW.setScrollCallback win' (Just (setScrollMotion mouseStat'))
    Nothing -> return ()

  return GLFWHandle 
    { winHdl = win
    , mouseStat = mouseStat'
    , keyStat = keyStat'
    , exitFlg = exitFlg'
    , runMode = runMode'
    , oldTime = oldTime'
    }

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

getDeltTime :: GLFWHandle -> IO Double
getDeltTime glfwHdl = do 
  Just newTime' <- GLFW.getTime  
  oldTime' <- readIORef $ oldTime glfwHdl 
  writeIORef (oldTime glfwHdl) newTime'
  return $ newTime' - oldTime'

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


