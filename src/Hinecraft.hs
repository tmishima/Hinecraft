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
import View
import Util

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

data ViewMode = V2DMode | V3DTitleMode | V3DMode
  deriving (Eq,Show)

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
runHinecraft (glfwHdl,guiRes,wldRes) = do
  wld <- genWorldData 
  sfl <- genSurfaceList wld
  dsps <- genWorldDispList sfl 
  useStat' <- newIORef $ UserStatus
      { userPos = (0.0,16 * 3 + 1,0.0)
      , userRot = (0.0,0.0,0.0) 
      , palletIndex = 2
      } 
  _ <- getDeltTime glfwHdl
  mainLoop useStat' (wld,sfl,dsps)
  where
    mainLoop u' (w',f',d') = do
      GLFW.pollEvents
      dt <- getDeltTime glfwHdl
      exitflg' <- readIORef $ exitFlg glfwHdl
      exitkey' <- mainProcess (glfwHdl,guiRes) u' dt
      u'' <- readIORef u'
      drawView (glfwHdl,guiRes,wldRes,u'',d')
      unless (exitflg' || exitkey') $ mainLoop u' (w',f',d')

getDeltTime :: GLFWHandle -> IO Double
getDeltTime glfwHdl = do 
  Just newTime' <- GLFW.getTime  
  oldTime' <- readIORef $ oldTime glfwHdl 
  writeIORef (oldTime glfwHdl) newTime'
  return $ newTime' - oldTime'

mainProcess :: (GLFWHandle, GuiResource) -> IORef UserStatus
            -> Double -> IO Bool
mainProcess (glfwHdl, guiRes) usrStat dt = do
  -- Common User input
  mous <- getButtonClick glfwHdl
  syskey <- getSystemKeyOpe glfwHdl
  -- 
  mode' <- readIORef $ runMode glfwHdl
  (newMode, extflg) <- case mode' of
    TitleMode -> guiProcess guiRes mous -- ### 2D ###
    PlayMode -> do
      u' <- readIORef usrStat
      (md,exflg,newStat) <- playProcess glfwHdl u' syskey dt
      writeIORef usrStat newStat
      return (md,exflg)
  when (mode' /= newMode) $ writeIORef (runMode glfwHdl) newMode
  return extflg

playProcess :: GLFWHandle -> UserStatus -> (Bool,Bool)
            -> Double -> IO (RunMode,Bool,UserStatus)
playProcess glfwHdl usrStat (esc,_ {-tool-}) dt = do
  vm <- getCursorMotion glfwHdl
  sm <- getScrollMotion glfwHdl 
  mm <- getMoveKeyOpe glfwHdl
  nst <- calcPlayerMotion usrStat vm mm dt
  return $ if esc
             then (TitleMode, False, nst)
             else (PlayMode, False, nst)

calcPlayerMotion :: UserStatus -> (Int,Int) -> (Int,Int,Int,Int,Int)
                 -> Double
                 -> IO UserStatus
calcPlayerMotion usrStat (sx,sy) (f,b,l,r,jmp) dt = do
  let (nrx,nry,nrz) = playerView dt ( realToFrac orx
                                    , realToFrac ory
                                    , realToFrac orz)
                      ((fromIntegral sy)*0.5, (fromIntegral sx)*0.5, 0)
      (ndx,ndy,ndz) = playerMotion dt nry ( fromIntegral (f - b)*4
                                          , fromIntegral (r - l)*4
                                          , 0)                
  {-
  let w = if jmp == 2
            then 5.0
            else maximum [ wo - (9.8 * dt), -9.8 ]
  writeIORef wHold w
  -}
  return UserStatus
    { userPos = ( ox + realToFrac ndx
                , oy + realToFrac ndy
                , oz + realToFrac ndz)
    , userRot = (realToFrac nrx, realToFrac nry, realToFrac nrz) 
    , palletIndex = palletIndex usrStat
    }

  where
    (ox,oy,oz) = userPos usrStat
    (orx,ory,orz) = userRot usrStat 

type Pos' = (Double,Double,Double)
type Vel' = Pos'
playerView :: Double -> Pos' -> Pos' -> Pos' 
playerView dt (frxo,fryo,frzo) (frx,fry,frz) = (rx, ry, rz) 
  where
    rx = bounded' 90 (-90) $ realToFrac frxo + frx*dt
    rz = bounded' 90 (-90) $ realToFrac frzo + frz*dt
    try = realToFrac fryo + fry*dt
    ry | try > 360    = try - 360
       | try < (-360) = try + 360
       | otherwise    = try 

playerMotion :: Double -> Double -> Vel' -> Pos' 
playerMotion dt r0 (v,u,w) = (dx,dy,dz)
  where
    dx = dt * nextx (v,u,r0)    -- right/left
    dy = dt * w                 -- up/down
    dz = dt * nextz (v,u,r0)    -- head
    phy r = (pi * r) / 180.0
    nextz (df,ds,r) = (df * cos (phy r)) + (ds * sin (phy r))
    nextx (df,ds,r) = (ds * cos (phy r)) - (df * sin (phy r))


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

drawView :: ( GLFWHandle, GuiResource, WorldResource
            , UserStatus, WorldDispList)
         -> IO ()
drawView (glfwHdl, guiRes, wldRes, usrStat, worldDispList) = do
  clear [ColorBuffer,DepthBuffer]
  mode' <- readIORef $ runMode glfwHdl
  winSize <- GLFW.getFramebufferSize win

  case mode' of
    TitleMode -> do -- ### 2D ###
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
      drawTitle winSize guiRes
    PlayMode -> do
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
      drawPlay winSize guiRes wldRes usrStat worldDispList
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
  return WorldResource
    { blockTexture = btex'
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"

data BlockInfo = BlockInfo
  { textureIndex :: [Int]
  , alpha :: Bool 
  , enter :: Bool
  }

getBlockInfo :: BlockID -> BlockInfo
getBlockInfo bid = fromJust $ lookup bid db
  where
    db = [ (voidBlockID, BlockInfo
             { textureIndex = []
             , alpha = True
             , enter = True
             })
         , (stoneBlockID, BlockInfo
             { textureIndex = []
             , alpha = False
             , enter = False
             })
         , (dirtBlockID, BlockInfo
             { textureIndex = []
             , alpha = False
             , enter = False
             })
         ]

test = do
  wld <- genWorldData 
  a <- getBlockID wld (0,0,0)
  setBlockID wld (0,0,0) 0
  b <- getBlockID wld (0,0,0) 
  print (a,b)
  lst <- mapM (getSurface wld) [(0,y) | y <- [0 .. 7]]
  print lst
  return ()


data Surface = STop | SBottom | SRight | SLeft | SFront | SBack 
  deriving (Show,Eq)

type WorldIndex = (Int,Int,Int)

data ChunkParam = ChunkParam
  { blockSize :: Int
  , blockNum  :: Int
  }

chunkParam :: ChunkParam
chunkParam = ChunkParam
  { blockSize = 16
  , blockNum = 8
  }

type SurfacePos = [(WorldIndex,BlockID,[Surface])] 

setBlockID :: WorldData -> WorldIndex -> BlockID
           -> IO ()
setBlockID wld (x,y,z) bid = do
  chl <- readIORef chlist
  case getChunk chl (x,y,z) of
    Just (_,c) -> setBlockIDfromChunk c (x,y,z) bid
    Nothing -> return () 
  where
    chlist = chunkList wld

getSurface :: WorldData -> (ChunkNo,Int) 
           -> IO SurfacePos 
getSurface wld (chNo,bkNo) = do
  blkpos <- getCompliePosList wld (chNo,bkNo)
  blks <- mapM (getBlockID wld) blkpos
            >>= return . (filter (\ (_,bid) -> bid /= voidBlockID ))
                       . (zip blkpos)
  blks' <- mapM (\ (pos,bid) -> do
    fs <- getSuf pos
    return (pos,bid,fs)) blks
  return $ (filter (\ (_,_,fs) -> not $ null fs)) blks'
  where
    getAroundIndex (x',y',z') = [ (SRight,(x' + 1, y', z'))
                                , (SLeft, (x' - 1, y', z'))
                                , (STop, (x', y' + 1, z'))
                                , (SBottom, (x', y' - 1, z'))
                                , (SBack,(x', y', z' + 1))
                                , (SFront,(x', y', z' - 1))
                                ]
    getSuf (x',y',z') = do
      f' <- mapM (\ (f,pos) -> do
        b <- getBlockID wld pos
        return (f,b) 
        ) (getAroundIndex (x',y',z')) 
      return $ map fst $ filter chk f'
      where
        chk :: (a,BlockID) -> Bool
        chk (_,b) = b == voidBlockID
                    || if b == (-1) 
                         then False
                         else (alpha (getBlockInfo b))


getCompliePosList :: WorldData -> (ChunkNo,Int) -> IO [WorldIndex]
getCompliePosList wld (chNo,blkNo) = do
  chunk <- fmap (fromJust . (lookup chNo)) (readIORef $ chunkList wld)
  let (x',z') = origin chunk 
      y' = blkNo * bsize
      (sx,sy,sz) = (f x', f y', f z')
  return [(x,y,z) | x <- [sx .. sx + bsize - 1]
                  , y <- [sy .. sy + bsize - 1]
                  , z <- [sz .. sz + bsize - 1]]
  where
    bsize = blockSize chunkParam
    f a = bsize * (div a bsize)
    

setBlockIDfromChunk :: Chunk -> WorldIndex -> BlockID -> IO ()
setBlockIDfromChunk c (x,y,z) bid = writeArray arr idx bid
  where
    bsize = blockSize chunkParam
    (lx,ly,lz) = (x - ox, y - (div y bsize) * bsize, z - oz )
    (ox,oz) = origin c
    dat = local c
    arr = fromJust $ lookup (div y bsize) dat
    idx = (bsize ^ (2::Int)) * ly + bsize * lz + lx

getBlockID :: WorldData -> WorldIndex -> IO BlockID
getBlockID wld (x,y,z) = do
  chl <- readIORef chlist
  case getChunk chl (x,y,z) of
    Just (_,c) -> getBlockIDfromChunk c (x,y,z)
    Nothing -> return (-1)
  where
    chlist = chunkList wld

getBlockIDfromChunk :: Chunk -> WorldIndex -> IO BlockID
getBlockIDfromChunk c (x,y,z) = readArray arr idx
  where
    bsize = blockSize chunkParam
    (lx,ly,lz) = (x - ox, y - (div y bsize) * bsize, z - oz)
    (ox,oz) = origin c
    dat = local c
    arr = fromJust $ lookup (div y bsize) dat
    idx = (bsize * bsize) * ly + bsize * lz + lx

getChunk :: [(ChunkNo,Chunk)] -> WorldIndex -> Maybe (ChunkNo,Chunk)
getChunk [] _ = Nothing 
getChunk (c:cs) (x,y,z) | (ox <= x) && (x < ox + bsize) &&
                          (oz <= z) && (z < oz + bsize) &&
                          (ymin <= y) && (y < ymax ) = Just c  
                        | otherwise = getChunk cs (x,y,z)
  where
    (ox,oz) = origin $ snd c
    (ymin,ymax) = (0,(blockSize chunkParam) * (blockNum chunkParam))
    bsize = blockSize chunkParam

type SurfaceList = IORef [(ChunkNo, [(Int,IORef SurfacePos)])]
type WorldDispList = IORef [(ChunkNo, [(Int,DisplayList)])]

getSuface :: SurfaceList -> (ChunkNo,Int) -> IO (Maybe SurfacePos)
getSuface suf (chNo,bNo) = do
  suf' <- readIORef suf
  case lookup chNo suf' of
    Just chl -> case lookup bNo chl of
      Just blk -> readIORef blk >>= return . Just
      Nothing -> return Nothing
    Nothing -> return Nothing

genWorldDispList :: SurfaceList -> IO WorldDispList
genWorldDispList suf = do
  suf' <- readIORef suf
  mapM (\ (chNo,blks) -> do
    sb <- mapM (\ (bNo,sfs) -> do
      sfs' <- readIORef sfs
      d <- genSufDispList sfs'
      return (bNo,d)) blks
    return (chNo,sb)) suf'
    >>= newIORef

genSufDispList :: SurfacePos -> IO DisplayList
genSufDispList bsf = do
  defineNewList Compile $ do
    --glBindTexture gl_TEXTURE_2D (tex !! (\ (_,m,_) -> m) (head elmTri))
    texture Texture2D $= Disabled
    renderPrimitive Quads $ mapM_ genFace bsf 
  where
    genFace ((x,y,z),bid,fs) = mapM_ (\ f -> case f of
         STop -> topFace
         SBottom -> btmFace
         SFront -> frtFace
         SBack -> bakFace
         SRight -> rhtFace
         SLeft -> lftFace) fs
      where
        btmFace = genSuf (0,-1,0) $ zip uvLst [p2,p3,p0,p1] -- Bottom
        bakFace = genSuf (0,0,1) $ zip uvLst [p4,p5,p3,p2] -- Back
        frtFace = genSuf (0,0,-1) $ zip uvLst [p6,p7,p1,p0] -- Front
        rhtFace = genSuf (1,0,0) $ zip uvLst [p4,p2,p1,p7] -- Right
        lftFace = genSuf (-1,0,0) $ zip uvLst [p3,p5,p6,p0] -- Left
        topFace = genSuf (0,-1,0) $ zip uvLst [p5,p4,p7,p6] -- Top
        genSuf :: (GLfloat,GLfloat,GLfloat)
               -> [((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
               -> IO ()
        genSuf (nx,ny,nz) ndlst = do
          --normal $ Normal3 nx ny nz
          mapM_ (\ ((u,v),(x', y', z')) -> do
                   -- (glTexCoord2f u v)
                   vertex (Vertex3 (x' + fromIntegral x)
                                   (y' + fromIntegral y)
                                   (z' + fromIntegral z))) ndlst
        uvLst = [uv0,uv1,uv2,uv3]
        (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex
        uv0 = (0.0,0.0)
        uv1 = (1.0,0.0)
        uv2 = (1.0,1.0)
        uv3 = (0.0,1.0)
  
{-
        --putStrLn $ unwords ["box (x,y,z)",show (x,y,z) ]
      where

-- [(WorldIndex,BlockID,[Surface])]
-}


genSurfaceList :: WorldData -> IO SurfaceList
genSurfaceList wld = readIORef chl
  >>= mapM (\ (cNo,_) -> do
    spos <- mapM (\ b -> do
      fs' <- getSurface wld (cNo,b)
      fs <- newIORef fs'
      return (b,fs)) [0 .. bkNo]
    return (cNo,spos)) 
      >>= newIORef
  where
    chl = chunkList wld 
    bkNo = blockNum chunkParam - 1

genWorldData :: IO WorldData
genWorldData = do
  cl <- newIORef . (zip [0 ..]) =<< mapM genChunk
          [ (x,z) | x <- [-16,0 .. 16], z <- [-16,0 .. 16] ]
        --  [ (x,z) | x <- [-64,-48 .. 48], z <- [-64,-48 .. 48] ]
  return $ WorldData 
    { chunkList =  cl
    }

type ChunkNo = Int
data WorldData = WorldData
  { chunkList :: IORef [(ChunkNo,Chunk)]
  }

data Chunk = Chunk
  { origin :: (Int,Int)
  , local :: [(Int,(IOUArray Int Int))]
  }

type BlockID = Int
voidBlockID :: BlockID
voidBlockID = 0 
stoneBlockID :: BlockID
stoneBlockID = 1 
dirtBlockID :: BlockID
dirtBlockID = 2 

genChunk :: (Int,Int) -> IO Chunk
genChunk org = do
  arrt <- replicateM 4
    (newArray (0, blength) voidBlockID) :: IO [IOUArray Int Int]

  arrs <- newArray (0,blength) voidBlockID :: IO (IOUArray Int Int)
  mapM_ (\ i -> writeArray arrs i dirtBlockID) [0 .. 16 * 16 * 2 - 1]

  arrb <- replicateM 3
    (newArray (0,blength) stoneBlockID) :: IO [IOUArray Int Int]

  return Chunk
    { origin = org
    , local = zip [0 .. ] $ arrb ++ (arrs : arrt)
    } 
  where
    blength = (blockSize chunkParam) ^ (3::Int) -1

drawPlay :: (Int,Int) -> GuiResource -> WorldResource
         -> UserStatus -> WorldDispList -> IO ()
drawPlay (w,h) guiRes wldRes usrStat worldDispList = do
  -- World
  preservingMatrix $ do
    setPerspective V3DMode w h
  
    -- 視線
    rotate (-urx) $ Vector3 1.0 0.0 (0.0::GLfloat)
    rotate (-ury) $ Vector3 0.0 1.0 (0.0::GLfloat) -- z軸
 
    preservingMatrix $ do
      scale 100.0 100.0 (100.0::GLfloat)
      drawBackGroundBox' 

    -- カメラ位置
    translate $ Vector3 (-ux) (-uy) (-uz) 
    color $ Color3 0.0 1.0 (0.0::GLfloat)
    readIORef worldDispList
      >>= mapM_ (\ (_,b) -> do
        mapM_ (\ (_,d) -> callList d) b)  

  -- HUD
  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
    depthMask $= Disabled

    -- Pallet
    drawBackPlane (394,2) (65*9, 69) (Just widTex')
                  (0,0) (183/256,22/256) (1.0,1.0,1.0,1.0)
    let curXpos = 394 + (fromIntegral pIndex) * 64
    drawBackPlane (curXpos,2) (64, 64) (Just widTex')
                  (0,24/256) (22/256,22/256) (1.0,1.0,1.0,1.0)

    glPopAttrib 
    depthMask $= Enabled
    glEnable gl_DEPTH_TEST

  where
    (ux,uy,uz) = userPos usrStat 
    (urx,ury,_) = userRot usrStat 
    pIndex =  palletIndex usrStat
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
      mapM_ (\ (x', y', z') ->
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
  cur <- gen3dCursol 
  Just ot <- GLFW.getTime
  ot' <- newIORef ot
  blkList' <- newIORef []
  sysIcontex <- loadTextures sysIconTextureFile 
  uiState <- initUIState

-}



