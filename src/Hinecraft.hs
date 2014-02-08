import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Data.IORef
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( unless,void,when,replicateM, {-filterM-} )
import Data.Maybe ( fromJust,isJust )
import System.Directory ( getHomeDirectory )
import Graphics.Rendering.FTGL as Ft
import Data.Array.IO
import Data.Tuple
import Data.List
import Data.Ord
import Control.Concurrent
import View
import Util
import Model

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
runHinecraft resouce@(glfwHdl,guiRes,wldRes) = do
  wld <- genWorldData 
  sfl <- genSurfaceList wld
  dsps <- genWorldDispList wldRes sfl 
  rotwld <- newIORef (0.0 :: Double)
  useStat <- newIORef $ UserStatus
      { userPos = (0.0,16 * 4 + 1,0.0)
      , userRot = (0.0,0.0,0.0) 
      , palletIndex = 0
      , userVel = (0.0,0.0,0.0)
      } 
  _ <- getDeltTime glfwHdl
  mainLoop rotwld useStat (wld,sfl,dsps) 
  where
    mainLoop r' u' (w',f',d') = do
      GLFW.pollEvents
      --threadDelay 100000
      dt <- getDeltTime glfwHdl
      exitflg' <- readIORef $ exitFlg glfwHdl
      (exitkey',pos) <- mainProcess resouce r' w' u' f' d' dt
      drawView (glfwHdl,guiRes,wldRes) r' u' d' pos
      unless (exitflg' || exitkey') $ mainLoop r' u' (w',f',d')

mainProcess :: (GLFWHandle, GuiResource, WorldResource) -> IORef Double 
            -> WorldData -> IORef UserStatus -> SurfaceList
            -> WorldDispList -> Double
            -> IO (Bool,Maybe (WorldIndex,Surface))
mainProcess (glfwHdl, guiRes, wldRes) rotW wld usrStat
            sufList dsps dt = do
  -- Common User input
  mous <- getButtonClick glfwHdl
  syskey <- getSystemKeyOpe glfwHdl
  -- 
  mode' <- readIORef $ runMode glfwHdl
  (newMode, extflg,pos) <- case mode' of
    TitleMode -> do
      modifyIORef rotW (\ r -> let nr = (r + (1.0 * dt))
                               in if nr > 360 then nr - 360 else nr ) 
      (md,exflg') <- guiProcess guiRes mous -- ### 2D ###
      return (md,exflg',Nothing)
    PlayMode -> do
      u' <- readIORef usrStat
      (md,exflg,newStat) <- playProcess glfwHdl wld u' syskey dt
      writeIORef usrStat newStat
      pos <- calcCursorPos wld sufList u'
      mouseOpe wld newStat mous pos
        (updateDisplist wldRes wld dsps sufList)
      return (md,exflg,pos)
  when (mode' /= newMode) $ writeIORef (runMode glfwHdl) newMode
  return (extflg,pos)

updateDisplist :: WorldResource -> WorldData -> WorldDispList
               -> SurfaceList -> WorldIndex -> IO ()
updateDisplist wldRes wld dsps sufList pos = do 
  clst <- calcReGenArea wld pos 
  dsps' <- readIORef dsps
  mapM_ (\ (cNo',bNo') -> do
    case lookup cNo' dsps' of
      Just d -> do
        let (_,d') = d !! bNo'
        sfs <- getSurface wld (cNo',bNo')
        setSurfaceList sufList (cNo',bNo') sfs  
        genSufDispList wldRes sfs (Just d')
        return ()
      Nothing -> return ()
    ) clst

type BlockNo = Int
calcReGenArea :: WorldData -> WorldIndex -> IO [(ChunkNo,BlockNo)]
calcReGenArea wld (x,y,z) = do
  chl <- readIORef chlist
  case getChunk chl (x,y,z) of
    Nothing -> return [] 
    Just (cNo,_) -> return $
         map (\ bno' -> (cNo,bno')) blknos 
      ++ map (\ cno' -> (cno',bNo)) (cNoX chl)
      ++ map (\ cno' -> (cno',bNo)) (cNoZ chl)
  where
    chlist = chunkList wld
    bNo = div y 16
    blknos | (mod y 16) == 0 && bNo > 0 = [bNo,bNo - 1]
           | (mod y 16) == 15 && bNo < 7 = [bNo,bNo + 1]
           | otherwise = [bNo]
    cNoX chl | (mod x 16) == 0 = case getChunk chl (x - 1,y,z) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | (mod x 16) == 15 = case getChunk chl (x + 1,y,z) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | otherwise = []
    cNoZ chl | (mod z 16) == 0 = case getChunk chl (x,y,z - 1) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | (mod z 16) == 15 = case getChunk chl (x,y,z + 1) of
                                   Nothing -> [] 
                                   Just (cNo,_) -> [cNo]
             | otherwise = []

setSurfaceList :: SurfaceList -> (Int,Int) -> SurfacePos -> IO ()
setSurfaceList sufList (cNo,bNo) sfs = do
  s <- readIORef sufList
  let b = fromJust $ lookup bNo $ fromJust $ lookup cNo s 
  writeIORef b sfs 
  return ()

mouseOpe :: WorldData -> UserStatus -> (Double,Double,Bool,Bool,Bool)
         -> Maybe (WorldIndex,Surface) -> (WorldIndex -> IO ())
         -> IO ()
mouseOpe _ _ _ Nothing _ = return ()
mouseOpe _ _ (_,_,False,False,_) _ _ = return ()
mouseOpe wld ustat (_,_,lb,rb,_) (Just ((cx,cy,cz),fpos)) callback
  | rb == True = do
    setBlockID wld setPos idx 
    callback setPos
  | lb == True = do
    setBlockID wld (cx,cy,cz) 0 
    callback (cx,cy,cz)
  | otherwise = return ()
  where
    idx = palletIndex ustat 
    setPos = case fpos of
      STop    -> (cx,cy + 1,cz)
      SBottom -> (cx,cy - 1,cz)
      SRight  -> (cx + 1,cy,cz)
      SLeft   -> (cx - 1,cy,cz)
      SFront  -> (cx,cy,cz - 1)
      SBack   -> (cx,cy,cz + 1)

playProcess :: GLFWHandle -> WorldData -> UserStatus
            -> (Bool,Bool) -> Double
            -> IO (RunMode,Bool,UserStatus)
playProcess glfwHdl wld usrStat (esc,_ {-tool-}) dt = do
  vm <- getCursorMotion glfwHdl
  sm <- getScrollMotion glfwHdl 
  mm <- getMoveKeyOpe glfwHdl
  nst <- calcPlayerMotion wld usrStat vm mm sm dt
  return $ if esc
             then (TitleMode, False, nst)
             else (PlayMode, False, nst)

calcPlayerMotion :: WorldData -> UserStatus
                 -> (Int,Int) -> (Int,Int,Int,Int,Int) -> (Int,Int)
                 -> Double
                 -> IO UserStatus
calcPlayerMotion wld usrStat (mx,my) (f,b,l,r,jmp) (_,sy) dt = do
  w <- getBlockID wld (round' ox,(round' oy) - 1,round' oz) 
         >>= (\ bid -> return $ if bid == voidBlockID
            then  wo - (9.8 * dt)
            else  if jmp == 2  then 8.0 else if wo > 0 then wo else 0.0)
  let (nrx,nry,nrz) = playerView dt ( realToFrac orx
                                    , realToFrac ory
                                    , realToFrac orz)
                        ((fromIntegral my)*0.5, (fromIntegral mx)*0.5, 0)
      (dx,dy,dz) = playerMotion dt nry ( fromIntegral (b - f)*4
                                       , fromIntegral (r - l)*4
                                       , w)                
     
      idx = (palletIndex usrStat) - sy 
  (nx,ny,nz) <- chkMove w (ox,oy,oz) (realToFrac dx
                                     ,realToFrac dy
                                     ,realToFrac dz)
  return UserStatus
    { userPos = ( nx
                , ny 
                , nz)
    , userRot = (realToFrac nrx, realToFrac nry, realToFrac nrz) 
    , palletIndex = if idx < 0 then 0 else if idx > 8 then 8 else idx
    , userVel = (0.0,w,0.0)
    }

  where
    (ox,oy,oz) = userPos usrStat
    (orx,ory,orz) = userRot usrStat 
    (_,wo,_) = userVel usrStat
    chkMove w (ox',oy',oz') (dx,dy,dz)
      | dl < 1 = do
          let ny = if w == 0 then fromIntegral $ round' oy'
                             else oy' + realToFrac dy
              nx = ox' + realToFrac dx  
              nz = oz' + realToFrac dz
          return (nx,ny,nz)
      | otherwise = do
          let (dx',dy',dz') = (dx / dl, dy / dl, dz / dl)
              (tx,ty,tz) = if w == 0
                 then (ox' + dx', oy', oz' + dz')
                 else (ox' + dx', oy' + dy', oz' + dz')
          bid <- getBlockID wld (round' tx, round' ty, round' tz)
          w' <- getBlockID wld (round' tx, round' ty - 1, round' tz)
                 >>= (\ b' -> return $ if voidBlockID == b then w else 0)
          if bid == voidBlockID 
            then chkMove w' (tx,ty,tz) (dx - dx', dy - dy', dz - dz')
            else chkMove w' (tx,ty,tz) (0.0, dy - dy' , 0.0)
      where 
        dl = sqrt (dx * dx + dz * dz + dy * dy)


type Pos' = (Double,Double,Double)
type Rot' = (Double,Double,Double)
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
    nextz (df,ds,r) = (df * cos (phy r)) - (ds * sin (phy r))
    nextx (df,ds,r) = (ds * cos (phy r)) + (df * sin (phy r))


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

drawView :: ( GLFWHandle, GuiResource, WorldResource)
         -> IORef Double -> IORef UserStatus -> WorldDispList
         -> Maybe (WorldIndex,Surface)
         -> IO ()
drawView (glfwHdl, guiRes, wldRes) dw usrStat worldDispList pos = do
  clear [ColorBuffer,DepthBuffer]
  mode' <- readIORef $ runMode glfwHdl
  winSize <- GLFW.getFramebufferSize win

  case mode' of
    TitleMode -> do -- ### 2D ###
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
      readIORef dw >>= (drawTitle winSize guiRes) . realToFrac
    PlayMode -> do
      u' <- readIORef usrStat
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden
      drawPlay winSize guiRes wldRes u' worldDispList pos
  flush
  GLFW.swapBuffers win
  where
    win = fromJust $ winHdl glfwHdl

data UserStatus = UserStatus
  { userPos :: (GLfloat,GLfloat,GLfloat)
  , userRot :: (GLfloat,GLfloat,GLfloat) 
  , palletIndex :: Int
  , userVel :: (Double,Double,Double)
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
  deriving (Ord,Show,Eq)

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
                          (ymin <= y) && (y < ymax )
                           = Just c  
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

genWorldDispList :: WorldResource -> SurfaceList -> IO WorldDispList
genWorldDispList wldRes suf = do
  suf' <- readIORef suf
  mapM (\ (chNo,blks) -> do
    sb <- mapM (\ (bNo,sfs) -> do
      sfs' <- readIORef sfs
      d <- genSufDispList wldRes sfs' Nothing
      return (bNo,d)) blks
    return (chNo,sb)) suf'
    >>= newIORef

d2fv3 :: (Double,Double,Double) -> (GLfloat,GLfloat,GLfloat)
d2fv3 (a,b,c) = ( realToFrac a, realToFrac b, realToFrac c)

genSufDispList :: WorldResource -> SurfacePos -> Maybe DisplayList
               -> IO DisplayList
genSufDispList wldRes bsf dsp = do
  case dsp of
    Nothing -> defineNewList Compile $ gen'
    Just d -> do
                defineList d Compile $ gen'
                return d
  where
    gen' = do
      glBindTexture gl_TEXTURE_2D tex
      texture Texture2D $= Enabled
      renderPrimitive Quads $ mapM_ genFace bsf 
    tex = blockTexture wldRes
    genFace ((x,y,z),bid,fs) = do
      let texIdx = textureIndex $ getBlockInfo bid
           -- Top | Bottom | Right | Left | Front | Back
          [tt',tb',tr',tl',tf',tba'] = if null texIdx 
                                  then take 6 $ repeat (0,0)
                                  else texIdx
      mapM_ (\ f -> case f of
         STop -> topFace $ calcUV tt'
         SBottom -> btmFace $ calcUV tb'
         SFront -> frtFace $ calcUV tr'
         SBack -> bakFace $ calcUV tl'
         SRight -> rhtFace $ calcUV tf'
         SLeft -> lftFace $ calcUV tba') fs
      where
        setColor i = replicate 4 (d2fv3 $ bClr) 
          where       
            bClr = (bcolor $ getBlockInfo bid) !! i
        topFace uvLst = genSuf (0,-1,0)
          $ zip3 uvLst (setColor 0) [p5,p4,p7,p6]
        btmFace uvLst = genSuf (0,-1,0)
          $ zip3 uvLst (setColor 1) [p2,p3,p0,p1] 
        rhtFace uvLst = genSuf (1,0,0)
          $ zip3 uvLst (setColor 2) [p7,p4,p2,p1]
        lftFace uvLst = genSuf (-1,0,0)
          $ zip3 uvLst (setColor 3) [p5,p6,p0,p3]
        frtFace uvLst = genSuf (0,0,-1)
          $ zip3 uvLst (setColor 4) [p6,p7,p1,p0]
        bakFace uvLst = genSuf (0,0,1)
          $ zip3 uvLst (setColor 5) [p4,p5,p3,p2]
        genSuf :: (GLfloat,GLfloat,GLfloat)
               -> [((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat)
                  ,(GLfloat,GLfloat,GLfloat))]
               -> IO ()
        genSuf (nx,ny,nz) ndlst = do
          normal $ Normal3 nx ny nz
          mapM_ (\ ((u,v),(r,g,b),(x', y', z')) -> do
                   color $ Color3 r g b
                   glTexCoord2f u v
                   vertex (Vertex3 (x' + fromIntegral x)
                                   (y' + fromIntegral y)
                                   (z' + fromIntegral z))) ndlst
        (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex
        calcUV (i',j') = [ ( i * (1/16), j * (1/16)) 
                         , ( (i + 1) * (1/16), j * (1/16))
                         , ( (i + 1) * (1/16), (j + 1) * (1/16))
                         , ( i * (1/16), (j + 1) * (1/16))
                         ]
          where i = fromIntegral i' ; j = fromIntegral j'
 

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

genChunk :: (Int,Int) -> IO Chunk
genChunk org = do
  arrt <- replicateM 4
    (newArray (0, blength) voidBlockID) :: IO [IOUArray Int Int]

  arrs <- newArray (0,blength) voidBlockID :: IO (IOUArray Int Int)
  mapM_ (\ i -> writeArray arrs i dirtBlockID) [0 .. 16 * 16 * 2 - 1]
  mapM_ (\ i -> writeArray arrs i grassBlockID)
           [ 16 * 16 * 2 .. 16 * 16 * 3 - 1]

  arrb <- replicateM 3
    (newArray (0,blength) stoneBlockID) :: IO [IOUArray Int Int]

  return Chunk
    { origin = org
    , local = zip [0 .. ] $ arrb ++ (arrs : arrt)
    } 
  where
    blength = (blockSize chunkParam) ^ (3::Int) -1

drawPlay :: (Int,Int) -> GuiResource -> WorldResource
         -> UserStatus -> WorldDispList
         -> Maybe (WorldIndex,Surface)
         -> IO ()
drawPlay (w,h) guiRes wldRes usrStat worldDispList pos = do
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
    translate $ Vector3 (-ux) (-uy - 1.5) (-uz) 

    -- Cursol 選択された面を強調
    --Dbg.traceIO $ show (pos, uy)
    renderCurFace  pos

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
    let curXpos = 392 + (fromIntegral pIndex) * 64
    drawBackPlane (curXpos,2) (64, 64) (Just widTex')
                  (0,24/256) (22/256,22/256) (1.0,1.0,1.0,1.0)

    preservingMatrix $ do
      texture Texture2D $= Disabled
      color $ Color3 0.0 0.0 (0.0::GLfloat)
      lineWidth $= 2.0
      renderPrimitive Lines $ do
        vertex $ Vertex2 ((1366.0/2.0) - 7.0) (768.0/2.0 ::GLfloat)
        vertex $ Vertex2 ((1366.0/2.0) + 7.0) (768.0/2.0 ::GLfloat)
        vertex $ Vertex2 (1366.0/2.0) (768.0/2.0 - 7.0 ::GLfloat)
        vertex $ Vertex2 (1366.0/2.0) (768.0/2.0 + 7.0 ::GLfloat)

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

calcCursorPos :: WorldData -> SurfaceList -> UserStatus
              -> IO (Maybe (WorldIndex,Surface))
calcCursorPos wld sufList usr = do
  chl <- readIORef chlist
  f' <- readIORef sufList
  case getChunk chl ( round' ux , round' uy , round' uz) of
    Just (cNo,c) -> do
      let (bx,bz) = origin c
          cblkNo = div (round' uy) 16
          bplst = if cblkNo == 0
               then [cblkNo, cblkNo + 1]
               else if cblkNo > 6
                      then [cblkNo - 1, cblkNo]
                      else [cblkNo - 1, cblkNo, cblkNo + 1]
          clst = map (fst . fromJust) $ filter isJust $ map (getChunk chl)
               $ [(bx + x, 0, bz + z) | x <-[-16,0,16], z <- [-16,0,16]]
          slst = map (\ c' -> fromJust $ lookup c' f') clst

      f <- mapM (\ (_,b) -> readIORef b)
            $ concat $ map (\ s -> map (s !!) bplst) slst 
      let res = filter (\ (_,r) -> isJust r)
           $ map (tomasChk pos rot) $ map (\ (a,_,b) -> (a,b)) $ concat f
          format =(\ (p,(_,s)) -> (p,s))
                     $ minimumBy (comparing (\ (_,(t,_)) -> t))  $ 
                          map (\ (p,a) -> (p,fromJust a)) res
      --Dbg.traceIO (show ({-(ux,uy,uz),cblkNo,cNo,clst,bplst-}res)) 
      return $ if null res
             then Nothing
             else Just format
    Nothing -> return Nothing
  where
    chlist = chunkList wld
    (ux,uy,uz) = userPos usr
    rot = (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)) $ userRot usr
    pos = (\ (a,b,c) -> (realToFrac a, realToFrac b + 1.5, realToFrac c)) $ userPos usr

calcPointer :: (Num a,Floating a) => (a,a,a) -> (a,a,a) -> a
            -> (a,a,a)
calcPointer (x,y,z) (rx,ry,_) r = ( x + r * ( -sin (d2r ry)
                                             * cos (d2r rx))
                                   , y + r * sin (d2r rx)
                                   , z + r * cos (d2r (ry + 180)) * cos (d2r rx))
  where
    d2r d = pi*d/180.0



tomasChk :: Pos' -> Rot' -> (WorldIndex,[Surface])
         -> (WorldIndex,Maybe (Double,Surface)) 
tomasChk pos@(px,py,pz) rot (ep,fs) = (ep, choise faceList)
  where
    dir =(\ (x,y,z) -> (x - px, y - py, z - pz)) $ calcPointer pos rot 1
    choise lst = {-Dbg.trace (show lst) $-}
               if null lst 
                 then Nothing
                 else Just $ (\ (Just a,s) -> (a,s)) (minimum $ map swap lst)
    faceList = filter (\ (_,v) -> isJust v) $ zip fs $
                    map (chk . genNodeList (i2d ep)) fs
    chk ftri = {- Dbg.trace (show l) $ -} if null l then Nothing else minimum l
      where l = {- Dbg.trace (unwords [show pos, show ftri]) $ -}
                filter isJust $ map (\ (n1,n2,n3) -> tomasMollerRaw pos dir n1 n2 n3) ftri
    genNodeList pos' face = genTri $ map (pos' .+. )
      $ map (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)) $ case face of
      STop    -> [p5,p4,p7,p6] -- Top
      SBottom -> [p2,p3,p0,p1] -- Bottom
      SFront  -> [p6,p7,p1,p0] -- Front
      SBack   -> [p4,p5,p3,p2] -- Back
      SRight  -> [p4,p2,p1,p7] -- Right
      SLeft   -> [p3,p5,p6,p0] -- Left 
    genTri [a1,a2,a3,a4] = [(a1,a2,a3),(a3,a4,a1)]
    (p0,p1,p2,p3,p4,p5,p6,p7) = blockNodeVertex
    i2d (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)


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

renderCurFace :: Maybe (WorldIndex,Surface) -> IO ()
renderCurFace objPos = 
  case objPos of 
    Just ((px,py,pz),s) -> preservingMatrix $ do -- 選択された面を強調
      texture Texture2D $= Disabled
      lineWidth         $= 1.2
      color $ Color3 0.1 0.1 (0.1::GLfloat)
      translate $ Vector3 (fromIntegral px)
                          (fromIntegral py)
                          (fromIntegral pz :: GLfloat)
      rotCursol s -- SBack -- SLeft -- SRight -- SFront -- SBottom -- STop
      gen3dCursol
    Nothing -> return ()
  where
  rotCursol face = case face of
    STop    -> return ()
    SBottom -> rotate 180   $ Vector3 1.0 0.0 (0::GLfloat)
    SFront  -> rotate (-90) $ Vector3 1.0 0.0 (0::GLfloat)
    SBack   -> rotate 90    $ Vector3 1.0 0.0 (0::GLfloat)
    SRight  -> rotate (-90) $ Vector3 0.0 0.0 (1::GLfloat)
    SLeft   -> rotate 90    $ Vector3 0.0 0.0 (1::GLfloat)


drawTitle :: (Int,Int) -> GuiResource -> GLfloat -> IO ()
drawTitle (w,h) res rw = do
  preservingMatrix $ do
    setPerspective V3DTitleMode w h
    scale 10.0 10.0 (10.0::GLfloat)
    rotate 10 $ Vector3 1.0 0.0 (0::GLfloat)
    rotate rw $ Vector3 0.0 1.0 (0::GLfloat)
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
    putTextLine font' (Just (1,1,1)) (Just 30) (780,768 - 614 + 20) "Quit game" 

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


