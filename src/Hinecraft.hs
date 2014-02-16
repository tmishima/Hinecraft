import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Data.IORef
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( unless,when, {-void,filterM-} )
import Data.Maybe ( fromJust,isJust ) --,catMaybes )
import System.Directory ( getHomeDirectory )
import Data.Tuple
import Data.List
import Data.Ord
import Control.Concurrent
import View
import Util
import Model
import Types
import Data

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
  useStat <- newIORef UserStatus
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
      threadDelay 1000
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
  mapM_ (\ (cNo',bNo') -> case lookup cNo' dsps' of
      Just d -> do
        let (_,d') = d !! bNo'
        sfs <- getSurface wld (cNo',bNo')
        setSurfaceList sufList (cNo',bNo') sfs  
        genSufDispList wldRes sfs (Just d')
        return ()
      Nothing -> return ()
    ) clst

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
            else  if jmp == 2  then 4.0 else if wo > 0 then wo else 0.0)
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
    chkMove w (ox',oy',oz') (dx,dy,dz) = do
      let (dx',dy',dz') = if dl < 1
              then (dx,dy,dz)
              else (dx / dl, dy / dl, dz / dl)
          (tx,ty,tz) = if w == 0
              then (ox' + dx', oy', oz' + dz')
              else (ox' + dx', oy' + dy', oz' + dz')
      bidT <- getBlockID wld (round' tx, round' ty + 1, round' tz)
      bidB <- getBlockID wld (round' tx, round' ty, round' tz)

      w' <- getBlockID wld (round' tx, round' ty - 1, round' tz)
             >>= (\ b' -> return $ if voidBlockID == b' then w else 0)

      if bidT == voidBlockID && bidB == voidBlockID
        then if dl < 1
          then return (tx,ty,tz)
          else chkMove w' (tx,ty,tz) (dx - dx', dy - dy', dz - dz')
        else if w' == 0
          then return (ox',oy',oz')
          else chkMove w' (tx,ty,tz) (0.0, dy - dy' , 0.0)
      where 
        dl = sqrt (dx * dx + dz * dz + dy * dy)

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


--
-- 
type WorldDispList = IORef [(ChunkNo, [(BlockNo,DisplayList)])]

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
genSufDispList wldRes bsf dsp = case dsp of
    Nothing -> defineNewList Compile gen'
    Just d -> defineList d Compile gen' >> return d
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
                                  then replicate 6 (0,0)
                                  else texIdx
      mapM_ (\ (f,l) -> case f of
         STop -> genSuf (0,-1,0) (setColor 0 l)
           $ zip (calcUV tt') (getVertexList STop)
         SBottom -> genSuf (0,-1,0) (setColor 1 l)
           $ zip (calcUV tb') (getVertexList SBottom)
         SRight -> genSuf (1,0,0) (setColor 2 l) 
           $ zip (calcUV tf') (getVertexList SRight)
         SLeft -> genSuf (-1,0,0) (setColor 3 l) 
           $ zip (calcUV tba') (getVertexList SLeft)
         SFront -> genSuf (0,0,-1) (setColor 4 l)
           $ zip (calcUV tr') (getVertexList SFront)
         SBack -> genSuf (0,0,1) (setColor 5 l)
           $ zip (calcUV tl') (getVertexList SBack)
         ) fs
      where
        setColor i l = (d2fv3 $ (bcolor $ getBlockInfo bid) !! i)
                       ..*. ((fromIntegral l) / 16.0) 
        genSuf :: (GLfloat,GLfloat,GLfloat)
               -> (GLfloat,GLfloat,GLfloat)
               -> [((GLfloat,GLfloat)
                  ,(GLfloat,GLfloat,GLfloat))]
               -> IO ()
        genSuf (nx,ny,nz) (r,g,b) ndlst = do
          normal $ Normal3 nx ny nz
          color $ Color3 r g b
          mapM_ (\ ((u,v),(x', y', z')) -> do
                   glTexCoord2f u v
                   vertex (Vertex3 (x' + fromIntegral x)
                                   (y' + fromIntegral y)
                                   (z' + fromIntegral z))) ndlst
        calcUV (i',j') = [ ( i * (1/16), j * (1/16)) 
                         , ( (i + 1) * (1/16), j * (1/16))
                         , ( (i + 1) * (1/16), (j + 1) * (1/16))
                         , ( i * (1/16), (j + 1) * (1/16))
                         ]
          where i = fromIntegral i' ; j = fromIntegral j'

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
    translate $ Vector3 (-ux) (-uy - 1) (-uz) 

    -- Cursol 選択された面を強調
    --Dbg.traceIO $ show (pos, uy)
    renderCurFace  pos

    color $ Color3 0.0 1.0 (0.0::GLfloat)
    readIORef worldDispList
      >>= mapM_ (\ (_,b) -> mapM_ (\ (_,d) -> callList d) b)  

  -- HUD
  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
    depthMask $= Disabled

    -- Pallet
    let (icSz, bdSz, prSz) = (64, 1, 2)
        (pltW,pltH) = ((icSz + prSz) * 9 + bdSz * 2, icSz + bdSz * 4)
        (pltOx,pltOy) = ((1366 - pltW) / 2, 2)
        curXpos = pltOx + bdSz
                + (fromIntegral pIndex) * (icSz + prSz / 2)
    drawBackPlane (pltOx,pltOy) (pltW,pltH) (Just widTex')
                  (0,0) (183/256,22/256) (1.0,1.0,1.0,1.0)
    drawBackPlane (curXpos,2) (icSz + prSz, icSz + 1) (Just widTex')
                  (0,24/256) (22/256,22/256) (1.0,1.0,1.0,1.0)

    preservingMatrix $ do
      mapM_ (\ ib -> drawIcon wldRes
        (pltOx + (icSz / 2) + bdSz + prSz + (icSz + prSz/2) * fromIntegral ib,12) ib)
        [1,2,3,4,6,7,8] 

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
    drawBackGroundBox' = preservingMatrix $ do
      color $ Color4 (180/255) (226/255) (255/255) (1.0::GLfloat)
      texture Texture2D $= Disabled
      renderPrimitive Quads $ do
        genSuf (0,0,-1) $ getVertexList SFront  -- Front
        genSuf (-1,0,0) $ getVertexList SRight  -- Right
        genSuf (1,0,0)  $ getVertexList SLeft   -- Left
        genSuf (0,-1,0) $ getVertexList STop    -- Top
        genSuf (0,1,0)  $ getVertexList SBottom -- Bottom
        genSuf (0,0,1)  $ getVertexList SBack   -- Back 
    genSuf :: (GLfloat,GLfloat,GLfloat)
           -> [(GLfloat,GLfloat,GLfloat)]
           -> IO ()
    genSuf (nx,ny,nz) ndlst = do
      normal $ Normal3 nx ny nz
      mapM_ (\ (x', y', z') ->
        vertex (Vertex3 x' y' z')) ndlst

drawIcon :: WorldResource -> (GLfloat,GLfloat) -> BlockID -> IO ()
drawIcon wldRes (ox,oy) bID | null texIdx = return ()
                            | otherwise = preservingMatrix $ do
  texture Texture2D $= Enabled 
  glBindTexture gl_TEXTURE_2D tex

  let [tt,_,_,tl,tf,_] = texIdx
  renderPrimitive Quads $ do
    -- left 
    color $ Color3 0.5 0.5 (0.5::GLfloat)
    mapM_ drawf $ zip (calcUV tl)
      [ (ox, oy)
      , (ox, oy + icSz)
      , (ox + icSz, oy + icSz + icSz * (sin.d2r) rt)
      , (ox + icSz, oy + icSz * (sin.d2r) rt)
      ]
    -- front 
    color $ Color3 0.5 0.5 (0.5::GLfloat)
    mapM_ drawf $ zip (calcUV tf)
      [ (ox, oy)
      , (ox, oy + icSz)
      , (ox - icSz, oy + icSz + icSz * (sin.d2r) rt)
      , (ox - icSz, oy + icSz * (sin.d2r) rt)
      ]
    -- top 
    color $ Color3 0.8 0.8 (0.8::GLfloat)
    mapM_ drawf $ zip (calcUV tt)
      [ (ox, oy + icSz)
      , (ox + icSz, oy + icSz + icSz * (sin.d2r) rt)
      , (ox, oy + icSz + icSz * (sin.d2r) 2 * rt)
      , (ox - icSz, oy + icSz + icSz * (sin.d2r) rt)
      ]
  where
    icSz = 20.0
    rt = 30.0
    tex = blockTexture wldRes
    d2r d = pi*d/180.0
    texIdx = textureIndex $ getBlockInfo bID
    calcUV (i',j') = [ ( i * (1/16), j * (1/16)) 
                     , ( (i + 1) * (1/16), j * (1/16))
                     , ( (i + 1) * (1/16), (j + 1) * (1/16))
                     , ( i * (1/16), (j + 1) * (1/16))
                     ]
      where i = fromIntegral i' ; j = fromIntegral j'
    drawf ((u,v),(x,y)) = do
      glTexCoord2f u v
      vertex $ Vertex2 x y


-- 

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
    putTextLine font' (Just (1,1,1)) (Just 30)
      (590,768 - 410 + 20) "Single Player" 
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

tomasChk :: Pos' -> Rot' -> (WorldIndex,[(Surface,Bright)])
         -> (WorldIndex,Maybe (Double,Surface)) 
tomasChk pos@(px,py,pz) rot (ep,fs) = (ep, choise faceList)
  where
    fs' = map fst fs
    dir =(\ (x,y,z) -> (x - px, y - py, z - pz))
          $ calcPointer pos rot 1
    choise lst = if null lst 
                   then Nothing
                   else Just $ (\ (Just a,s) -> (a,s))
                                     (minimum $ map swap lst)
    faceList = filter (\ (_,v) -> isJust v) $ zip fs' $
                    map (chk . genNodeList (i2d ep)) fs'
    chk ftri = if null l then Nothing else minimum l
      where l = filter isJust $ map (\ (n1,n2,n3) ->
                           tomasMollerRaw pos dir n1 n2 n3) ftri
    genNodeList pos' face = genTri $ map ((pos' .+. )
      . (\ (a,b,c) -> (realToFrac a, realToFrac b, realToFrac c)))
      $ getVertexList face
    genTri [a1,a2,a3,a4] = [(a1,a2,a3),(a3,a4,a1)]
    i2d (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)

calcCursorPos :: WorldData -> SurfaceList -> UserStatus
              -> IO (Maybe (WorldIndex,Surface))
calcCursorPos wld sufList usr = do
  chl <- readIORef chlist
  f' <- readIORef sufList
  case getChunk chl ( round' ux , round' uy , round' uz) of
    Just (cNo,c) -> do
      let (bx,bz) = origin c
          cblkNo = div (round' uy) 16
          bplst | cblkNo == 0 = [cblkNo, cblkNo + 1]
                | cblkNo > 6 = [cblkNo - 1, cblkNo]
                | otherwise = [cblkNo - 1, cblkNo, cblkNo + 1]
          clst = map (fst . fromJust) $ filter isJust $ map (getChunk chl)
               $ [(bx + x, 0, bz + z) | x <-[-16,0,16], z <- [-16,0,16]]
          slst = map (\ c' -> fromJust $ lookup c' f') clst

      f <- mapM (\ (_,b) -> readIORef b)
            $ concatMap (\ s -> map (s !!) bplst) slst 
      let res = filter (\ (_, Just (d,_)) -> d > 0)
           $ filter (\ (_,r) -> isJust r)
           $ map (tomasChk pos rot . (\ (a,_,b) -> (a,b))) (concat f)
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
    pos = (\ (a,b,c) -> (realToFrac a, realToFrac b + 1, realToFrac c)) $ userPos usr

calcPointer :: (Num a,Floating a) => (a,a,a) -> (a,a,a) -> a
            -> (a,a,a)
calcPointer (x,y,z) (rx,ry,_) r =
  ( x + r * ( -sin (d2r ry) * cos (d2r rx))
  , y + r * sin (d2r rx)
  , z + r * cos (d2r (ry + 180)) * cos (d2r rx))
  where
    d2r d = pi*d/180.0


test = do
  wld <- genWorldData 
  ret <- mapM (getSunLightEffect wld)
              [ (0,48,0)
              , (0,49,0)
              , (0,50,0)
              , (0,51,0)
              , (0,52,0)
              , (0,22,0)
              ]
  print (show ret)

  ret2 <- mapM (getBlockID wld)
              [ (0,48,0)
              , (0,49,0)
              , (0,50,0)
              , (0,51,0)
              , (0,52,0)
              , (0,22,0)
              ]

  print (show ret2)
  --  setBlockID wld (0,0,0) 0
  --lst <- mapM (getSurface wld) [(0,y) | y <- [0 .. 7]]
  --print lst

  return ()

