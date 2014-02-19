module Main (main) where

import Data.IORef
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( unless,when,forM,forM_ {-void,filterM-} )
import Data.Maybe ( fromJust,isJust ) --,catMaybes )
import System.Directory ( getHomeDirectory )
import Data.Tuple
import Data.List
import Data.Ord
--import Control.Concurrent
import Hinecraft.Render.View
--import Hinecraft.Render.UI
--import Hinecraft.Render.Types
import Hinecraft.Util
import Hinecraft.Types
import Hinecraft.Data
import Hinecraft.GUI.GLFWWindow 

main :: IO ()
main = bracket initHinecraft exitHinecraft runHinecraft

data RunMode = TitleMode | PlayMode | InventoryMode
  deriving (Eq,Show)

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
  runMode' <- newIORef TitleMode
  mainLoop rotwld useStat runMode' (wld,sfl,dsps) 
  where
    plt = [StoneBlockID, DirtBlockID, GlassBlockID, WoodBlockID
          ,GrassBlockID, GlowBlockID, PlankBlockID, StonebrickBlockID
          ,PlankHalfBlockID]
    mainLoop r' u' m' (w',f',d') = do
      pollGLFW
      --threadDelay 1000
      dt <- getDeltTime glfwHdl
      exitflg' <- getExitReqGLFW glfwHdl
      (exitkey',pos) <- mainProcess resouce r' w' u' m' f' d' plt dt
      drawView (glfwHdl,guiRes,wldRes) r' u' m' d' pos plt
      unless (exitflg' || exitkey') $ mainLoop r' u' m' (w',f',d')

mainProcess :: (GLFWHandle, GuiResource, WorldResource) -> IORef Double 
            -> WorldData -> IORef UserStatus -> IORef RunMode
            -> SurfaceList -> WorldDispList -> [BlockID]
            -> Double
            -> IO (Bool,Maybe (WorldIndex,Surface))
mainProcess (glfwHdl, guiRes, wldRes) rotW wld usrStat runMode'
            sufList dsps plt dt = do
  -- Common User input
  mous <- getButtonClick glfwHdl
  syskey <- getSystemKeyOpe glfwHdl
  -- 
  mode' <- readIORef runMode'
  (newMode, extflg,pos) <- case mode' of
    TitleMode -> do
      modifyIORef rotW (\ r -> let nr = (r + (1.0 * dt))
                               in if nr > 360 then nr - 360 else nr ) 
      (md,exflg') <- guiProcess guiRes mous -- ### 2D ###
      return (md,exflg',Nothing)
    PlayMode -> do
      u' <- readIORef usrStat
      newStat <- playProcess glfwHdl wld u' dt
      writeIORef usrStat newStat
      pos <- calcCursorPos wld sufList u'
      mouseOpe wld newStat mous pos plt
        (updateDisplist wldRes wld dsps sufList)
      let md = case syskey of
             (True,_) -> TitleMode
             (False,True) -> InventoryMode
             _ ->  PlayMode
      return (md,False,pos)
    InventoryMode -> do
      let md = case syskey of
             (True,_) -> TitleMode
             (False,True) -> PlayMode
             _ -> InventoryMode
      return (md,False,Nothing)
  when (mode' /= newMode) $ do
    writeIORef runMode' newMode
    setUIMode glfwHdl $ if newMode == PlayMode
      then Mode3D
      else Mode2D
                                              
  return (extflg,pos)

updateDisplist :: WorldResource -> WorldData -> WorldDispList
               -> SurfaceList -> WorldIndex -> IO ()
updateDisplist wldRes wld dsps sufList pos = do 
  clst <- calcReGenArea wld pos 
  dsps' <- readIORef dsps
  forM_ clst $ \ (cNo',bNo') -> case lookup cNo' dsps' of
      Just d -> do
        let (_,d') = d !! bNo'
        sfs <- getSurface wld (cNo',bNo')
        setSurfaceList sufList (cNo',bNo') sfs  
        genSufDispList wldRes sfs (Just d')
        return ()
      Nothing -> return ()

mouseOpe :: WorldData -> UserStatus -> (Double,Double,Bool,Bool,Bool)
         -> Maybe (WorldIndex,Surface) -> [BlockID]
         -> (WorldIndex -> IO ())
         -> IO ()
mouseOpe _ _ _ Nothing _ _ = return ()
mouseOpe _ _ (_,_,False,False,_) _ _ _ = return ()
mouseOpe wld ustat (_,_,lb,rb,_) (Just ((cx,cy,cz),fpos)) plt callback
  | rb == True = do
    setBlockID wld setPos (plt !! idx)
    callback setPos
  | lb == True = do
    setBlockID wld (cx,cy,cz) AirBlockID 
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

playProcess :: GLFWHandle -> WorldData -> UserStatus -> Double
            -> IO UserStatus
playProcess glfwHdl wld usrStat dt = do
  vm <- getCursorMotion glfwHdl
  sm <- getScrollMotion glfwHdl 
  mm <- getMoveKeyOpe glfwHdl
  calcPlayerMotion wld usrStat vm mm sm dt

calcPlayerMotion :: WorldData -> UserStatus
                 -> (Int,Int) -> (Int,Int,Int,Int,Int) -> (Int,Int)
                 -> Double
                 -> IO UserStatus
calcPlayerMotion wld usrStat (mx,my) (f,b,l,r,jmp) (_,sy) dt = do
  w <- getBlockID wld (round' ox,(round' oy) - 1,round' oz) 
         >>= (\ bid -> return $ if bid == AirBlockID
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
          (tx,ty,tz) = (ox' + dx', oy' + dy', oz' + dz')
      bidT <- getBlockID wld ( round' $ ox' + dx' * 1.5 
                             , round' $ ty + 1
                             , round' $ oz' + dz' * 1.5)
      bidB <- getBlockID wld ( round' $ ox' + dx' * 1.5
                             , round' $ ty
                             , round' $ oz' + dz' * 1.5)

      if bidT == AirBlockID && bidB == AirBlockID
        then if dl < 1 
          then return (tx,ty,tz)
          else do
            w' <- fmap (\ b' -> if AirBlockID == b' then w else 0)
              $ getBlockID wld (round' tx, round' ty - 1, round' tz)
            chkMove w' (tx,ty,tz) (dx - dx', dy - dy', dz - dz')
        else do
          w' <- fmap (\ b' -> if AirBlockID == b' then w else 0)
              $ getBlockID wld (round' ox, round' oy - 1, round' oz)
          if w' == 0
            then return (ox',oy',oz')
            else chkMove w' (ox',ty,oz') (0.0, dy - dy' , 0.0)
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
         -> IORef Double -> IORef UserStatus -> IORef RunMode
         -> WorldDispList -> Maybe (WorldIndex,Surface)
         -> [BlockID]
         -> IO ()
drawView (glfwHdl, guiRes, wldRes) dw usrStat runMode'
         worldDispList pos plt = do
  mode' <- readIORef runMode'
  winSize <- getWindowSize glfwHdl
  updateDisplay $
    if mode' == TitleMode
      then do -- 2D
        readIORef dw >>= (drawTitle winSize guiRes) . realToFrac
      else do 
        u' <- readIORef usrStat
        let invSw = mode' == InventoryMode
        drawPlay winSize guiRes wldRes u' worldDispList pos plt invSw
  swapBuff glfwHdl
--

genWorldDispList :: WorldResource -> SurfaceList -> IO WorldDispList
genWorldDispList wldRes suf = do
  suf' <- readIORef suf
  forM suf' (\ (chNo,blks) -> do
    sb <- forM blks (\ (bNo,sfs) -> do
      sfs' <- readIORef sfs
      d <- genSufDispList wldRes sfs' Nothing
      return (bNo,d)) 
    return (chNo,sb)) 
      >>= newIORef

-- 

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
      . (\ ((a,b,c),_) -> (realToFrac a, realToFrac b, realToFrac c)))
      $ getVertexList Cube face
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

