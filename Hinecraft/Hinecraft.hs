{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.IORef
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( unless,when,forM,forM,foldM {-void,filterM-} )
--import Data.Maybe ( fromJust,isJust ) --,catMaybes )
import System.Directory ( getHomeDirectory )
--import Control.Concurrent

import Hinecraft.Render.View
import Hinecraft.Render.Types
import Hinecraft.Model
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
  let tmstat = TitleModeState (0::Double) False False False
      plstat = PlayModeState
        { usrStat = UserStatus
                     { userPos = (0.0,16 * 4 + 1,0.0)
                     , userRot = (0.0,0.0,0.0) 
                     , palletIndex = 0
                     , userVel = (0.0,0.0,0.0)
                     }
        , drgdrpMd = Nothing
        , drgSta = Nothing
        , curPos = Nothing
        , pallet = replicate 9 airBlockID
        }
      wld = genWorldData 
  sfl <- genSurfaceList wld
  dsps <- genWorldDispList wldRes sfl 
  _ <- getDeltTime glfwHdl
  mainLoop tmstat plstat TitleMode (wld,sfl,dsps) 
  where
    mainLoop tmstat' plstat' runMode (w',f',d') = do
      pollGLFW
      --threadDelay 10000
      dt <- getDeltTime glfwHdl
      exitflg' <- getExitReqGLFW glfwHdl
      (ntmstat',nplstat',runMode',f'',nw') <- mainProcess
                   resouce tmstat' plstat' w' runMode f' d' dt
      drawView (glfwHdl,guiRes,wldRes) ntmstat' nplstat' runMode' d' 
      unless (exitflg' || (isQuit ntmstat'))
        $ mainLoop ntmstat' nplstat' runMode' (nw',f'',d')

mainProcess :: (GLFWHandle, GuiResource, WorldResource) -> TitleModeState 
            -> PlayModeState ->  WorldData ->RunMode
            -> SurfaceList -> WorldDispList 
            -> Double
            -> IO ( TitleModeState, PlayModeState, RunMode
                  , SurfaceList, WorldData)
mainProcess (glfwHdl, guiRes, wldRes) tmstat plstat wld  runMode
            sufList dsps dt = do
  -- Common User input
  mous <- getButtonClick glfwHdl
  syskey <- getSystemKeyOpe glfwHdl
  -- 
  (newMode,newPmstat,newTmstat,newSufList',nw') <- case runMode of
    TitleMode -> do
      let !((md,cEt),(exflg',eEt)) = guiProcess guiRes mous -- ### 2D ###
          !r' = (rotW tmstat) + (1.0 * dt)
          !ntstat = TitleModeState
            { rotW = if r' > 360 then r' - 360 else r'
            , isModeChgBtnEntr = cEt
            , isExitBtnEntr = eEt
            , isQuit = exflg'
            }
      return $! (md,plstat,ntstat,sufList,wld)
    PlayMode -> do
      vm <- getCursorMotion glfwHdl
      sm <- getScrollMotion glfwHdl 
      mm <- getMoveKeyOpe glfwHdl
      let !u' = usrStat plstat
          !plt = pallet plstat
          !pos = calcCursorPos wld sufList u'
          !newStat = calcPlayerMotion wld u' vm mm sm dt
      (w',newSufList) <- case setBlock newStat mous pos plt of
        Just (pos',bid) -> do
          let newWld = setBlockID wld pos' bid
          s' <- updateDisplist wldRes newWld dsps sufList pos'
          return (newWld,s')
        Nothing -> return (wld,sufList)
      let !md = case syskey of
             (True,_) -> TitleMode
             (False,True) -> InventoryMode
             _ ->  PlayMode
          !nplstat' = PlayModeState
                       { usrStat = newStat
                       , drgdrpMd = Nothing
                       , drgSta = Nothing
                       , curPos = pos
                       , pallet = plt
                       }
      return $! (md,nplstat',tmstat, newSufList,w')
    InventoryMode -> do
      winSize <- getWindowSize glfwHdl
      let !md = case syskey of
             (True,_) -> TitleMode
             (False,True) -> PlayMode
             _ -> InventoryMode
          !drgMd = drgdrpMd plstat
          !plt = pallet plstat
          !(drgSta',drgMd',nPlt') = invMouseOpe winSize mous drgMd plt
          !nplstat' = PlayModeState
                       { usrStat = usrStat plstat
                       , drgdrpMd = drgMd'
                       , drgSta = drgSta'
                       , curPos = Nothing
                       , pallet = nPlt'
                       }
      return $! (md,nplstat',tmstat, sufList,wld)
  when (runMode /= newMode) $ do
    if (newMode == InventoryMode)
      then setMouseBtnMode glfwHdl StateMode
      else setMouseBtnMode glfwHdl REdgeMode 
    setUIMode glfwHdl $ if newMode == PlayMode
      then Mode3D
      else Mode2D
  return $! (newTmstat,newPmstat,newMode,newSufList',nw')


invMouseOpe :: (Int,Int) -> (Double,Double,Bool,Bool,Bool) -> DragDropMode
            -> [BlockIDNum] -> (DragDropState, DragDropMode, [BlockIDNum])
invMouseOpe (w,h) (x,y, btn, _, _) drgMd plt = 
    case drgMd of
      Just bid -> if btn
                  then (Just ((x',y'),bid), drgMd, plt)
                  else  -- Drop
                    ( Nothing , Nothing
                    , case getInventoryIndex (w,h) (x,y) of
                               Just (i,5) -> newPallet i bid
                               Nothing -> plt
                               _ -> plt )
      Nothing -> if btn 
                 then  -- Drag
                   ( case bid' of
                       Just i -> Just ((x',y'),i)
                       Nothing -> Nothing
                   , bid'
                   , plt)
                 else (Nothing,drgMd, plt)
  where
    (x',y') = (realToFrac x, realToFrac y)
    newPallet i b = (take i plt) ++ (b : (drop (i + 1) plt))
    bid' = case getInventoryIndex (w,h) (x,y) of
             Just (i,j) -> let !idx = j * 9 + i
                           in if length blockCatalog > idx
                                then Just $ blockCatalog !! idx
                                else Nothing
             Nothing -> Nothing

getInventoryIndex :: (Int,Int) -> (Double,Double) -> Maybe (Int,Int)
getInventoryIndex (w,h) (x,y) = case (flt (> x) xbordLst , yIdx ) of
                                  (Nothing,_) -> Nothing
                                  (_,Nothing) -> Nothing
                                  (Just i,Just j) -> Just (i,j)
  where
    (w',h') = (fromIntegral w, fromIntegral h)
    (dotW,dotH) = rectDotSize inventoryParam
    (invOx,invOy) = ( (w' - rate * fromIntegral dotW) * 0.5
                    , (h' - rate * fromIntegral dotH) * 0.5)
    (icloX,icloY) = iconListOrg inventoryParam
    (_,ploY) = palletOrg inventoryParam
    rate = projectionRate inventoryParam
    itvl = iconListItvl inventoryParam
    xbordLst = [ invOx + (icloX + (itvl * i)) * rate | i <- [0 .. 9]]
    ybordLst = [ invOy + (icloY - (itvl * i)) * rate | i <- [-1 .. 4]]
    flt f lst =  case break f lst of
      ([],_) -> Nothing
      (_,[]) -> Nothing
      (a,_) -> Just $ (length a) - 1
    chkPltY y' = (invOy + (ploY * rate)) < y'
                &&  y' < (invOy + (ploY + itvl - 2) * rate)
    yIdx = case flt (< y) ybordLst of
             Just a -> Just a
             Nothing -> if chkPltY y then Just 5 else Nothing

updateDisplist :: WorldResource -> WorldData -> WorldDispList
               -> SurfaceList -> WorldIndex -> IO SurfaceList
updateDisplist wldRes wld dsps sufList pos = do 
  let !clst = calcReGenArea wld pos 
  dsps' <- readIORef dsps
  foldM (\ sflst (cNo',bNo') -> case lookup cNo' dsps' of
      Just d -> do
        let !(_,d') = d !! bNo'
            !sfs = getSurface wld (cNo',bNo')
            !newSfs = setSurfaceList sflst (cNo',bNo') sfs  
        genSufDispList wldRes sfs (Just d')
        return newSfs
      Nothing -> return sflst) sufList clst 

setBlock :: UserStatus -> (Double,Double,Bool,Bool,Bool)
         -> Maybe (WorldIndex,Surface) -> [BlockIDNum]
         -> Maybe (WorldIndex,BlockIDNum)
setBlock _ _ Nothing _ = Nothing
setBlock _ (_,_,False,False,_) _ _ = Nothing
setBlock ustat (_,_,lb,rb,_) (Just ((cx,cy,cz),fpos)) plt 
  | rb == True = Just (setPos,(plt !! idx))
  | lb == True = Just ((cx,cy,cz),airBlockID)
  | otherwise = Nothing
  where
    idx = palletIndex ustat 
    setPos = case fpos of
      STop    -> (cx,cy + 1,cz)
      SBottom -> (cx,cy - 1,cz)
      SRight  -> (cx + 1,cy,cz)
      SLeft   -> (cx - 1,cy,cz)
      SFront  -> (cx,cy,cz - 1)
      SBack   -> (cx,cy,cz + 1)


calcPlayerMotion :: WorldData -> UserStatus
                 -> (Double,Double) -> (Int,Int,Int,Int,Int) -> (Int,Int)
                 -> Double
                 -> UserStatus
calcPlayerMotion wld usrStat' (mx,my) (f,b,l,r,jmp) (_,sy) dt =
  UserStatus
    { userPos = ( nx
                , ny 
                , nz)
    , userRot = (realToFrac nrx, realToFrac nry, realToFrac nrz) 
    , palletIndex = if idx < 0 then 0 else if idx > 8 then 8 else idx
    , userVel = (0.0,w,0.0)
    }
  where
    (ox,oy,oz) = userPos usrStat'
    (orx,ory,orz) = userRot usrStat'
    (_,wo,_) = userVel usrStat'
    !(nrx,nry,nrz) = playerView dt ( realToFrac orx
                                   , realToFrac ory
                                   , realToFrac orz)
                        ( my * 0.5, mx * 0.5, 0)
    !(dx,dy,dz) = playerMotion dt nry ( fromIntegral (b - f)*4
                                      , fromIntegral (r - l)*4
                                      , w)                
    !idx = (palletIndex usrStat') - sy 
    !w = if jmp == 2
          then 5
          else wo - (9.8 * dt)
    !(nx,ny,nz) = movePlayer wld (ox,oy,oz) (realToFrac dx
                                            ,realToFrac dy
                                            ,realToFrac dz)

movePlayer :: WorldData -> Pos' -> Pos' -> Pos'
movePlayer wld (ox',oy',oz') (dx,dy,dz) =
  if bid == airBlockID || chkHalf bid || y' + 0.6 > y''
    then if dl < 1
           then (tx,y',tz)
           else movePlayer wld (tx,y',tz) (dx - dx', dy - dy', dz - dz')
    else (ox',y',oz')
  where 
    !dl = sqrt (dx * dx + dz * dz + dy * dy)
    !(dx',dy',dz') = if dl < 1
          then (dx,dy,dz)
          else (dx / dl, dy / dl, dz / dl)
    !(tx,_,tz) = (ox' + dx', oy' + dy', oz' + dz')
    !y' = calYpos wld (ox',oy',oz') dy'
    !y'' = calYpos wld (tx,y'+1,tz) (-1)
    Just bid = getBlockID wld (round' tx, round' oy' ,round' tz)

calYpos :: WorldData -> (Double,Double,Double) -> Double -> Double
calYpos _   (_,y,_) 0 = y
calYpos wld (x,y,z) dy = if (abs ndy) < 0.001
    then {-trace (show (y',y0,c,ly,dy,ndy)) -} y'
    else {- trace (show (y',ndy)) $ -} calYpos wld (x,y',z) ndy
  where
    Just t = getBlockID wld (round' x, round' y, round' z) 
    !y0 = fromIntegral $ (round' y :: Int)
    !ly = y - y0
    !t' = if t == airBlockID
            then 0
            else if chkHalf t == True then 2 else 1
    !(ny,ndy,c) = calcPos' t' (ly,dy)
    !y' =  y0 + fromIntegral c + (if c > 0
                                     then ny + 0.001
                                     else ny - 0.001)
-- | 
-- -0.5 < ly <= 0.5
calcPos' :: Int -> (Double,Double) -> (Double,Double,Int)
calcPos' t (ly,dy)
  | t == 0 = case (ny >= 0.5, ny < -0.5) of
               (True , _ ) -> (-0.5, ny - 0.5, 1) 
               (_ , True ) -> ( 0.5, ny + 0.5,-1)
               _ -> if dy == 0 then (ly,0,0) else (ny,0,0)
  | t == 1 = (if ly > 0 then -0.5 else 0.5 ,0,if dy > 0 then -1 else 1) 
  | t == 2 = case (ny >= 0.5,  ny < 0) of
               (True , _ ) -> (-0.5, ny - 0.5,1) 
               (_ , True ) -> (0, 0, 0)
               _ -> (ny,0,0) 
  | otherwise = (ly,dy,0) 
  where
    !ny = ly + dy

chkHalf :: BlockIDNum -> Bool
chkHalf bid = case shape $ getBlockInfo bid of
                Half _ -> True
                _ -> False

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
           -> ((RunMode,Bool),(Bool,Bool))
guiProcess res (x,y,btn1,_,_) = (chkModeChg,chkExit)
  where
    plybtnPosOrgn = widgetPlayBtnPos res
    plybtnSize = widgetPlayBtnSiz res
    extbtnPosOrgn = widgetExitBtnPos res
    extbtnSize = widgetExitBtnSiz res
    f2d (a,b) = (realToFrac a, realToFrac b) -- GLfloat to Double
    chkEntrBtn (xo,yo) (w,h) =
       xo < x && x < (xo + w) && yo < y && y < (yo + h) 
    isPlyBtnEntr = chkEntrBtn (f2d plybtnPosOrgn) (f2d plybtnSize)
    chkModeChg = ( if isPlyBtnEntr && btn1 then PlayMode else TitleMode 
                 , isPlyBtnEntr)
    isExtBtnEntr = chkEntrBtn (f2d extbtnPosOrgn) (f2d extbtnSize)
    chkExit = ( isExtBtnEntr && btn1 , isExtBtnEntr) 

drawView :: ( GLFWHandle, GuiResource, WorldResource)
         -> TitleModeState -> PlayModeState -> RunMode
         -> WorldDispList 
         -> IO ()
drawView (glfwHdl, guiRes, wldRes) tmstat plstat runMode'
         worldDispList = do
  winSize <- getWindowSize glfwHdl
  updateDisplay $
    if runMode' == TitleMode
      then -- 2D
        drawTitle winSize guiRes tmstat
      else  
        drawPlay winSize guiRes wldRes usrStat' worldDispList pos plt
                 (runMode' == InventoryMode) drgSta'
  swapBuff glfwHdl
  where
    usrStat' = usrStat plstat
    pos = curPos plstat
    drgSta' = drgSta plstat
    plt = pallet plstat
--

genWorldDispList :: WorldResource -> SurfaceList -> IO WorldDispList
genWorldDispList wldRes suf = do
  forM suf (\ (chNo,blks) -> do
    sb <- forM blks (\ (bNo,sfs) -> do
      d <- genSufDispList wldRes sfs Nothing
      return (bNo,d)) 
    return (chNo,sb)) 
      >>= newIORef

-- 


