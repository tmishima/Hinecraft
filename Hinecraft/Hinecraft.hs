{-# LANGUAGE BangPatterns #-}
{-
   Copyright 2014 Tetsuya.Mishima

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Main (main) where

--import qualified Data.Map as M
import Data.Maybe
import Debug.Trace as Dbg
import Control.Exception ( bracket )
import Control.Monad ( when {-unless,,foldMvoid,filterM-} )
--import Data.Maybe ( fromJust,isJust ) --,catMaybes )
import System.Directory ( getHomeDirectory )
import Control.Concurrent
import System.Environment (getArgs)

import Hinecraft.Render.View
import Hinecraft.Render.Types
import Hinecraft.Render.TitleView
import Hinecraft.Render.WorldView
import Hinecraft.Model
import Hinecraft.Util
import Hinecraft.Types
import Hinecraft.Data
import Hinecraft.GUI.GLFWWindow 
--import Hinecraft.WithSqlite

main :: IO ()
main = bracket initHinecraft exitHinecraft runHinecraft

data RunMode = TitleMode | PlayMode | InventoryMode | InitMode
  deriving (Eq,Show)

type Handls = (GLFWHandle, GuiResource)

initHinecraft :: IO Handls
initHinecraft = do
  Dbg.traceIO "Hinecraft Start"
  home <- getHomeDirectory
  args <- getArgs
  fullsw <- case args of
    ("-f":_) -> return True 
    _ -> return False

  !glfwHdl <- initGLFW winSize fullsw

  initGL
  !guiRes <- loadGuiResource home winSize
  
  return $! (glfwHdl,guiRes)
  where
    winSize = (1366,768)

exitHinecraft :: Handls -> IO ()
exitHinecraft (glfwHdl,_) = do
  exitGLFW glfwHdl
  Dbg.traceIO "Hinecraft End"

runHinecraft :: Handls 
             -> IO ()
runHinecraft resouce@(glfwHdl,guiRes) = do
  home <- getHomeDirectory
  !tvHdl <- initTitleModeView home guiRes
  !wvHdl <- initWorldView home
  !dtHdl <- initData home
  _ <- getDeltTime glfwHdl
  mainLoop (TitleModeState (0::Double) False False False)
           plstat TitleMode (dtHdl,tvHdl,wvHdl) dbgInfo
  exitData dtHdl
  where
    !plstat = PlayModeState
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
    dbgInfo = DebugInfo 0 "debug message"
    showUsrStat ustat = unwords ["pos = ",prtStr x, prtStr y, prtStr z,
                                 "rot = ",prtStr r, prtStr s]
      where
        prtStr v | abs v < 0.1 = "0.0"
                 | otherwise = (take 5) $ show v
        (x,y,z) = userPos $ usrStat ustat
        (r,s,_) = userRot $ usrStat ustat
    mainLoop tmstat' plstat' runMode (dtHdl,tvHdl,wvHdl) dbgInfo' = do
      pollGLFW
      threadDelay 10000
      dt <- getDeltTime glfwHdl
      exitflg' <- getExitReqGLFW glfwHdl
      !(ntmstat',nplstat',runMode',ndtHdl') <- mainProcess
                   resouce tmstat' plstat' dtHdl runMode wvHdl dt
      tglWin <- getScreenModeKeyOpe glfwHdl
      let !newfps = ((fps dbgInfo') * 9.0 / 10.0) + ((1.0 / dt) / 10)
          !dmsg = showUsrStat nplstat'
          !newDbgInfo = DebugInfo newfps dmsg 
      if tglWin
        then toggleFullScreenMode glfwHdl
        else do
          drawView resouce ntmstat' nplstat' runMode' tvHdl wvHdl
                   newDbgInfo         
          swapBuff glfwHdl
      if exitflg' || isQuit ntmstat'
        then return ()
        else mainLoop ntmstat' nplstat' runMode'
                      (ndtHdl',tvHdl,wvHdl) newDbgInfo

mainProcess :: Handls
            -> TitleModeState -> PlayModeState -> DataHdl
            ->RunMode -> WorldViewVHdl -> Double
            -> IO ( TitleModeState, PlayModeState, RunMode
                  , DataHdl)
mainProcess (glfwHdl, guiRes) tmstat plstat dtHdl runMode wvHdl dt = do
  -- Common User input
  !mous <- getButtonClick glfwHdl
  !syskey <- getSystemKeyOpe glfwHdl
  -- 
  (newMode,newPmstat,newTmstat,newHdl') <- case runMode of
    TitleMode -> do
      let !((md,cEt),(exflg',eEt)) = guiProcess guiRes mous -- ### 2D ###
          !r' = (rotW tmstat) + (1.0 * dt)
          !ntstat = TitleModeState
            { rotW = if r' > 360 then r' - 360 else r'
            , isModeChgBtnEntr = cEt
            , isExitBtnEntr = eEt
            , isQuit = exflg'
            }
      return $! (md,plstat,ntstat,dtHdl)
    InitMode -> do
      if (isEmpty dtHdl)
        then return $! (PlayMode,plstat,tmstat,dtHdl)
        else do
          !dtHdl' <- loadData dtHdl
          initWorldVAOList wvHdl $ getAllSurfaceData dtHdl'
          !md <- return PlayMode
          return $! (md,plstat,tmstat,dtHdl')
    PlayMode -> do
      !vm <- getCursorMotion glfwHdl
      !sm <- getScrollMotion glfwHdl 
      !mm <- getMoveKeyOpe glfwHdl
      let !md = case syskey of
             (True,_) -> TitleMode
             (False,True) -> InventoryMode
             _ ->  PlayMode
          !u' = usrStat plstat
          !plt = pallet plstat
          !newStat = calcPlayerMotion dtHdl u' vm mm sm dt
          !pos = calcCursorPos dtHdl u'
      newDtHdl' <- case setBlock u' mous pos plt of
        Just (pos',bid) -> do
          !newDtHdl <- setBlockID dtHdl pos' bid
          let !clst = calcReGenArea pos'
              !sflst = map (\ (ij,bNo') ->
                         ((ij,bNo')
                         , fromJust $ getSurfaceList newDtHdl (ij,bNo')
                         )) clst 
          updateVAOlist wvHdl sflst
          return $! newDtHdl
        Nothing -> return dtHdl
      return $! ( md
                , PlayModeState
                       { usrStat = newStat , drgdrpMd = Nothing
                       , drgSta = Nothing , curPos = pos , pallet = plt
                       }
                , tmstat, newDtHdl')
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
      return $! (md,nplstat',tmstat, dtHdl)
  when (runMode /= newMode) $ do
    if (newMode == InventoryMode)
      then setMouseBtnMode glfwHdl StateMode
      else setMouseBtnMode glfwHdl REdgeMode 
    setUIMode glfwHdl $ if newMode == PlayMode
      then Mode3D
      else Mode2D
  return $! (newTmstat,newPmstat,newMode,newHdl')

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
    !(x',y') = (realToFrac x, realToFrac y)
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
    !(w',h') = (fromIntegral w, fromIntegral h)
    !(dotW,dotH) = rectDotSize inventoryParam
    !(invOx,invOy) = ( (w' - rate * fromIntegral dotW) * 0.5
                     , (h' - rate * fromIntegral dotH) * 0.5)
    !(icloX,icloY) = iconListOrg inventoryParam
    !(_,ploY) = palletOrg inventoryParam
    !rate = projectionRate inventoryParam
    !itvl = iconListItvl inventoryParam
    !xbordLst = [ invOx + (icloX + (itvl * i)) * rate | i <- [0 .. 9]]
    !ybordLst = [ invOy + (icloY - (itvl * i)) * rate | i <- [-1 .. 4]]
    flt f lst =  case break f lst of
      ([],_) -> Nothing
      (_,[]) -> Nothing
      (a,_) -> Just $ (length a) - 1
    chkPltY y' = (invOy + (ploY * rate)) < y'
                &&  y' < (invOy + (ploY + itvl - 2) * rate)
    !yIdx = case flt (< y) ybordLst of
             Just a -> Just a
             Nothing -> if chkPltY y then Just 5 else Nothing

setBlock :: UserStatus -> (Double,Double,Bool,Bool,Bool)
         -> Maybe (WorldIndex,Surface) -> [BlockIDNum]
         -> Maybe (WorldIndex,BlockIDNum)
setBlock _ _ Nothing _ = Nothing
setBlock _ (_,_,False,False,_) _ _ = Nothing
setBlock ustat (_,_,lb,rb,_) (Just ((cx,cy,cz),fpos)) plt 
  | rb = Just (setPos,(plt !! idx))
  | lb = Just ((cx,cy,cz),airBlockID)
  | otherwise = Nothing
  where
    !idx = palletIndex ustat 
    !setPos = case fpos of
      STop    -> (cx,cy + 1,cz)
      SBottom -> (cx,cy - 1,cz)
      SRight  -> (cx + 1,cy,cz)
      SLeft   -> (cx - 1,cy,cz)
      SFront  -> (cx,cy,cz - 1)
      SBack   -> (cx,cy,cz + 1)


calcPlayerMotion :: DataHdl -> UserStatus
                 -> (Double,Double) -> (Int,Int,Int,Int,Int) -> (Int,Int)
                 -> Double
                 -> UserStatus
calcPlayerMotion dtHdl usrStat' (mx,my) (f,b,l,r,jmp) (_,sy) dt =
  UserStatus
    { userPos = ( nx
                , ny 
                , nz)
    , userRot = (realToFrac nrx, realToFrac nry, realToFrac nrz) 
    , palletIndex = if idx < 0 then 0 else if idx > 8 then 8 else idx
    , userVel = (0.0,w,0.0)
    }
  where
    !(ox,oy,oz) = userPos usrStat'
    !(orx,ory,orz) = userRot usrStat'
    !(_,wo,_) = userVel usrStat'
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
    !(nx,ny,nz) = movePlayer dtHdl (ox,oy,oz) (realToFrac dx
                                            ,realToFrac dy
                                            ,realToFrac dz)

movePlayer :: DataHdl -> Pos' -> Pos' -> Pos'
movePlayer dtHdl (ox',oy',oz') (dx,dy,dz) =
  case getBlockID dtHdl (round' tx, round' y' ,round' tz) of
    Just bid -> if bid == airBlockID || chkHalf bid ||
                   not ( isCollision $ getBlockInfo bid)
      then if dl < 1
           then (tx,y',tz)
           else movePlayer dtHdl (tx,y',tz) (dx - dx', dy - dy', dz - dz')
      else case bid'' of
        Just bid' -> if chkHalf bid' && y' + 0.6 > y''
          then if dl < 1
            then (tx,y'',tz)
            else movePlayer dtHdl (tx,y'',tz)
                                  (dx - dx', dy - dy', dz - dz')
          else (ox',y',oz')
        Nothing -> (ox',y',oz')
    Nothing -> (ox',oy',oz') 
  where 
    !bid'' = getBlockID dtHdl (round' ox', round' oy' ,round' oz')
    !dl = sqrt (dx * dx + dz * dz + dy * dy)
    !(dx',dy',dz') = if dl < 1
          then (dx,dy,dz)
          else (dx / dl, dy / dl, dz / dl)
    !(tx,tz) = (ox' + dx', oz' + dz')
    !y' = calYpos dtHdl (ox',oy',oz') dy'
    !y'' = calYpos dtHdl (tx,y'+1,tz) (-1)

calYpos :: DataHdl -> (Double,Double,Double) -> Double -> Double
calYpos _   (_,y,_) 0 = y
calYpos dtHdl (x,y,z) dy = if (abs ndy) < 0.001
    then {-trace (show (y',y0,c,ly,dy,ndy)) -} y'
    else {- trace (show (y',ndy)) $ -} calYpos dtHdl (x,y',z) ndy
  where
    !y0 = fromIntegral $ (round' y :: Int)
    !ly = y - y0
    !t' = case getBlockID dtHdl (round' x, round' y, round' z) of
      Just t -> if t == airBlockID  || 
                   not ( isCollision $ getBlockInfo t)
            then 0
            else if chkHalf t == True then 2 else 1
      Nothing -> 0
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
    !rx = bounded' 90 (-90) $ realToFrac frxo + frx*dt
    !rz = bounded' 90 (-90) $ realToFrac frzo + frz*dt
    !try = realToFrac fryo + fry*dt
    !ry | try >= 180 = -360 + try
        | try < -180 =  360 + try
        | otherwise  = try 

playerMotion :: Double -> Double -> Vel' -> Pos' 
playerMotion dt r0 (v,u,w) = (dx,dy,dz)
  where
    !dx = dt * nextx (v,u,r0)    -- right/left
    !dy = dt * w                 -- up/down
    !dz = dt * nextz (v,u,r0)    -- head
    phy r = (pi * r) / 180.0
    nextz (df,ds,r) = (df * cos (phy r)) - (ds * sin (phy r))
    nextx (df,ds,r) = (ds * cos (phy r)) + (df * sin (phy r))

guiProcess :: GuiResource -> (Double,Double,Bool,Bool,Bool)
           -> ((RunMode,Bool),(Bool,Bool))
guiProcess res (x,y,btn1,_,_) = (chkModeChg,chkExit)
  where
    !plybtnPosOrgn = widgetPlayBtnPos res
    !plybtnSize = widgetPlayBtnSiz res
    !extbtnPosOrgn = widgetExitBtnPos res
    !extbtnSize = widgetExitBtnSiz res
    f2d (a,b) = (realToFrac a, realToFrac b) -- GLfloat to Double
    chkEntrBtn (xo,yo) (w,h) =
       xo < x && x < (xo + w) && yo < y && y < (yo + h) 
    !isPlyBtnEntr = chkEntrBtn (f2d plybtnPosOrgn) (f2d plybtnSize)
    !chkModeChg = ( if isPlyBtnEntr && btn1 then InitMode else TitleMode 
                 , isPlyBtnEntr)
    !isExtBtnEntr = chkEntrBtn (f2d extbtnPosOrgn) (f2d extbtnSize)
    !chkExit = ( isExtBtnEntr && btn1 , isExtBtnEntr) 

drawView :: Handls 
         -> TitleModeState -> PlayModeState -> RunMode
         -> TitleModeHdl -> WorldViewVHdl
         -> DebugInfo
         -> IO ()
drawView (glfwHdl, guiRes) tmstat plstat runMode'
          tvHdl wvHdl dbgInfo = do
  !worldDispList <- getBlockVAOList wvHdl
  !winSize <- getWindowSize glfwHdl
  updateDisplay $
    case runMode' of
      TitleMode -> -- 2D
        drawTitle winSize guiRes tmstat tvHdl
      InitMode ->
        drawInit winSize guiRes (InitModeState 10) tvHdl 
      _ -> -- 3D
        drawPlay winSize guiRes usrStat' worldDispList pos plt
                 (runMode' == InventoryMode) drgSta' wvHdl
                 dbgInfo
  where
    !usrStat' = usrStat plstat
    !pos = curPos plstat
    !drgSta' = drgSta plstat
    !plt = pallet plstat
--

