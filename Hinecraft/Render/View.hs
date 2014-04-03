{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.View
  ( ViewMode (..)
  , UserStatus (..)
  , WorldDispList
  , loadGuiResource
  , initGL
  , setPerspective
  , drawBackPlane
  , putTextLine
  , updateDisplay
  , drawTitle
  , drawPlay
  , WorldVAOList
  --
  ) where

-- Font
import Graphics.Rendering.FTGL as Ft

-- OpenGL
import Graphics.Rendering.OpenGL as GL
--import qualified Graphics.GLUtil as GU
--import qualified Graphics.GLUtil.Camera3D as GU3

-- Common
--import Debug.Trace as Dbg
import Control.Monad (  forM_ {-,when, unless,void,filterM-} )
import qualified Data.Map as M

import Hinecraft.Model
import Hinecraft.Types

import Hinecraft.Render.Types
import Hinecraft.Render.Util
import Hinecraft.Render.TitleView
import Hinecraft.Render.WorldView

-- Define

-- ##################### OpenGL ###########################
data ViewMode = V2DMode | V3DTitleMode | V3DMode
  deriving (Eq,Show)

type WorldDispList = M.Map (Int,Int) [DisplayList]

drawPlay :: (Int,Int) -> GuiResource
         -> UserStatus -> WorldVAOList 
         -> Maybe (WorldIndex,Surface)
         -> [BlockIDNum] -> Bool -> DragDropState
         -> WorldViewVHdl 
         -> IO ()
drawPlay (w,h) guiRes usrStat' worldDispList pos plt
         invSw dragDrop wvHdl = do

  -- World
  drawWorldView wvHdl (w,h) worldDispList usrStat' pos

  -- HUD
  renderHUD (w,h) guiRes tex pIndex invSw plt dragDrop
  where
    pIndex =  palletIndex usrStat'
    tex = blockTexture wvHdl

renderHUD :: (Int,Int) -> GuiResource -> TextureObject 
          -> Int -> Bool -> [BlockIDNum] -> DragDropState
          -> IO ()
renderHUD (w,h) guiRes blocktex pIndex invSw plt dragDrop = 
  preservingMatrix $ do
    setPerspective V2DMode w h
    depthMask $= Disabled

    if invSw
      then renderInventory (w,h) guiRes blocktex plt dragDrop -- Inventry
      else preservingMatrix $ do -- Scope
        let !(pltW,pltH) = (15 * rate, 15 * rate)
            !(pltOx,pltOy) = ((w' - pltW) / 2, (h' - pltH) / 2)
        drawBackPlane (pltOx,pltOy) (pltW,pltH) (Just widTex')
                      (240/256,0) (15/256,15/256) (1.0,1.0,1.0,1.0)   

    preservingMatrix $ do
      -- Pallet
      let !(pltW,pltH) = (182 * rate, 22 * rate)
          !(pltOx,pltOy) = ((w' - pltW) / 2, 2)
          !curXpos = pltOx + rate + 20 * rate * fromIntegral pIndex 
      drawBackPlane (pltOx,pltOy) (pltW,pltH) (Just widTex')
                    (0,0) (182/256,22/256) (1.0,1.0,1.0,1.0)
      drawBackPlane (curXpos,rate) (20 * rate, 20 * rate)
                    (Just widTex')
                    (1/256,24/256) (22/256,22/256) (1.0,1.0,1.0,1.0)

      mapM_ (\ (p,ib) -> drawIcon blocktex (16 * rate)
        (pltOx + rate + (20 * rate / 2) + (20 * rate * p) ,12) ib)
        $ zip [0.0,1.0 .. ] plt

    depthMask $= Enabled
  where
    !rate = 2.5
    !(w',h') = (fromIntegral w, fromIntegral h)
    !widTex' = widgetsTexture guiRes

renderInventory :: (Int,Int) -> GuiResource -> TextureObject
                -> [BlockIDNum] -> DragDropState -> IO ()
renderInventory (w,h) guiRes tex plt dragDrop = preservingMatrix $ do
  -- Back
  drawBackPlane (0,0) (fromIntegral w, fromIntegral h) Nothing
                (0,0) (0,0) (0.0,0.0,0.0,0.8)
  -- borad
  let !(pltW,pltH) = (rate * fromIntegral dotW, rate * fromIntegral dotH )
      !(pltOx,pltOy) = ((w' - pltW) * 0.5, (h' - pltH) * 0.5)
  drawBackPlane (d2f (pltOx,pltOy)) (d2f (pltW,pltH)) (Just tdlg')
                (d2f (orgU,orgV)) (d2f (uvRctW, uvRctH))
                (1.0,1.0,1.0,1.0)   
  -- Catalog
  mapM_ (\ (liNo,iclst) ->
    forM_ iclst (\ (p,ib) -> drawIcon tex (realToFrac $ icSz * rate)
      (d2f 
        ( pltOx + ((icloX - 1) + itvl * 0.5) * rate + (itvl * rate * p) 
        , pltOy + ((icloY + 1) - itvl * liNo) * rate)) ib))
      [ (0, zip [0.0,1.0 .. ] $ take 9 blockCatalog)
      , (1, zip [0.0,1.0 .. ] $ take 9 $ drop 9 blockCatalog)
      ]
  -- pallet
  mapM_ (\ (p,ib) -> drawIcon tex (realToFrac $ icSz * rate)
      (d2f ( pltOx + ((ploX - 1) + itvl * 0.5) * rate + (itvl * rate * p) 
           , pltOy + (ploY + 1) * rate)) ib)
      $ zip [0.0,1.0 .. ] plt

  -- Drag & Drop
  case dragDrop of
    Just ((x,y),bid) -> drawIcon tex (realToFrac $ icSz * rate) 
                          (realToFrac x,realToFrac $ y - (rate * icSz * 0.5))
                          bid  
    Nothing -> return ()
  where
    d2f (x,y) = (realToFrac x, realToFrac y)
    icSz = iconSize inventoryParam
    rate = projectionRate inventoryParam
    itvl = iconListItvl inventoryParam
    (uvRctW,uvRctH) = uvRectSize inventoryParam
    (orgU,orgV) = uvOrg inventoryParam
    (dotW,dotH) = rectDotSize inventoryParam
    (icloX,icloY) = iconListOrg inventoryParam
    (ploX,ploY) = palletOrg inventoryParam
    (w',h') = (fromIntegral w, fromIntegral h)
    tdlg' = invDlgTexture guiRes

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
  --colorMaterial     $= Just (FrontAndBack, AmbientAndDiffuse)
  colorMaterial     $= Just (GL.Front, AmbientAndDiffuse)

loadGuiResource :: FilePath -> (Int,Int) -> IO GuiResource
loadGuiResource home (w,h) = do
  wtex' <- loadTexture widPng
  font' <- Ft.createBitmapFont fontPath
  itex' <- loadTexture invDlgPng
  ibtex' <- loadTexture invTabPng
  return GuiResource
    { widgetsTexture = wtex'
    , font = font'
    , widgetPlayBtnPos = ((w' - texBtnWd * rate) * 0.5 , h' * 0.5)
    , widgetPlayBtnSiz = (texBtnWd * rate, texBtnHt * rate)
    , widgetExitBtnPos = ((w' - texBtnWd * rate) * 0.5 , h' * 0.2)
    , widgetExitBtnSiz = (texBtnWd * rate, texBtnHt * rate)
    , invDlgTexture = itex'
    , invDlgTbTexture = ibtex'
    }
  where
    (w',h') = (fromIntegral w, fromIntegral h)
    rate = 3.0
    texBtnWd = 200
    texBtnHt = 20
    widPng    = home ++ "/.Hinecraft/textures/gui/widgets.png"
    invDlgPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tab_items.png"
    invTabPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tabs.png"
    fontPath = home ++ "/.Hinecraft/Font/ipamp.ttf" 

-- ##################### Font(Text) #######################

-- ##################### GLFW #############################

updateDisplay :: IO () -> IO ()
updateDisplay drawFn = do
  clear [ColorBuffer,DepthBuffer]
  drawFn
  flush

