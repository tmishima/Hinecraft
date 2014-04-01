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
  , loadWorldResouce
  , initGL
  , setPerspective
  , gen3dCursol
  , drawBackPlane
  , getVertexList
  , putTextLine
  , updateDisplay
  , drawTitle
  , drawPlay
  , genSufDispList
  , WorldVAOList
  --
  ) where

-- Font
import Graphics.Rendering.FTGL as Ft

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GU
--import qualified Graphics.GLUtil.Camera3D as GU3

-- Common
import Debug.Trace as Dbg
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

drawPlay :: (Int,Int) -> GuiResource -> WorldResource
         -> UserStatus -> WorldVAOList 
         -> Maybe (WorldIndex,Surface)
         -> [BlockIDNum] -> Bool -> DragDropState
         -> WorldViewVHdl 
         -> IO ()
drawPlay (w,h) guiRes wldRes usrStat' worldDispList pos plt
         invSw dragDrop wvHdl = do

  -- World
  drawWorldView wvHdl (w,h) worldDispList usrStat' pos

  -- HUD
  renderHUD (w,h) guiRes wldRes pIndex invSw plt dragDrop
  where
    pIndex =  palletIndex usrStat'

renderHUD :: (Int,Int) -> GuiResource ->  WorldResource
          -> Int -> Bool -> [BlockIDNum] -> DragDropState
          -> IO ()
renderHUD (w,h) guiRes wldRes pIndex invSw plt dragDrop = 
  preservingMatrix $ do
    setPerspective V2DMode w h
    depthMask $= Disabled

    if invSw
      then renderInventory (w,h) guiRes wldRes plt dragDrop -- Inventry
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

      mapM_ (\ (p,ib) -> drawIcon wldRes (16 * rate)
        (pltOx + rate + (20 * rate / 2) + (20 * rate * p) ,12) ib)
        $ zip [0.0,1.0 .. ] plt

    depthMask $= Enabled
  where
    !rate = 2.5
    !(w',h') = (fromIntegral w, fromIntegral h)
    !widTex' = widgetsTexture guiRes

renderInventory :: (Int,Int) -> GuiResource -> WorldResource
                -> [BlockIDNum] -> DragDropState -> IO ()
renderInventory (w,h) guiRes wldRes plt dragDrop = preservingMatrix $ do
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
    forM_ iclst (\ (p,ib) -> drawIcon wldRes (realToFrac $ icSz * rate)
      (d2f 
        ( pltOx + ((icloX - 1) + itvl * 0.5) * rate + (itvl * rate * p) 
        , pltOy + ((icloY + 1) - itvl * liNo) * rate)) ib))
      [ (0, zip [0.0,1.0 .. ] $ take 9 blockCatalog)
      , (1, zip [0.0,1.0 .. ] $ take 9 $ drop 9 blockCatalog)
      ]
  -- pallet
  mapM_ (\ (p,ib) -> drawIcon wldRes (realToFrac $ icSz * rate)
      (d2f ( pltOx + ((ploX - 1) + itvl * 0.5) * rate + (itvl * rate * p) 
           , pltOy + (ploY + 1) * rate)) ib)
      $ zip [0.0,1.0 .. ] plt

  -- Drag & Drop
  case dragDrop of
    Just ((x,y),bid) -> drawIcon wldRes (realToFrac $ icSz * rate) 
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
  tu <- get maxTextureUnit
  tm <- get maxTextureSize
  lt <- get maxLights
  _ <- get bindVertexArrayObject
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

genSufDispList :: WorldResource -> SurfacePos -> Maybe DisplayList
               -> IO DisplayList
genSufDispList wldRes bsf dsp = case dsp of
    Nothing -> defineNewList Compile gen'
    Just d -> defineList d Compile gen' >> return d
  where
    gen' = do
      texture Texture2D $= Enabled
      GU.withTextures2D [tex] $ 
        renderPrimitive Quads $ mapM_ genFace bsf 
    tex = blockTexture wldRes
    genFace ((x,y,z),bid,fs) = do
      let !sp = shape $ getBlockInfo bid
          !texIdx = textureIndex $ getBlockInfo bid
           -- Top | Bottom | Right | Left | Front | Back
          ![tt',tb',tr',tl',tf',tba'] = if null texIdx 
                                  then replicate 6 (0,0)
                                  else texIdx
      mapM_ (\ f -> case f of
         STop -> genSuf (0,-1,0) (setColor 0)
           $ map (calcUV tt') (getVertexList sp STop)
         SBottom -> genSuf (0,-1,0) (setColor 1)
           $ map (calcUV tb') (getVertexList sp SBottom)
         SRight -> genSuf (1,0,0) (setColor 2) 
           $ map (calcUV tf') (getVertexList sp SRight)
         SLeft -> genSuf (-1,0,0) (setColor 3) 
           $ map (calcUV tba') (getVertexList sp SLeft)
         SFront -> genSuf (0,0,-1) (setColor 4)
           $ map (calcUV tr') (getVertexList sp SFront)
         SBack -> genSuf (0,0,1) (setColor 5)
           $ map (calcUV tl') (getVertexList sp SBack)
         ) fs
      where
        d2fv3 :: (Double,Double,Double) -> VrtxPos3D 
        d2fv3 (a,b,c) = ( realToFrac a, realToFrac b, realToFrac c)
        setColor i = d2fv3 $ bcolor (getBlockInfo bid) !! i
        genSuf :: (GLfloat,GLfloat,GLfloat)
               -> (GLfloat,GLfloat,GLfloat)
               -> [((GLfloat,GLfloat)
                  ,(GLfloat,GLfloat,GLfloat))]
               -> IO ()
        genSuf (nx,ny,nz) (r,g,b) ndlst = do
          normal $ Normal3 nx ny nz
          color $ Color3 r g b
          forM_ ndlst $ \ ((u,v),(x', y', z')) -> do
                   texCoord (TexCoord2 u v)
                   vertex (Vertex3 (x' + fromIntegral x)
                                   (y' + fromIntegral y)
                                   (z' + fromIntegral z))
        calcUV (i',j') (pos,(u,v))
          = ( ( i * (1/16) + (1/16) * u
              , j * (1/16) + (1/16) * v
              ) 
            , pos)
          where i = fromIntegral i' ; j = fromIntegral j'

-- | 最小単位のブロックを囲う枠を描画する
-- Private
--
gen3dCursol :: IO () 
gen3dCursol = renderPrimitive LineLoop
  $ drawCurLine $ map ajust [p5,p4,p7,p6] -- BackFace
  where
    drawCurLine = mapM_ (\ (x, y, z) -> vertex (Vertex3 x y z)) 
    extnd v = if v > 0 then v + 0.05 else  v - 0.05
    (_:_:_:_:p4:p5:p6:p7:_) = blockNodeVertex
    ajust (a,b,c) = ( extnd a, extnd b, extnd c)

loadWorldResouce :: FilePath -> IO WorldResource
loadWorldResouce home = do
  btex' <- loadTexture' blkPng 
  return WorldResource
    { blockTexture = btex'
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"
    --skyPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/cloud1.png"
    --starPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/star1.png"

loadGuiResource :: FilePath -> (Int,Int) -> IO GuiResource
loadGuiResource home (w,h) = do
  wtex' <- loadTexture' widPng
  font' <- Ft.createBitmapFont fontPath
  itex' <- loadTexture' invDlgPng
  ibtex' <- loadTexture' invTabPng
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
    fontPath = "/usr/share/fonts/truetype/takao-mincho/TakaoPMincho.ttf" -- linux

-- ##################### Font(Text) #######################

-- ##################### GLFW #############################

updateDisplay :: IO () -> IO ()
updateDisplay drawFn = do
  clear [ColorBuffer,DepthBuffer]
  drawFn
  flush

