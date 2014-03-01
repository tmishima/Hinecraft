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
  , drawBackGroundBox
  , updateDisplay
  , drawTitle
  , drawPlay
  , genSufDispList
  ) where

-- Font
import Graphics.Rendering.FTGL as Ft

-- OpenGL
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw

-- Common
import Debug.Trace as Dbg
import Data.IORef
import Control.Monad (  forM_ {-,when, unless,void,filterM-} )
import qualified Data.ByteString as B

import Hinecraft.Model
import Hinecraft.Types
import Hinecraft.Util

import Hinecraft.Render.Types
import Hinecraft.Render.Util
-- Define


-- ##################### OpenGL ###########################
data ViewMode = V2DMode | V3DTitleMode | V3DMode
  deriving (Eq,Show)

type WorldDispList = IORef [(ChunkNo, [(BlockNo,DisplayList)])]

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

drawPlay :: (Int,Int) -> GuiResource -> WorldResource
         -> UserStatus -> WorldDispList
         -> Maybe (WorldIndex,Surface)
         -> [BlockIDNum] -> Bool -> DragDropState 
         -> IO ()
drawPlay (w,h) guiRes wldRes usrStat' worldDispList pos plt
         invSw dragDrop = do
  -- World
  preservingMatrix $ do
    setPerspective V3DMode w h
  
    -- 視線
    rotate (-urx :: GLfloat) $ Vector3 1.0 0.0 (0.0::GLfloat)
    rotate (-ury :: GLfloat) $ Vector3 0.0 1.0 (0.0::GLfloat) -- z軸
 
    preservingMatrix $ do
      scale 100.0 100.0 (100.0::GLfloat)
      drawBackGroundBox' 

    -- カメラ位置
    translate $ Vector3 (-ux :: GLfloat) (-uy - 1.5) (-uz) 

    -- Cursol 選択された面を強調
    renderCurFace  pos

    color $ Color3 0.0 1.0 (0.0::GLfloat)
    readIORef worldDispList
      >>= mapM_ (\ (_,b) -> mapM_ (\ (_,d) -> callList d) b)  

  -- HUD
  renderHUD (w,h) guiRes wldRes pIndex invSw plt dragDrop

  where
    d2f (a,b,c) = (realToFrac a, realToFrac b, realToFrac c)
    (ux,uy,uz) = d2f $ userPos usrStat'
    (urx,ury,_) = d2f $ userRot usrStat'
    pIndex =  palletIndex usrStat'
    drawBackGroundBox' = preservingMatrix $ do
      color $ Color4 (180/255) (226/255) (255/255) (1.0::GLfloat)
      texture Texture2D $= Disabled
      renderPrimitive Quads $ do
        genSuf (0,0,-1) $ getVertexList Cube SFront  -- Front
        genSuf (-1,0,0) $ getVertexList Cube SRight  -- Right
        genSuf (1,0,0)  $ getVertexList Cube SLeft   -- Left
        genSuf (0,-1,0) $ getVertexList Cube STop    -- Top
        genSuf (0,1,0)  $ getVertexList Cube SBottom -- Bottom
        genSuf (0,0,1)  $ getVertexList Cube SBack   -- Back 
    genSuf :: (GLfloat,GLfloat,GLfloat)
           -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat))]
           -> IO ()
    genSuf (nx,ny,nz) ndlst = do
      normal $ Normal3 nx ny nz
      forM_ ndlst $ \ ((x', y', z'),_) -> vertex (Vertex3 x' y' z')

renderHUD :: (Int,Int) -> GuiResource ->  WorldResource
          -> Int -> Bool -> [BlockIDNum] -> DragDropState
          -> IO ()
renderHUD (w,h) guiRes wldRes pIndex invSw plt dragDrop = 
  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
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

    glPopAttrib 
    depthMask $= Enabled
    glEnable gl_DEPTH_TEST
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
  initShader

initShader :: IO ()
initShader = do
  vsh <- createShader VertexShader
  shaderSourceBS vsh $= vertSrc
  compileShader vsh

  fsh <- createShader FragmentShader 
  shaderSourceBS fsh $= flgSrc
  compileShader fsh

  prg <- createProgram
  attachShader prg vsh
  attachShader prg fsh

  linkProgram prg

  validateProgram prg


-- | GLSL Source code for a text vertex shader.
vertSrc :: B.ByteString
vertSrc = B.concat [ "#version 300\n"
                   , "\n"
                   , "in vec3 VertexPosition;\n"
                   , "in vec3 VertexColor;\n"
                   , "\n"
                   , "out vec3 Color;\n"
                   , "\n"
                   , "void main () {\n"
                   , " Color = VertexColor;\n"
                   , " gl_Position = vec4(VertexPosition, 1.0);\n"
                   , "}\n"
                   ]

-- | GLSL Source code for a text fragment shader.
flgSrc :: B.ByteString
flgSrc = B.concat  [ "#version 300\n"
                   , "\n"
                   , "in vec3 Color;\n"
                   , "\n"
                   , "out vec4 FragColor;\n"
                   , "\n"
                   , "void main () {\n"
                   , " FragColor = vec4(color, 1.0);\n"
                   , "}\n"
                   ]

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
      let !sp = shape $ getBlockInfo bid
          !texIdx = textureIndex $ getBlockInfo bid
           -- Top | Bottom | Right | Left | Front | Back
          ![tt',tb',tr',tl',tf',tba'] = if null texIdx 
                                  then replicate 6 (0,0)
                                  else texIdx
      mapM_ (\ (f,l) -> case f of
         STop -> genSuf (0,-1,0) (setColor 0 l)
           $ map (calcUV tt') (getVertexList sp STop)
         SBottom -> genSuf (0,-1,0) (setColor 1 l)
           $ map (calcUV tb') (getVertexList sp SBottom)
         SRight -> genSuf (1,0,0) (setColor 2 l) 
           $ map (calcUV tf') (getVertexList sp SRight)
         SLeft -> genSuf (-1,0,0) (setColor 3 l) 
           $ map (calcUV tba') (getVertexList sp SLeft)
         SFront -> genSuf (0,0,-1) (setColor 4 l)
           $ map (calcUV tr') (getVertexList sp SFront)
         SBack -> genSuf (0,0,1) (setColor 5 l)
           $ map (calcUV tl') (getVertexList sp SBack)
         ) fs
      where
        d2fv3 :: (Double,Double,Double) -> VrtxPos3D 
        d2fv3 (a,b,c) = ( realToFrac a, realToFrac b, realToFrac c)
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
          forM_ ndlst $ \ ((u,v),(x', y', z')) -> do
                   glTexCoord2f u v
                   vertex (Vertex3 (x' + fromIntegral x)
                                   (y' + fromIntegral y)
                                   (z' + fromIntegral z))
        calcUV (i',j') (pos,(u,v))
          = ( ( i * (1/16) + (1/16) * u
              , j * (1/16) + (1/16) * v
              ) 
            , pos)
          where i = fromIntegral i' ; j = fromIntegral j'

-- 

drawTitle :: (Int,Int) -> GuiResource -> TitleModeState -> IO ()
drawTitle (w,h) res stat = do
  preservingMatrix $ do
    setPerspective V3DTitleMode w h
    scale 10.0 10.0 (10.0::GLfloat)
    rotate 10 $ Vector3 1.0 0.0 (0::GLfloat)
    rotate (realToFrac $ rotW stat) $ Vector3 0.0 1.0 (0::GLfloat)
    drawBackGroundBox bkgTex

  preservingMatrix $ do
    setPerspective V2DMode w h
    glPushAttrib gl_DEPTH_BUFFER_BIT
    glDisable gl_DEPTH_TEST
    depthMask $= Disabled

    putTextLine font' (Just (1,1,1)) (Just 20) (10,10) "Hinecraft 0.0.3" 

    -- White
    drawBackPlane (0,0) (fromIntegral w, fromIntegral h) Nothing
                  (0,0) (1,1) (1.0,1.0,1.0,0.30)
    -- Title 
    drawBackPlane (232,768 - 233) (508, 145) (Just titleTex)
                  (0,0) (0.61,0.172) (1.0,1.0,1.0,1.0)
    drawBackPlane (232 + 508 - 5, 768 - 233) (382, 145) (Just titleTex)
                  (0,0.176) (0.458,0.172) (1.0,1.0,1.0,1.0)


    -- Botton 
    let vm = slcBtnTexCrd $ isModeChgBtnEntr stat
    drawBackPlane (xPlybtnPos, yPlybtnPos) (wPlybntSiz,hPlybtnSiz)
                  (Just widTex)
                  (0,vm) (200/256,20/256) (1.0,1.0,1.0,1.0)
    putTextLine font' (Just (1,1,1)) (Just 30)
      (590,768 - 410 + 20) "Single Player" 
    -- 
    let ve = slcBtnTexCrd $ isExitBtnEntr stat
    drawBackPlane (xExtbtnPos, yExtbtnPos) (wExtbtnSiz / 2, hExtbtnSiz)
                  (Just widTex)
                  (0,ve) (0.20,0.078) (1.0,1.0,1.0,1.0)
    drawBackPlane (xExtbtnPos + wExtbtnSiz / 2, yExtbtnPos) (wExtbtnSiz / 2, hExtbtnSiz)
                  (Just widTex)
                  (0.58,ve) (0.20,0.078) (1.0,1.0,1.0,1.0)
    putTextLine font' (Just (1,1,1)) (Just 30) (780,768 - 614 + 20) "Quit game" 

    glPopAttrib 
    depthMask $= Enabled
    glEnable gl_DEPTH_TEST

  where
    slcBtnTexCrd sw = if sw then 86 / 256 else 66 / 256
    bkgTex = backgroundBoxTexture res
    titleTex = backgroundTitleTexture res
    widTex = widgetsTexture res
    (xPlybtnPos,yPlybtnPos) = widgetPlayBtnPos res
    (wPlybntSiz,hPlybtnSiz) = widgetPlayBtnSiz res
    (xExtbtnPos,yExtbtnPos) = widgetExitBtnPos res
    (wExtbtnSiz,hExtbtnSiz) = widgetExitBtnSiz res
    font' = font res

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
  btex' <- loadTextures blkPng 
  return WorldResource
    { blockTexture = btex'
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"
    --skyPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/cloud1.png"
    --starPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/star1.png"

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
  itex' <- loadTextures invDlgPng
  ibtex' <- loadTextures invTabPng
  return GuiResource
    { backgroundBoxTexture = tex'
    , backgroundTitleTexture = ttex'
    , widgetsTexture = wtex'
    , font = font'
    , widgetPlayBtnPos = ((1366 - texBtnWd * rate) * 0.5 , 768 - 410)
    , widgetPlayBtnSiz = (texBtnWd * rate, texBtnHt * rate)
    , widgetExitBtnPos = ( 1366 - (1366 - texBtnWd * rate) * 0.5
                           - texBtnWd * hfrate
                         , 768 - 614)
    , widgetExitBtnSiz = (texBtnWd * hfrate , texBtnHt * rate)
    , invDlgTexture = itex'
    , invDlgTbTexture = ibtex'
    }
  where
    rate = 3.0
    hfrate = 1.4
    texBtnWd = 200
    texBtnHt = 20
    bkgndPng0 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_0.png"
    bkgndPng1 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_1.png"
    bkgndPng2 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_2.png"
    bkgndPng3 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_3.png"
    bkgndPng4 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_4.png"
    bkgndPng5 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_5.png"
    bkgTtlPng = home ++ "/.Hinecraft/textures/gui/title/hinecraft.png"
    widPng    = home ++ "/.Hinecraft/textures/gui/widgets.png"
    invDlgPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tab_items.png"
    invTabPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tabs.png"
    fontPath = "/usr/share/fonts/truetype/takao-mincho/TakaoPMincho.ttf" -- linux

-- ##################### Font(Text) #######################

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
      forM_ (getVertexList Cube f)
        (\ ((x', y', z'),(u,v)) -> do
               glTexCoord2f u v
               vertex (Vertex3 x' y' z')) 

-- ##################### GLFW #############################

updateDisplay :: IO () -> IO ()
updateDisplay drawFn = do
  clear [ColorBuffer,DepthBuffer]
  drawFn
  flush

