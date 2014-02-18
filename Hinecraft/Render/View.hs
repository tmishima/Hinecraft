module Hinecraft.Render.View
  ( GuiResource (..)
  , ViewMode (..)
  , WorldResource (..)
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
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Codec.Picture as CdP
import qualified Data.Vector.Storable as Vct

-- Common
import Debug.Trace as Dbg
import Data.IORef
import Control.Monad ( {-unless,void,when,-} void )
import Hinecraft.Model
import Hinecraft.Types
import Hinecraft.Util

-- Define

data GuiResource = GuiResource
  { backgroundBoxTexture :: [GLuint]
  , backgroundTitleTexture :: GLuint
  , widgetsTexture :: GLuint
  , widgetPlayBtnPos :: (GLfloat,GLfloat)
  , widgetPlayBtnSiz :: (GLfloat,GLfloat)
  , widgetExitBtnPos :: (GLfloat,GLfloat)
  , widgetExitBtnSiz :: (GLfloat,GLfloat)
  , font :: Ft.Font
  , invDlgTexture :: GLuint
  , invDlgTbTexture :: GLuint
  }

data UserStatus = UserStatus
  { userPos :: (GLfloat,GLfloat,GLfloat)
  , userRot :: (GLfloat,GLfloat,GLfloat) 
  , palletIndex :: Int
  , userVel :: (Double,Double,Double)
  }
  deriving (Eq,Show)

type VertexPosition = (GLfloat,GLfloat,GLfloat)

data WorldResource = WorldResource
  { blockTexture :: GLuint
  }
  deriving (Eq,Show)

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
         -> [BlockID]
         -> IO ()
drawPlay (w,h) guiRes wldRes usrStat worldDispList pos plt = do
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
      mapM_ (\ (p,ib) -> drawIcon wldRes
        (pltOx + (icSz / 2) + bdSz + prSz + (icSz + prSz/2) * p ,12) ib)
        $ zip [0.0,1.0 .. ] plt

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
      mapM_ (\ ((x', y', z'),_) ->
        vertex (Vertex3 x' y' z')) ndlst

drawIcon :: WorldResource -> (GLfloat,GLfloat) -> BlockID -> IO ()
drawIcon wldRes (ox,oy) bID | null texIdx = return ()
                            | otherwise = preservingMatrix $ do
  texture Texture2D $= Enabled 
  glBindTexture gl_TEXTURE_2D tex

  let [tt,_,_,tl,tf,_] = texIdx
  renderPrimitive Quads $ do
    -- top 
    color $ Color3 0.8 0.8 (0.8::GLfloat)
    mapM_ drawf $ zip (calcUV tt)
      [ (ox, oy + icSzH)
      , (ox + icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox, oy + icSzH + icSzW * (sin.d2r) 2 * rt)
      , (ox - icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      ]
    -- left 
    color $ Color3 0.5 0.5 (0.5::GLfloat)
    mapM_ drawf $ zip (calcUV tl)
      [ (ox, oy + icSzH)
      , (ox + icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox + icSzW, oy + icSzW * (sin.d2r) rt)
      , (ox, oy)
      ]
    -- front 
    color $ Color3 0.5 0.5 (0.5::GLfloat)
    mapM_ drawf $ zip (calcUV tf)
      [ (ox - icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox, oy + icSzH)
      , (ox, oy)
      , (ox - icSzW, oy + icSzW * (sin.d2r) rt)
      ]
  where
    icSzW = 20.0
    icSzH = case shape $ getBlockInfo bID of
              Cube -> 20.0
              Half _ -> 10.0
              _ -> 20.0
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

blockNodeVertex :: [VertexPosition]
blockNodeVertex = 
  [ ( -0.5, -0.5, -0.5) -- P0
  , (  0.5, -0.5, -0.5) -- P1
  , (  0.5, -0.5,  0.5) -- P2
  , ( -0.5, -0.5,  0.5) -- P3
  , (  0.5,  0.5,  0.5) -- P4
  , ( -0.5,  0.5,  0.5) -- P5
  , ( -0.5,  0.5, -0.5) -- P6
  , (  0.5,  0.5, -0.5) -- P7
  , (  0.5,  0.0,  0.5) -- P8
  , ( -0.5,  0.0,  0.5) -- P9
  , ( -0.5,  0.0, -0.5) -- P10
  , (  0.5,  0.0, -0.5) -- P11
  ]

getVertexList :: Shape -> Surface
              -> [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat))]
getVertexList Cube f = case f of
  STop    -> zip [p7,p6,p5,p4] uvLst
  SBottom -> zip [p0,p1,p2,p3] uvLst
  SFront  -> zip [p6,p7,p1,p0] uvLst
  SBack   -> zip [p4,p5,p3,p2] uvLst
  SRight  -> zip [p7,p4,p2,p1] uvLst
  SLeft   -> zip [p5,p6,p0,p3] uvLst
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:_) = blockNodeVertex
    uvLst = [ (0.0,0.0)
            , (1.0,0.0)
            , (1.0,1.0)
            , (0.0,1.0)
            ]
getVertexList (Half b) f 
  | not b = case f of -- False
    STop    -> zip [p11,p10,p9,p8] uvLstD
    SBottom -> zip [p0,p1,p2,p3] uvLstD
    SFront  -> zip [p10,p11,p1,p0] uvLstD
    SBack   -> zip [p8,p9,p3,p2] uvLstD
    SRight  -> zip [p11,p8,p2,p1] uvLstD
    SLeft   -> zip [p9,p10,p0,p3] uvLstD
  | otherwise = case f of
    STop    -> zip [p7,p6,p5,p4] uvLstU 
    SBottom -> zip [p10,p11,p8,p9] uvLstU
    SFront  -> zip [p6,p7,p11,p10] uvLstU
    SBack   -> zip [p4,p5,p9,p8] uvLstU
    SRight  -> zip [p7,p4,p8,p11] uvLstU
    SLeft   -> zip [p5,p6,p10,p9] uvLstU
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:p8:p9:p10:p11:_) = blockNodeVertex
    uvLstU = [uv0,uv1,uv2,uv3]
    uvLstD = [uv3,uv2,uv4,uv5]
    (uv0,uv1,uv2,uv3,uv4,uv5)
      = ((0.0,0.0)
        ,(1.0,0.0)
        ,(1.0,0.5)
        ,(0.0,0.5)
        ,(1.0,1.0)
        ,(0.0,1.0))

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
      let sp = shape $ getBlockInfo bid
          texIdx = textureIndex $ getBlockInfo bid
           -- Top | Bottom | Right | Left | Front | Back
          [tt',tb',tr',tl',tf',tba'] = if null texIdx 
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
        calcUV (i',j') (pos,(u,v))
          = ( ( i * (1/16) + (1/16) * u
              , j * (1/16) + (1/16) * v
              ) 
            , pos)
          where i = fromIntegral i' ; j = fromIntegral j'

d2fv3 :: (Double,Double,Double) -> (GLfloat,GLfloat,GLfloat)
d2fv3 (a,b,c) = ( realToFrac a, realToFrac b, realToFrac c)


-- 

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
  itex' <- loadTextures invDlgPng
  ibtex' <- loadTextures invTabPng
  return GuiResource
    { backgroundBoxTexture = tex'
    , backgroundTitleTexture = ttex'
    , widgetsTexture = wtex'
    , font = font'
    , widgetPlayBtnPos = (365, 768 - 410)
    , widgetPlayBtnSiz = (640, 62.5)
    , widgetExitBtnPos = (690, 768 - 614)
    , widgetExitBtnSiz = (155 * 2, 62.5)
    , invDlgTexture = itex'
    , invDlgTbTexture = ibtex'
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
    invDlgPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tab_items.png"
    invTabPng = home ++ "/.Hinecraft/textures/gui/container/creative_inventory/tabs.png"
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
      mapM_ (\ ((x', y', z'),(u,v)) -> do
               glTexCoord2f u v
               vertex (Vertex3 x' y' z')) 
           $ getVertexList Cube f

-- ##################### GLFW #############################

updateDisplay :: IO () -> IO ()
updateDisplay drawFn = do
  clear [ColorBuffer,DepthBuffer]
  drawFn
  flush

