{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.TitleView
  ( TitleModeHdl
  , initTitleModeView
  , drawTitle
  )
  where

--import Data.Tree 

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3
import qualified Graphics.GLUtil.Camera2D as GU2
--import Linear ( M44 ) --  eye4

import Hinecraft.Types
import Hinecraft.Render.Util
import Hinecraft.Render.Types
import Hinecraft.Render.WithSimpleShader


--type Scene = (M44 GLfloat, GU.VAO, [TextureObject], M44 GLfloat -> GU.VAO -> IO ())
--type SceneTree = Tree Scene

data TitleModeHdl = TitleModeHdl
  { shader  :: SimpleShaderProg
  , cubeVAO :: GU.VAO
  , whitePlatVAO :: GU.VAO
  , titlePlatVAO :: [GU.VAO]
  , startBtnVAO :: GU.VAO
  , quitBtnVAO :: GU.VAO
  , envCubeTex :: [TextureObject]
  , envTitleTex :: TextureObject
  }

initTitleModeView :: FilePath -> GuiResource -> IO TitleModeHdl
initTitleModeView home res = do
  sh <- initShaderProgram home
  cvao <- genCubeVAO sh
  pvao <- genWhitePlateVAO sh (w,h)
  tvao <- genTitlePlateVAO sh (w,h)
  stbvao <- genStartBtnVAO res sh
  qbvao <- genQuitBtnVAO res sh
  bktex' <- mapM loadTexture'
    [ bkgndPng0 , bkgndPng1 , bkgndPng2
    , bkgndPng3 , bkgndPng4 , bkgndPng5
    ]
  ttex' <- loadTexture' bkgTtlPng
  return TitleModeHdl
    { shader = sh
    , cubeVAO = cvao
    , whitePlatVAO = pvao
    , titlePlatVAO = tvao
    , startBtnVAO = stbvao
    , quitBtnVAO = qbvao
    , envCubeTex = bktex'
    , envTitleTex = ttex'
    }
  where
    (w,h) = (1366,768)
    bkgTtlPng = home ++ "/.Hinecraft/textures/gui/title/hinecraft.png"
    bkgndPng0 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_0.png"
    bkgndPng1 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_1.png"
    bkgndPng2 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_2.png"
    bkgndPng3 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_3.png"
    bkgndPng4 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_4.png"
    bkgndPng5 = home ++ "/.Hinecraft/textures/gui/title/background/panorama_5.png"

drawTitle :: (Int,Int) -> GuiResource -> TitleModeState
          -> TitleModeHdl
          -> IO ()
drawTitle (w,h) res stat tvHdl =
  GU.withViewport (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) $ do
    -- Draw 3D cube
    let prjMat = GU3.projectionMatrix (GU3.deg2rad 90) (fromIntegral w/ fromIntegral h) 0.1 (10::GLfloat)
    setProjViewMat sh prjMat
    setCamParam sh $ GU3.pan (realToFrac $ rotW stat) $ GU3.tilt (10::GLfloat) GU3.fpsCamera
    
    renderTitleCube sh (cubeVAO tvHdl) bkgTex 

     -- Draw 2D Title
    depthMask $= Disabled
    matrixMode $= Projection
    loadIdentity
    ortho2D 0 (realToFrac w) 0 (realToFrac h)
    matrixMode $= Modelview 0
    loadIdentity

    setProjViewMat sh $ orthoProjMatrix (fromIntegral w) (fromIntegral h)
    setCamParam sh (GU2.camera2D :: GU2.Camera GLfloat)

    -- White
    renderPlate sh (whitePlatVAO tvHdl) Nothing

    -- Botton 
    let si = slcBtnTexCrd $ isModeChgBtnEntr stat
    renderPlate' sh (startBtnVAO tvHdl) (Just widTex) si
    putTextLine font' (Just (1,1,1)) (Just 30)
                  (590, yPlybtnPos + 20) "Single Player"
    --
    let qi = slcBtnTexCrd $ isExitBtnEntr stat
    renderPlate' sh (quitBtnVAO tvHdl) (Just widTex) qi
    putTextLine font' (Just (1,1,1)) (Just 30)
                  (620 ,yExtbtnPos + 20) "Quit game"

    -- Title 
    mapM_ (\ v -> 
       renderPlate sh v $ Just $ envTitleTex tvHdl)
       (titlePlatVAO tvHdl) 
   
    putTextLine font' (Just (1,1,1)) (Just 20) (10,10)
                      $ "Hinecraft " ++ version

    depthMask $= Enabled

  where
    bkgTex = envCubeTex tvHdl
    widTex = widgetsTexture res
    sh = shader tvHdl
    font' = font res
    slcBtnTexCrd sw = if sw then 4 else 0
    (_,yPlybtnPos) = widgetPlayBtnPos res
    (_,yExtbtnPos) = widgetExitBtnPos res

-- For 2D
renderPlate :: SimpleShaderProg -> GU.VAO -> Maybe TextureObject -> IO ()
renderPlate sh vao tex = renderPlate' sh vao tex 0

renderPlate' :: SimpleShaderProg -> GU.VAO -> Maybe TextureObject -> Int -> IO ()
renderPlate' sh vao tex i = do
  GL.currentProgram GL.$= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled
  GU.withVAO vao $
    case tex of
      Just t -> do
        enableTexture sh True
        GU.withTextures2D [t] $ drawArrays Quads (fromIntegral i) 4
      Nothing -> do  
        textureBinding Texture2D $= Nothing
        enableTexture sh False
        drawArrays Quads (fromIntegral i) 4
  GL.currentProgram GL.$= Nothing
  where 
    !shprg' = getShaderProgram sh

genQuitBtnVAO :: GuiResource -> SimpleShaderProg -> IO GU.VAO
genQuitBtnVAO res shd =
  makeSimpShdrVAO shd platVert pColor pTexCoord
  where
    (xbtnPos,ybtnPos) = widgetExitBtnPos res
    (wbtnSiz,hbtnSiz) = widgetExitBtnSiz res
    !platVert = concat [p0,p1,p2,p3,p0,p1,p2,p3]

    p0,p1,p2,p3 :: [GLfloat]
    p0 = [ xbtnPos, ybtnPos, 0.0]
    p1 = [ xbtnPos + wbtnSiz, ybtnPos, 0.0]
    p2 = [ xbtnPos + wbtnSiz, ybtnPos + hbtnSiz, 0.0]
    p3 = [ xbtnPos, ybtnPos + hbtnSiz, 0.0]

    pColor :: [GLfloat]
    !pColor = concat $ replicate 8 [ 1.0, 1.0, 1.0, 1.0 ]

    pTexCoord :: [GLfloat]
    (cox,coy1,coy2,cw,ch) = ( 0, 66/256, 86/256, 200/256, 20/256)
    pTexCoord = [ cox, coy1, cox + cw, coy1, cox + cw, coy1 + ch, cox, coy1 + ch
                , cox, coy2, cox + cw, coy2, cox + cw, coy2 + ch, cox, coy2 + ch]

genStartBtnVAO :: GuiResource -> SimpleShaderProg -> IO GU.VAO
genStartBtnVAO res shd =
  makeSimpShdrVAO shd platVert pColor pTexCoord
  where
    (xbtnPos,ybtnPos) = widgetPlayBtnPos res
    (wbntSiz,hbtnSiz) = widgetPlayBtnSiz res
    platVert :: [GLfloat]
    !platVert = concat [p0,p1,p2,p3,p0,p1,p2,p3]

    p0,p1,p2,p3 :: [GLfloat]
    !p0 = [ xbtnPos, ybtnPos, 0.0]
    !p1 = [ xbtnPos + wbntSiz, ybtnPos, 0.0]
    !p2 = [ xbtnPos + wbntSiz, ybtnPos + hbtnSiz, 0.0]
    !p3 = [ xbtnPos, ybtnPos + hbtnSiz, 0.0]

    pColor :: [GLfloat]
    !pColor = concat $ replicate 8 [ 1.0, 1.0, 1.0, 1.0 ]

    pTexCoord :: [GLfloat]
    (cox,coy1,coy2,cw,ch) = ( 0, 66/256, 86/256, 200/256, 20/256)
    pTexCoord = [ cox, coy1, cox + cw, coy1, cox + cw, coy1 + ch, cox, coy1 + ch
                , cox, coy2, cox + cw, coy2, cox + cw, coy2 + ch, cox, coy2 + ch]

genTitlePlateVAO :: SimpleShaderProg -> (Int,Int) -> IO [GU.VAO]
genTitlePlateVAO sh (w',h') = 
  mapM (\ (v,c) ->
    makeSimpShdrVAO sh v pColor c)
      [(platVert1,pTexCoord1),(platVert2,pTexCoord2)]
  where
    (w,h) = (fromIntegral w', fromIntegral h')
    (texW1,texW2,texH) = (310,240,90)
    rate = 1.65
    (orgx1,orgy1) = ( (w - (texW1 + texW2) * rate ) / 2
                    , h * 0.85  - texH * rate )
    (orgx2,orgy2) = ( orgx1 + texW1 * rate , orgy1)
    platVert1,platVert2 :: [GLfloat]
    !platVert1 = concat [p0,p1,p2,p3]
    p0,p1,p2,p3 :: [GLfloat]
    !p0 = [ orgx1, orgy1,  0.0]
    !p1 = [ orgx1 + texW1 * rate, orgy1,  0.0]
    !p2 = [ orgx1 + texW1 * rate, orgy1 + texH * rate,  0.0]
    !p3 = [ orgx1,  orgy1 + texH * rate,  0.0]
    p4,p5,p6,p7 :: [GLfloat]
    !platVert2 = concat [p4,p5,p6,p7]
    !p4 = [ orgx2, orgy2,  0.0]
    !p5 = [ orgx2 + texW2 * rate, orgy2,  0.0]
    !p6 = [ orgx2 + texW2 * rate, orgy2 + texH * rate,  0.0]
    !p7 = [ orgx2,  orgy2 + texH * rate,  0.0]
    pColor :: [GLfloat]
    !pColor = concat $ replicate 4 [ 1.0, 1.0, 1.0, 1.0 ]

    pTexCoord1,pTexCoord2 :: [GLfloat]
    pTexCoord1 = [ 0.0, texH / 512, texW1 / 512, texH / 512
                 , texW1 / 512, 0.0 , 0.0, 0.0 ] 
    pTexCoord2 = [ 0.0, 2 * texH / 512 , texW2 / 512, 2 * texH / 512 
                 , texW2 / 512, texH / 512 , 0.0, texH / 512 ] 

genWhitePlateVAO :: SimpleShaderProg -> (Int,Int) ->  IO GU.VAO
genWhitePlateVAO sh (w',h') =
  makeSimpShdrVAO sh platVert pColor pTexCoord
  where
    (w,h) = (fromIntegral w', fromIntegral h')
    platVert :: [GLfloat]
    !platVert = concat [p0,p1,p2,p3]

    p0,p1,p2,p3 :: [GLfloat]
    !p0 = [  0.0,  0.0,  0.0]
    !p1 = [    w,  0.0,  0.0]
    !p2 = [    w,    h,  0.0]
    !p3 = [  0.0,    h,  0.0]

    pColor :: [GLfloat]
    !pColor = concat $ replicate 4 [ 1.0, 1.0, 1.0, 0.3 ]

    pTexCoord :: [GLfloat]
    pTexCoord = [ 0.0, 0.0 , 1.0, 0.0 , 1.0, 1.0 , 0.0, 1.0 ] 

-- For 3D --

renderTitleCube :: SimpleShaderProg -> GU.VAO -> [TextureObject] -> IO ()
renderTitleCube sh vao [ftex',rtex',batex',ltex',ttex',botex'] = do
  GL.currentProgram GL.$= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled

  enableTexture sh True
  GU.withVAO vao $ mapM_ (\ (t,i) ->
    GU.withTextures2D [t] $ drawArrays Quads i 4 )
    $ zip [ftex',rtex',batex',ltex',ttex',botex'] [0,4,8,12,16,20]
  GL.currentProgram GL.$= Nothing
  where 
    !shprg' = getShaderProgram sh

genCubeVAO :: SimpleShaderProg  -> IO GU.VAO
genCubeVAO sh = 
  makeSimpShdrVAO sh cubeVert qColor qTexCoord
  where
    qColor :: [GL.GLfloat]
    !qColor = concat $ replicate 24 [ 1.0, 1.0, 1.0, 1.0 ]

    p0,p1,p2,p3,p4,p5,p6,p7 :: [GLfloat]
    !p0 = [ -0.5, -0.5, -0.5]
    !p1 = [  0.5, -0.5, -0.5]
    !p2 = [  0.5, -0.5,  0.5]
    !p3 = [ -0.5, -0.5,  0.5]
    !p4 = [  0.5,  0.5,  0.5]
    !p5 = [ -0.5,  0.5,  0.5]
    !p6 = [ -0.5,  0.5, -0.5]
    !p7 = [  0.5,  0.5, -0.5]

    cubeVert :: [GLfloat]
    !cubeVert = concat 
      [ p6, p7, p1, p0 -- Front
      , p7, p4, p2, p1 -- Right
      , p4, p5, p3, p2 -- Back
      , p5, p6, p0, p3 -- Left
      , p7, p6, p5, p4 -- Top
      , p0, p1, p2 ,p3 -- Bottom
      ]

    qTexCoord :: [GLfloat]
    qTexCoord = concat $ replicate 6 
      [ 0.0, 0.0 , 1.0, 0.0 , 1.0, 1.0 , 0.0, 1.0 ] 
