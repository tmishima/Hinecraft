{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.TitleView
  ( TitleModeHdl
  , initTitleModeView
  -- 
  , drawBackGroundBox
  )
  where

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3

import Hinecraft.Types
import Hinecraft.Render.Types
import Hinecraft.Render.WithSimpleShader

data TitleModeHdl = TitleModeHdl
  { shader  :: SimpleShaderProg
  , cubeVAO :: GU.VAO
  , platVAO :: GU.VAO
  }

initTitleModeView :: FilePath -> IO TitleModeHdl
initTitleModeView home = do
  sh <- initShaderProgram home
  cvao <- genCubeVAO sh
  pvao <- genPlateVAO sh
  return TitleModeHdl
    { shader = sh
    , cubeVAO = cvao
    , platVAO = pvao
    }

drawBackGroundBox :: (Int,Int) -> GuiResource -> TitleModeState
                  -> TitleModeHdl -> IO ()
drawBackGroundBox (w,h) res stat tvHdl = do
  -- Draw 3D cube
  let cam = GU3.pan (realToFrac $ rotW stat) $ GU3.tilt (10::GLfloat) GU3.fpsCamera
      prjMat = GU3.projectionMatrix (GU3.deg2rad 90) (fromIntegral w/ fromIntegral h) 0.1 (10::GLfloat)
  setProjViewMat sh prjMat
  set3DCamParam sh cam
  GU.withViewport (Position 0 0) (Size (fromIntegral w) (fromIntegral h))
    $ renderTitleCube sh vao bkgTex 

  -- Draw 2D Title




  where
    bkgTex = backgroundBoxTexture res
    sh = shader tvHdl
    vao = cubeVAO tvHdl

-- For 2D

genPlateVAO :: SimpleShaderProg -> IO GU.VAO
genPlateVAO sh =
  makeSimpShdrVAO sh platVert pColor pTexCoord platElem
  where
    platVert :: [GLfloat]
    !platVert = concat [p0,p1,p2,p3]

    p0,p1,p2,p3 :: [GLfloat]
    !p0 = [ -0.5,  0.0, -0.5]
    !p1 = [  0.5,  0.0, -0.5]
    !p2 = [  0.5,  0.0,  0.5]
    !p3 = [ -0.5,  0.0,  0.5]

    pColor :: [GLfloat]
    !pColor = concat $ replicate 4 [ 1.0, 1.0, 1.0, 1.0 ]

    platElem :: [GU.Word32]
    !platElem = [0 .. 3]

    pTexCoord :: [GLfloat]
    pTexCoord = [ 0.0, 0.0 , 1.0, 0.0 , 1.0, 1.0 , 0.0, 1.0 ] 


-- For 3D --

renderTitleCube :: SimpleShaderProg -> GU.VAO -> [TextureObject] -> IO ()
renderTitleCube sh vao [ftex',rtex',batex',ltex',ttex',botex'] = do
  GL.currentProgram GL.$= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled

  GU.withVAO vao $ mapM_ (\ (t,elst) ->
    GU.withTextures2D [t] $ 
      drawElements Quads (fromIntegral $ length elst) UnsignedInt $ GU.offsetPtr $ fromIntegral (head elst * 4))
    $ zip [ftex',rtex',batex',ltex',ttex',botex'] ([[0..3],[4..7],[8..11],[12..15],[16..19],[20..23]] :: [[GU.Word32]])
  GL.currentProgram GL.$= Nothing
  where 
    !shprg' = getShaderProgram sh

genCubeVAO :: SimpleShaderProg  -> IO GU.VAO
genCubeVAO sh = 
  makeSimpShdrVAO sh cubeVert qColor qTexCoord cubeElem
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

    cubeElem :: [GU.Word32]
    !cubeElem = [0 .. 23]

    qTexCoord :: [GLfloat]
    qTexCoord = concat $ replicate 6 
      [ 0.0, 0.0 , 1.0, 0.0 , 1.0, 1.0 , 0.0, 1.0 ] 
