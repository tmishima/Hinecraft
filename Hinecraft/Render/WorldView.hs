{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.WorldView
  ( WorldViewVHdl
  , initWorldView
  , drawWorldView
  , renderEnvCube
  , genChunkVAO
  )
  where

--import Data.Tree 

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3
import qualified Graphics.GLUtil.Camera2D as GU2
--import Linear ( M44 ) --  eye4
import Linear.V3

import Hinecraft.Types
import Hinecraft.Render.Util
import Hinecraft.Render.Types
import Hinecraft.Render.WithBasicShader as BSd
import Hinecraft.Render.WithSimpleShader as SSd

data WorldViewVHdl = WorldViewVHdl 
  { basicShader :: BasicShaderProg
  , simpleShader :: SimpleShaderProg
  , envCube :: GU.VAO
  , blockTexture' :: TextureObject
  , skyTexture :: TextureObject
  --, starTexture ::
  }
{- 
  , whitePlatVAO :: GU.VAO
  , titlePlatVAO :: [GU.VAO]
  , startBtnVAO :: GU.VAO
  , quitBtnVAO :: GU.VAO
  , envCubeTex :: [TextureObject]
-}

initWorldView :: FilePath -> IO WorldViewVHdl
initWorldView home = do
  bsh <- initShaderProgram home
  ssh <- initShaderProgram home
  blks <- loadTexture' blkPng
  sky <- loadTexture' skyPng
  cb <- genEnvCubeVAO bsh
  return WorldViewVHdl 
    { basicShader = bsh
    , simpleShader = ssh
    , envCube = cb
    , blockTexture' = blks
    , skyTexture = sky
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"
    skyPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/cloud1.png"
    --starPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/star1.png"

drawWorldView :: WorldViewVHdl -> (Int,Int) -> UserStatus -> IO ()
drawWorldView wvhdl (w,h) usrStat' = 
  GU.withViewport (Position 0 0) (Size (fromIntegral w) (fromIntegral h)) $ do
    -- Draw 3D cube
    let prjMat = GU3.projectionMatrix (GU3.deg2rad 60) (fromIntegral w/ fromIntegral h) 0.1 (2000::GLfloat)
    setProjViewMat pg prjMat
    setCamParam pg $ GU3.dolly (V3 0 (-uy - 1.5) 0)
                   $ GU3.pan ury $ GU3.tilt urx GU3.fpsCamera

    renderEnvCube pg cbVao skyTex 
  where
    pg = basicShader wvhdl
    cbVao = envCube wvhdl
    skyTex = skyTexture wvhdl
    d2f (a,b,c) = (realToFrac a, realToFrac b, realToFrac c)
    (ux,uy,uz) = d2f $ userPos usrStat'
    (urx,ury,_) = d2f $ userRot usrStat' :: (GLfloat,GLfloat,GLfloat)

genChunkVAO :: BasicShaderProg -> SurfacePos -> IO GU.VAO
genChunkVAO sh bsf =
  makeBasicShdrVAO sh vertlst clrlst normlst coordlst
  where
    vertlst = undefined
    clrlst = undefined
    normlst = undefined
    coordlst = undefined
  --mapM_ genFace bsf

renderEnvCube :: BasicShaderProg -> GU.VAO -> TextureObject -> IO ()
renderEnvCube sh vao tex = do 
  currentProgram $= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled
  setLightMode sh 0
  setColorBlendMode sh 1
  enableTexture sh True
  GU.withVAO vao $ 
    GU.withTextures2D [tex] $ drawArrays Quads 0 24

  currentProgram $= Nothing
  where 
    !shprg' = getShaderProgram sh

genEnvCubeVAO :: BasicShaderProg  -> IO GU.VAO
genEnvCubeVAO sh = 
  makeBasicShdrVAO sh cubeVert qColor qnorm qTexCoord 
  where
    qColor :: [GL.GLfloat]
    qColor = concat $ replicate 24 [ (180/255),(226/255),(255/255), 1.0 ]

    p0,p1,p2,p3,p4,p5,p6,p7 :: [GLfloat]
    {-
    !p0 = [ -0.5, -0.25, -0.5]
    !p1 = [  0.5, -0.25, -0.5]
    !p2 = [  0.5, -0.25,  0.5]
    !p3 = [ -0.5, -0.25,  0.5]
    -} 
    !p0 = [ -0.5, -0.4, -0.5]
    !p1 = [  0.5, -0.4, -0.5]
    !p2 = [  0.5, -0.4,  0.5]
    !p3 = [ -0.5, -0.4,  0.5]
    !p4 = [  0.5,  0.5,  0.5]
    !p5 = [ -0.5,  0.5,  0.5]
    !p6 = [ -0.5,  0.5, -0.5]
    !p7 = [  0.5,  0.5, -0.5]

    cubeVert :: [GLfloat]
    !cubeVert = map (* 2000) $ concat
      [ p6, p7, p1, p0 -- Front
      , p7, p4, p2, p1 -- Right
      , p4, p5, p3, p2 -- Back
      , p5, p6, p0, p3 -- Left
      , p7, p6, p5, p4 -- Top
      , p0, p1, p2 ,p3 -- Bottom
      ]

    qTexCoord :: [GLfloat]
    !qTexCoord =  
      [ 1/3, 1/2 , 2/3, 1/2 , 2/3, 1.0 , 1/3, 1.0
      , 2/3, 1/2 , 1.0, 1/2 , 1.0, 1.0 , 2/3, 1.0
      , 2/3, 0.0 , 1.0, 0.0 , 1.0, 1/2 , 2/3, 1/2
      , 0.0, 1/2 , 1/3, 1/2 , 1/3, 1.0 , 0.0, 1.0
      , 2/3, 1/2 , 1/3, 1/2 , 1/3, 0.0 , 2/3, 0.0
      , 0.0, 0.0 , 1/3, 0.0 , 1/3, 1/2 , 0.0, 1/2
      ]

    qnorm = [ 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            ]
