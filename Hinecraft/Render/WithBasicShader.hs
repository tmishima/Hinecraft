{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.WithBasicShader 
  ( BasicShaderProg
  , makeBasicShdrVAO
  , orthoProjMatrix
  , setLightMode
  , setColorBlendMode
  )
  where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3
--import Control.Exception ( bracket )

import Linear

import Hinecraft.Render.Types

data BasicShaderProg = BasicShaderProg
  { shprg :: ShaderProgram
  , inVertTag :: String
  , inVertClrTag :: String
  , inTexTag :: String
  , inVertNormTag :: String
  , outFragTag :: String
  , uniProjViewMTag :: String      
  , uniRotMTag :: String
  , uniScaleMTag :: String
  , uniTexEnFTag :: String
  , uniTexUnitTag :: String
  , uniClrBlndTag :: String
  , uniLightMdTag :: String
  }

instance WithShader BasicShaderProg where
  initShaderProgram home = do
    sp <- simpleShaderProgramWith vertFn fragFn $ \ p -> do
      attribLocation p inVertTag'        $= AttribLocation 0
      attribLocation p inVertClrTag'     $= AttribLocation 1
      attribLocation p inVertNormTag'    $= AttribLocation 2
      attribLocation p inTexTag'         $= AttribLocation 3
      bindFragDataLocation p outFragTag' $= 0

    -- set Defualt param to uniform
    currentProgram $= Just (program sp)

    asUniform pvMat $ getUniform sp uniProjViewMTag' 
    GU.asUniform (GU3.camMatrix (GU3.fpsCamera :: GU3.Camera GLfloat)) (getUniform sp uniRotMTag')
    uniformMat (getUniform sp uniScaleMTag') $= ( [[1.0,0,0],[0,1.0,0],[0,0,1.0]]:: [[GLfloat]])
    uniformScalar (getUniform sp uniTexEnFTag') $= (0 :: GLint)

    currentProgram $= Nothing

    return BasicShaderProg 
             { shprg = sp
             , inVertTag = inVertTag'
             , inVertClrTag = inVertClrTag'
             , inTexTag = inTexTag'
             , inVertNormTag = inVertNormTag'
             , outFragTag = outFragTag'
             , uniProjViewMTag = uniProjViewMTag'
             , uniRotMTag = uniRotMTag'
             , uniScaleMTag = uniScaleMTag'
             , uniTexEnFTag = uniTexEnFTag'
             , uniTexUnitTag = uniTexUnitTag'
             , uniClrBlndTag = uniClrBlndTag'
             , uniLightMdTag = uniLightMdTag'
             }
    where
      !vertFn = home ++ "/.Hinecraft/shader/basic.vert"
      !fragFn = home ++ "/.Hinecraft/shader/basic.frag"
      !inVertTag'        = "VertexPosition"
      !inVertClrTag'     = "VertexColor"
      !inTexTag'         = "VertexTexture"
      !inVertNormTag'    = "VertexNormal"
      !outFragTag'       = "FragColor"
      !uniProjViewMTag'  = "ProjViewMat"
      !uniRotMTag'       = "RotMat" 
      !uniScaleMTag'     = "SclMat"
      !uniTexEnFTag'     = "TexEnbFlg"
      !uniTexUnitTag'    = "TexUnit"
      !uniClrBlndTag'    = "ColorBlandType"
      !uniLightMdTag'    = "LightMode"
      !prjMat = GU3.projectionMatrix (GU3.deg2rad 60) 1.0 0.1 (1000::GLfloat)
      !cam = GU3.camMatrix GU3.fpsCamera
      !pvMat = prjMat !*! cam

  enableTexture shd flg =
    uniformScalar (getUniform s $ uniTexEnFTag shd) $= if flg then 1 else (0 :: GLint)
    where
      !s = shprg shd

  getShaderProgram = shprg 

  setProjViewMat shd pvMat = do
    currentProgram $= Just (program s)
    asUniform pvMat pvUnifLoc 
    currentProgram $= Nothing
    where
      !pvUnifLoc = getUniform s (uniProjViewMTag shd)
      !s = shprg shd

  setCamParam simpShdr cam = do
    currentProgram $= Just (program sp)
    GU.asUniform mat rUnifLoc
    currentProgram $= Nothing
    where
      !mat = GU3.camMatrix cam
      !rUnifLoc = GU.getUniform sp (uniRotMTag simpShdr)
      !sp = shprg simpShdr

orthoProjMatrix :: GLfloat -> GLfloat -> M44 GLfloat
orthoProjMatrix w h = V4 (V4 (2/w) 0 0 (-1)) (V4 0 (2/h) 0 (-1)) (V4 0 0 0 0) (V4 0 0 0 1)

setLightMode :: BasicShaderProg -> Int -> IO ()
setLightMode sh md = 
  uniformScalar (getUniform sp $ uniLightMdTag sh)
    $= (fromIntegral md::GLint)
  where
    sp = shprg sh

setColorBlendMode :: BasicShaderProg -> Int -> IO ()
setColorBlendMode sh md =
  uniformScalar (getUniform sp $ uniClrBlndTag sh)
    $= (fromIntegral md::GLint)
  where
    sp = shprg sh

makeBasicShdrVAO :: BasicShaderProg -> [GLfloat]
                -> [GLfloat] -> [GLfloat] -> [GLfloat] -> IO VAO
makeBasicShdrVAO simpShdr vertLst vertClrLst vertNrmLst texCdLst = do
  currentProgram $= Just (program sp)
  vao <- makeVAO $ do
    makeBuffer ArrayBuffer vertLst
    enableAttrib sp (inVertTag simpShdr)
    setAttrib sp (inVertTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    makeBuffer ArrayBuffer vertClrLst 
    enableAttrib sp (inVertClrTag simpShdr)
    setAttrib sp (inVertClrTag simpShdr) ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
   
    makeBuffer ArrayBuffer vertNrmLst 
    enableAttrib sp (inVertNormTag simpShdr)
    setAttrib sp (inVertNormTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    makeBuffer ArrayBuffer texCdLst
    enableAttrib sp (inTexTag simpShdr)
    setAttrib sp (inTexTag simpShdr) ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

  GL.currentProgram $= Nothing
  return vao
  where
    sp = shprg simpShdr

