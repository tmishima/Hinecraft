{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Rendering.WithSimpleShader 
  ( SimpleShaderProg
  , makeSimpShdrVAO
  , orthoProjMatrix
  , setProjViewMat
  , setCamParam
  )
  where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3
--import Control.Exception ( bracket )

import Linear

import Hinecraft.Rendering.Types

data SimpleShaderProg = SimpleShaderProg
  { shprg :: ShaderProgram
  , inVertTag :: String
  , inVertClrTag :: String
  , inTexTag :: String
  , outFragTag :: String
  , uniProjViewMTag :: String      
  , uniRotMTag :: String
  , uniScaleMTag :: String
  , uniTexEnFTag :: String
  , uniTexUnitTag :: String
  }

instance WithShader SimpleShaderProg where
  initShaderProgram home = do
    sp <- simpleShaderProgramWith vertFn fragFn $ \ p -> do
      attribLocation p inVertTag' $= AttribLocation 0
      attribLocation p inVertClrTag' $= AttribLocation 1
      attribLocation p inTexTag' $= AttribLocation 2
      bindFragDataLocation p outFragTag' $= 0

    -- set Defualt param to uniform
    currentProgram $= Just (program sp)

    asUniform pvMat $ getUniform sp uniProjViewMTag' 
    GU.asUniform (GU3.camMatrix (GU3.fpsCamera :: GU3.Camera GLfloat)) (getUniform sp uniRotMTag')
    uniformMat (getUniform sp uniScaleMTag') $= ( [[1.0,0,0],[0,1.0,0],[0,0,1.0]]:: [[GLfloat]])
    uniformScalar (getUniform sp uniTexEnFTag') $= (0 :: GLint)

    currentProgram $= Nothing

    return SimpleShaderProg 
             { shprg = sp
             , inVertTag = inVertTag'
             , inVertClrTag = inVertClrTag'
             , inTexTag = inTexTag'
             , outFragTag = outFragTag'
             , uniProjViewMTag = uniProjViewMTag'
             , uniRotMTag = uniRotMTag'
             , uniScaleMTag = uniScaleMTag'
             , uniTexEnFTag = uniTexEnFTag'
             , uniTexUnitTag = uniTexUnitTag'
             }
    where
      !vertFn = home ++ "/.Hinecraft/shader/simple.vert"
      !fragFn = home ++ "/.Hinecraft/shader/simple.frag"
      !inVertTag'        = "VertexPosition"
      !inVertClrTag'     = "VertexColor"
      !inTexTag'         = "VertexTexture"
      !outFragTag'       = "FragColor"
      !uniProjViewMTag'  = "ProjViewMat"
      !uniRotMTag'       = "RotMat" 
      !uniScaleMTag'     = "SclMat"
      !uniTexEnFTag'     = "TexEnbFlg"
      !uniTexUnitTag'    = "TexUnit"
      !prjMat = GU3.projectionMatrix (GU3.deg2rad 60) 1.0 0.1 (10::GLfloat)
      !cam = GU3.camMatrix GU3.fpsCamera
      !pvMat = prjMat !*! cam

  useShader shd fn = do
    op <- get currentProgram
    currentProgram $= Just (program sp)
    fn shd
    currentProgram $= op
    where sp = shprg shd

  enableTexture shd flg =
    uniformScalar (getUniform s $ uniTexEnFTag shd) $= if flg then 1 else (0 :: GLint)
    where
      !s = shprg shd

  getShaderProgram = shprg 

orthoProjMatrix :: GLfloat -> GLfloat -> M44 GLfloat
orthoProjMatrix w h = V4 (V4 (2/w) 0 0 (-1)) (V4 0 (2/h) 0 (-1)) (V4 0 0 0 0) (V4 0 0 0 1)

setProjViewMat :: SimpleShaderProg -> M44 GLfloat -> IO ()
setProjViewMat shd pvMat = asUniform pvMat pvUnifLoc 
  where
    !pvUnifLoc = getUniform s (uniProjViewMTag shd)
    !s = shprg shd

setCamParam :: SimpleShaderProg -> GU3.Camera GLfloat -> IO ()
setCamParam simpShdr cam = GU.asUniform mat rUnifLoc
  where
    !mat = GU3.camMatrix cam
    !rUnifLoc = GU.getUniform sp (uniRotMTag simpShdr)
    !sp = shprg simpShdr

makeSimpShdrVAO :: SimpleShaderProg -> [GLfloat]
                -> [GLfloat] -> [GLfloat] -> IO VAO
makeSimpShdrVAO simpShdr vertLst vertClrLst texCdLst = do
  op <- get currentProgram
  currentProgram $= Just (program sp)
  vao <- makeVAO $ do
    _ <- makeBuffer ArrayBuffer vertLst
    enableAttrib sp (inVertTag simpShdr)
    setAttrib sp (inVertTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    _ <- makeBuffer ArrayBuffer vertClrLst 
    enableAttrib sp (inVertClrTag simpShdr)
    setAttrib sp (inVertClrTag simpShdr) ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
    
    _ <- makeBuffer ArrayBuffer texCdLst
    enableAttrib sp (inTexTag simpShdr)
    setAttrib sp (inTexTag simpShdr) ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

  GL.currentProgram $= op
  return vao
  where
    sp = shprg simpShdr

