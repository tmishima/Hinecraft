{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.WithSimpleShader 
  ( SimpleShaderProg
  , makeSimpShdrVAO
  , set3DCamParam
  , WithShader ( .. )
  )
  where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3

import Linear

class WithShader a where
  initShaderProgram :: FilePath -> IO a
  setProjViewMat :: a -> M44 GLfloat -> IO ()
  getShaderProgram :: a -> ShaderProgram

data SimpleShaderProg = SimpleShaderProg
  { shprg :: ShaderProgram
  , inVertTag :: String
  , inVertClrTag :: String
  , inTexTag :: String
  , outFragTag :: String
  , uniProjViewMTag :: String      
  , uniRotMTag :: String
  , uniScaleMTag :: String
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
      !prjMat = GU3.projectionMatrix (GU3.deg2rad 60) 1.0 0.1 (10::GLfloat)
      !cam = GU3.camMatrix GU3.fpsCamera
      !pvMat = prjMat !*! cam

  getShaderProgram = shprg 

  setProjViewMat shd pvMat = do
    currentProgram $= Just (program s)
    asUniform pvMat pvUnifLoc 
    currentProgram $= Nothing
    where
      !pvUnifLoc = getUniform s (uniProjViewMTag shd)
      !s = shprg shd


set3DCamParam :: SimpleShaderProg -> GU3.Camera GLfloat -> IO ()
set3DCamParam simpShdr cam = do
  currentProgram $= Just (program sp)
  GU.asUniform mat rUnifLoc
  currentProgram $= Nothing
  where
    !mat = GU3.camMatrix cam
    !rUnifLoc = GU.getUniform sp (uniRotMTag simpShdr)
    !sp = shprg simpShdr

makeSimpShdrVAO :: SimpleShaderProg -> [GLfloat]
                -> [GLfloat] -> [GLfloat] -> [Word32]->IO VAO
makeSimpShdrVAO simpShdr vertLst vertClrLst texCdLst elmLst = do
  currentProgram $= Just (program sp)
  vao <- makeVAO $ do
    makeBuffer ArrayBuffer vertLst
    enableAttrib sp (inVertTag simpShdr)
    setAttrib sp (inVertTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    bufferIndices elmLst

    makeBuffer ArrayBuffer vertClrLst 
    enableAttrib sp (inVertClrTag simpShdr)
    setAttrib sp (inVertClrTag simpShdr) ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
    
    makeBuffer ArrayBuffer texCdLst
    enableAttrib sp (inTexTag simpShdr)
    setAttrib sp (inTexTag simpShdr) ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

  GL.currentProgram $= Nothing
  return vao
  where
    sp = shprg simpShdr

