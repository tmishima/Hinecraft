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
  , setModelMatrix
  , setMVPMatrix
  , setTMatrix
  , setShadowSW
  , setTextureUnitUniform
  )
  where

-- OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil as GU
--import qualified Graphics.GLUtil.Camera3D as GU3
--import Control.Exception ( bracket )

import Linear.V4
import Linear

import Hinecraft.Render.Types

data BasicShaderProg = BasicShaderProg
  { shprg :: ShaderProgram
  , inVertTag :: String
  , inVertClrTag :: String
  , inTexTag :: String
  , inVertNormTag :: String
  , outFragTag :: String
  , uniMMatTag :: String      
  , uniMVPMTag :: String
  , uniTMatTag :: String
  , uniTexEnFTag :: String
  , uniClrBlndTag :: String
  , uniLightMdTag :: String
  , uniShadwMdTag :: String
  , uniTexUnitTags :: [String]
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

    asUniform eMat $ getUniform sp uniMMatrixTag' 
    asUniform eMat $ getUniform sp uniMvpMatrixTag'
    asUniform eMat $ getUniform sp uniTMatrixTag' 
    uniformScalar (getUniform sp uniTexEnFTag') $= (0 :: GLint)

    uniformScalar (getUniform sp (uniTexUnitTags' !! 0)) $= (0 :: GLint)
    uniformScalar (getUniform sp (uniTexUnitTags' !! 1)) $= (1 :: GLint)

    currentProgram $= Nothing

    return BasicShaderProg 
             { shprg = sp
             , inVertTag = inVertTag'
             , inVertClrTag = inVertClrTag'
             , inTexTag = inTexTag'
             , inVertNormTag = inVertNormTag'
             , outFragTag = outFragTag'
             , uniMMatTag = uniMMatrixTag'
             , uniMVPMTag = uniMvpMatrixTag'
             , uniTMatTag = uniTMatrixTag'
             , uniTexEnFTag = uniTexEnFTag'
             , uniClrBlndTag = "ColorBlandType"
             , uniLightMdTag = "LightMode"
             , uniShadwMdTag = "ShadowSW"
             , uniTexUnitTags = uniTexUnitTags'
             }
    where
      !vertFn = home ++ "/.Hinecraft/shader/basic.vert"
      !fragFn = home ++ "/.Hinecraft/shader/basic.frag"
      !inVertTag'        = "VertexPosition"
      !inVertClrTag'     = "VertexColor"
      !inTexTag'         = "VertexTexture"
      !inVertNormTag'    = "VertexNormal"
      !outFragTag'       = "FragColor"
      !uniMMatrixTag'    = "mMatrix"
      !uniMvpMatrixTag'  = "mvpMatrix" 
      !uniTMatrixTag'    = "tMatrix"
      !uniTexEnFTag'     = "TexEnbFlg"
      !uniTexUnitTags'   = ["TexUnit", "ShadowMap"]
      !eMat =  V4 (V4 1.0 0.0 0.0 0.0)
                  (V4 0.0 1.0 0.0 0.0)
                  (V4 0.0 0.0 1.0 0.0)
                  (V4 0.0 0.0 0.0 1.0) :: M44 GLfloat

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

setModelMatrix :: BasicShaderProg -> M44 GLfloat -> IO ()
setModelMatrix shd mMat = asUniform mMat mMUnifLoc 
  where
    !mMUnifLoc = getUniform s (uniMMatTag shd)
    !s = shprg shd

setMVPMatrix :: BasicShaderProg -> M44 GLfloat -> IO ()
setMVPMatrix shd mvpMat = asUniform mvpMat mvpMUnifLoc
  where
    !mvpMUnifLoc = GU.getUniform sp (uniMVPMTag shd)
    !sp = shprg shd

setTMatrix :: BasicShaderProg -> M44 GLfloat -> IO ()
setTMatrix shd tMat = asUniform tMat tMUnifLoc
  where
    !tMUnifLoc = GU.getUniform sp (uniTMatTag shd)
    !sp = shprg shd

orthoProjMatrix :: GLfloat -> GLfloat -> M44 GLfloat
orthoProjMatrix w h = V4 (V4 (2/w) 0 0 (-1)) (V4 0 (2/h) 0 (-1)) (V4 0 0 0 0) (V4 0 0 0 1)

setLightMode :: BasicShaderProg -> Int -> IO ()
setLightMode sh md = 
  uniformScalar (getUniform sp $ uniLightMdTag sh)
    $= (fromIntegral md::GLint)
  where
    sp = shprg sh

setShadowSW :: BasicShaderProg -> Int -> IO ()
setShadowSW sh md = 
  uniformScalar (getUniform sp $ uniShadwMdTag sh)
    $= (fromIntegral md::GLint)
  where
    sp = shprg sh

setTextureUnitUniform :: BasicShaderProg -> IO ()
setTextureUnitUniform sh = do
  asUniform (0::GLint) tunit0
  asUniform (1::GLint) tunit1
  where
    sp = shprg sh
    [tTag,sTag] = uniTexUnitTags sh
    tunit0 = getUniform sp tTag
    tunit1 = getUniform sp sTag

setColorBlendMode :: BasicShaderProg -> Int -> IO ()
setColorBlendMode sh md =
  uniformScalar (getUniform sp $ uniClrBlndTag sh)
    $= (fromIntegral md::GLint)
  where
    sp = shprg sh

makeBasicShdrVAO :: BasicShaderProg -> [GLfloat]
                -> [GLfloat] -> [GLfloat] -> [GLfloat] -> IO VAO
makeBasicShdrVAO simpShdr vertLst vertClrLst vertNrmLst texCdLst = do
  op <- get currentProgram
  currentProgram $= Just (program sp)
  vb <- makeBuffer ArrayBuffer vertLst
  cb <- makeBuffer ArrayBuffer vertClrLst 
  nb <- makeBuffer ArrayBuffer vertNrmLst 
  tb <- makeBuffer ArrayBuffer texCdLst
  vao <- makeVAO $ do
    bindBuffer ArrayBuffer $= Just vb
    enableAttrib sp (inVertTag simpShdr)
    setAttrib sp (inVertTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    bindBuffer ArrayBuffer $= Just cb
    enableAttrib sp (inVertClrTag simpShdr)
    setAttrib sp (inVertClrTag simpShdr) ToFloat (VertexArrayDescriptor 4 Float 0 offset0)
   
    bindBuffer ArrayBuffer $= Just nb
    enableAttrib sp (inVertNormTag simpShdr)
    setAttrib sp (inVertNormTag simpShdr) ToFloat (VertexArrayDescriptor 3 Float 0 offset0)

    bindBuffer ArrayBuffer $= Just tb
    enableAttrib sp (inTexTag simpShdr)
    setAttrib sp (inTexTag simpShdr) ToFloat (VertexArrayDescriptor 2 Float 0 offset0)

  deleteObjectNames [vb,cb,nb,tb]
  
  GL.currentProgram $= op
  return vao
  where
    sp = shprg simpShdr

