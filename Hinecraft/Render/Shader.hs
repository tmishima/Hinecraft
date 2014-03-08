{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Shader
  ( ShaderPrgSrc (..)
  , initShaderProg
  , setSun
  , genShaderProg
  )
  where

import Graphics.GLUtil
import Graphics.Rendering.OpenGL as GL
--import qualified Data.ByteString as B
--import Debug.Trace as Dbg

data ShaderPrgSrc = ShaderPrgSrc
  { vSrc :: FilePath 
  , fSrc :: FilePath
  , vattrLoc :: [String]
  , fattrLoc :: [String]
  , uattrLoc :: [String]
  }

initShaderProg home = do
  prg <- genShaderProg sps
  return prg
  where
    sps = ShaderPrgSrc
      { vSrc = home ++ "/.Hinecraft/shader/basic.vert"
      , fSrc = home ++ "/.Hinecraft/shader/basic.frag"
      , vattrLoc = ["VertexPosition","VertexColor"]
      , fattrLoc = ["FragColor"]
      , uattrLoc = ["RotationMatrix"]
      }

-- OpenGL 3.0を前提に初期化
genShaderProg :: ShaderPrgSrc -> IO ShaderProgram
genShaderProg sps =
  simpleShaderProgramWith (vSrc sps) (fSrc sps) $ \ p -> do
    mapM_ (\ (i,s) -> attribLocation p s $= AttribLocation i)
      $ zip [0 .. ] (vattrLoc sps)
    mapM_ (\ (i,s) -> bindFragDataLocation p s $= i) 
      $ zip [0 .. ] (fattrLoc sps)

setSun :: ShaderProgram -> IO VertexArrayObject
setSun shprg = do
  varray <- makeBuffer ArrayBuffer vert
  carray <- makeBuffer ArrayBuffer clr

  vbo <- makeVAO $ do
    bindBuffer ArrayBuffer $= Just varray
    setAttrib shprg "VertexPosition" ToFloat
      (VertexArrayDescriptor 4 Float 0 offset0)
    enableAttrib shprg "VertexPosition"
    
    bindBuffer ArrayBuffer $= Just carray
    setAttrib shprg "VertexColor" ToFloat
      (VertexArrayDescriptor 4 Float 0 offset0)
    enableAttrib shprg "VertexColor"
    --bindBuffer ArrayBuffer $= Nothing
    --bindVertexArrayObject $= Nothing

  return vbo
  where
    vert :: [GLfloat] 
    vert  =
{-      [ 0.0, 100.0, -50,0
      , 00.0, 50.0, -50.0
      , 50.0, 50.0, -50.0
--      , 0.0, 70.0, 50.0 -}
      [ -1.0, -1.0,  0.0, 1.0
      ,  1.0, -1.0,  0.0, 1.0
      ,  0.0,  1.0,  0.0, 1.0
      ]
    clr :: [Color4 GLfloat] 
    clr = [GL.Color4 1.0 0.0 0.0 1.0,
           GL.Color4 0.0 1.0 0.0 1.0,
           GL.Color4 0.0 0.0 1.0 1.0] :: [Color4 GLfloat]
    rot :: [GLfloat]
    rot = [1,0,0,0,0,1,0,0 ,0,0,1,0,0,0,0,1]


