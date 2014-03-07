{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Shader
  ( ShaderPrgSrc (..)
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
  }

genShaderProg :: ShaderPrgSrc -> IO ShaderProgram
genShaderProg sps = 
  simpleShaderExplicit (vSrc sps) (fSrc sps)
    (vattrLoc sps,[])
   -- (vattrLoc sps,["RotationMatrix"])



setSun :: IO VertexArrayObject
setSun = do
  varray <- makeBuffer ArrayBuffer vert
  carray <- makeBuffer ArrayBuffer clr

  vbo <- makeVAO $ do
      bindBuffer ArrayBuffer $= Just varray
      vertexAttribPointer (AttribLocation 0) $=
            (ToFloat, VertexArrayDescriptor 4 Float 0 offset0)
    
      vertexAttribArray (AttribLocation 0) $= Enabled
    
      bindBuffer ArrayBuffer $= Just carray
      vertexAttribPointer (AttribLocation 1) $=
            (ToFloat, VertexArrayDescriptor 4 Float 0 offset0)
    
      vertexAttribArray (AttribLocation 1) $= Enabled

      bindBuffer ArrayBuffer $= Nothing
      bindVertexArrayObject $= Nothing

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



