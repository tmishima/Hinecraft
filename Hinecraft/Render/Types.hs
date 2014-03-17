--{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Types where

-- OpenGL
import Graphics.Rendering.OpenGL
import Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3

import Linear

-- Font
import Graphics.Rendering.FTGL as Ft

type VrtxPos3D = (GLfloat,GLfloat,GLfloat)
type VrtxPos2D = (GLfloat,GLfloat)

class WithShader a where
  initShaderProgram :: FilePath -> IO a
  setProjViewMat :: a -> M44 GLfloat -> IO ()
  getShaderProgram :: a -> ShaderProgram
  setCamParam :: a -> GU3.Camera GLfloat -> IO ()
  enableTexture :: a -> Bool -> IO ()

data GuiResource = GuiResource
  { widgetsTexture :: TextureObject
  , widgetPlayBtnPos :: VrtxPos2D
  , widgetPlayBtnSiz :: VrtxPos2D
  , widgetExitBtnPos :: VrtxPos2D
  , widgetExitBtnSiz :: VrtxPos2D
  , font :: Ft.Font
  , invDlgTexture :: TextureObject
  , invDlgTbTexture :: TextureObject
  }

data WorldResource = WorldResource
  { blockTexture :: TextureObject
  --, skyTexture :: 
  --, starTexture :: 
  }
  deriving (Eq,Show)




