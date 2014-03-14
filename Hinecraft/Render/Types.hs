--{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Types where

-- OpenGL
import Graphics.Rendering.OpenGL

-- Font
import Graphics.Rendering.FTGL as Ft

type VrtxPos3D = (GLfloat,GLfloat,GLfloat)
type VrtxPos2D = (GLfloat,GLfloat)

data GuiResource = GuiResource
  { backgroundBoxTexture :: [TextureObject]
  , backgroundTitleTexture :: TextureObject
  , widgetsTexture :: TextureObject
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




