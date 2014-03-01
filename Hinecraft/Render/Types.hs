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
  { backgroundBoxTexture :: [GLuint]
  , backgroundTitleTexture :: GLuint
  , widgetsTexture :: GLuint
  , widgetPlayBtnPos :: VrtxPos2D
  , widgetPlayBtnSiz :: VrtxPos2D
  , widgetExitBtnPos :: VrtxPos2D
  , widgetExitBtnSiz :: VrtxPos2D
  , font :: Ft.Font
  , invDlgTexture :: GLuint
  , invDlgTbTexture :: GLuint
  }

data WorldResource = WorldResource
  { blockTexture :: GLuint
  --, skyTexture :: GLuint
  --, starTexture :: GLuint
  }
  deriving (Eq,Show)




