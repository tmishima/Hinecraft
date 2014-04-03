{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Util
  ( blockNodeVertex
  , drawBackPlane
  , putTextLine
  , drawIcon
  , loadTexture
  , getVertexList
  )
  where

import Graphics.Rendering.OpenGL 
import Graphics.Rendering.OpenGL.Raw
import Control.Monad ( void )
--import Debug.Trace as Dbg

import qualified Graphics.GLUtil as GU

-- Font
import Graphics.Rendering.FTGL as Ft

import Hinecraft.Render.Types
import Hinecraft.Types
import Hinecraft.Model

blockNodeVertex :: [VrtxPos3D]
blockNodeVertex = 
  [ ( -0.5, -0.5, -0.5) -- P0
  , (  0.5, -0.5, -0.5) -- P1
  , (  0.5, -0.5,  0.5) -- P2
  , ( -0.5, -0.5,  0.5) -- P3
  , (  0.5,  0.5,  0.5) -- P4
  , ( -0.5,  0.5,  0.5) -- P5
  , ( -0.5,  0.5, -0.5) -- P6
  , (  0.5,  0.5, -0.5) -- P7
  , (  0.5,  0.0,  0.5) -- P8
  , ( -0.5,  0.0,  0.5) -- P9
  , ( -0.5,  0.0, -0.5) -- P10
  , (  0.5,  0.0, -0.5) -- P11
  ]

loadTexture :: FilePath -> IO TextureObject
loadTexture fn = do
  t <- GU.readTexture fn
  case t of
    Right t' -> do
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
        >> GU.texture2DWrap $= (Repeated, ClampToEdge)
      return t'
    Left e -> error e

getVertexList :: Shape -> Surface
              -> ([VrtxPos3D],[(GLfloat,GLfloat)])
getVertexList Cube f = case f of
  STop    -> ([p7,p6,p5,p4],uvLst)
  SBottom -> ([p0,p1,p2,p3],uvLst)
  SFront  -> ([p6,p7,p1,p0],uvLst)
  SBack   -> ([p4,p5,p3,p2],uvLst)
  SRight  -> ([p7,p4,p2,p1],uvLst)
  SLeft   -> ([p5,p6,p0,p3],uvLst)
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:_) = blockNodeVertex
    !uvLst = [ (0.0,0.0)
             , (1.0,0.0)
             , (1.0,1.0)
             , (0.0,1.0)
             ]
getVertexList (Half b) f 
  | not b = case f of -- False
    STop    -> ([p11,p10,p9,p8],uvLstD)
    SBottom -> ([p0,p1,p2,p3],uvLstD)
    SFront  -> ([p10,p11,p1,p0],uvLstD)
    SBack   -> ([p8,p9,p3,p2],uvLstD)
    SRight  -> ([p11,p8,p2,p1],uvLstD)
    SLeft   -> ([p9,p10,p0,p3],uvLstD)
  | otherwise = case f of
    STop    -> ([p7,p6,p5,p4],uvLstU)
    SBottom -> ([p10,p11,p8,p9],uvLstU)
    SFront  -> ([p6,p7,p11,p10],uvLstU)
    SBack   -> ([p4,p5,p9,p8],uvLstU)
    SRight  -> ([p7,p4,p8,p11],uvLstU)
    SLeft   -> ([p5,p6,p10,p9],uvLstU)
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:p8:p9:p10:p11:_) = blockNodeVertex
    uvLstU = [uv0,uv1,uv2,uv3]
    uvLstD = [uv3,uv2,uv4,uv5]
    (uv0,uv1,uv2,uv3,uv4,uv5)
      = ((0.0,0.0)
        ,(1.0,0.0)
        ,(1.0,0.5)
        ,(0.0,0.5)
        ,(1.0,1.0)
        ,(0.0,1.0))

drawBackPlane :: VrtxPos2D -> (GLfloat,GLfloat)
              -> Maybe TextureObject -> (GLfloat,GLfloat) -> (GLfloat,GLfloat)
              -> (GLfloat,GLfloat,GLfloat,GLfloat) -> IO ()
drawBackPlane (xo,yo) (w,h) tex' (u0,v0) (tw,th) (r,g,b,a) =
  preservingMatrix $ do
    color $ Color4 r g b a 
    case tex' of
      Just t -> do
        texture Texture2D $= Enabled 
        textureBinding Texture2D $= Just t
        renderPrimitive Quads $ do
          glTexCoord2f u0 (v0 + th)
          vertex $ Vertex2 xo  (yo::GLfloat)
          glTexCoord2f (u0 + tw) (v0 + th)
          vertex $ Vertex2 (xo + w) (yo::GLfloat)
          glTexCoord2f (u0 + tw) v0
          vertex $ Vertex2 (xo + w) (yo + h)
          glTexCoord2f u0 v0
          vertex $ Vertex2 xo  (yo + h)
      Nothing -> do
        texture Texture2D $= Disabled
        renderPrimitive Quads $ do
          vertex $ Vertex2 xo  (yo::GLfloat)
          vertex $ Vertex2 (xo + w) (yo::GLfloat)
          vertex $ Vertex2 (xo + w) (yo + h)
          vertex $ Vertex2 xo  (yo + h)

putTextLine :: Ft.Font -> Maybe (GLfloat,GLfloat,GLfloat)
            -> Maybe Int -> VrtxPos2D -> String -> IO ()
putTextLine ft cl sz (x,y) str = preservingMatrix $ do
  texture Texture2D $= Disabled
  case cl of
    Just (r,g,b) -> color $ Color4 r g b 1.0
    _ -> return ()
  case sz of
    Just s -> void $ Ft.setFontFaceSize ft s 72 
    _ -> return ()
  rasterPos $ Vertex2 x y
  Ft.renderFont ft str Ft.Front

drawIcon :: TextureObject -> GLfloat -> (GLfloat,GLfloat)
         -> BlockIDNum -> IO ()
drawIcon tex icSz (ox,oy) bID | null texIdx = return ()
                                 | otherwise = preservingMatrix $ do
  texture Texture2D $= Enabled 
  textureBinding Texture2D $= Just tex

  let ![tt,_,_,tl,tf,_] = texIdx
  renderPrimitive Quads $ do
    -- top 
    setColor 1.3 ct
    mapM_ drawf $ zip (calcUV tt)
      [ (ox, oy + icSzH)
      , (ox + icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox, oy + icSzH + icSzW * (sin.d2r) 2 * rt)
      , (ox - icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      ]
    -- left 
    setColor 1.0 cl 
    mapM_ drawf $ zip (calcUV tl)
      [ (ox, oy + icSzH)
      , (ox + icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox + icSzW, oy + icSzW * (sin.d2r) rt)
      , (ox, oy)
      ]
    -- front 
    setColor 1.0 cf 
    mapM_ drawf $ zip (calcUV tf)
      [ (ox - icSzW, oy + icSzH + icSzW * (sin.d2r) rt)
      , (ox, oy + icSzH)
      , (ox, oy)
      , (ox - icSzW, oy + icSzW * (sin.d2r) rt)
      ]
  where
    !icSzW = icSz / 2.2
    !icSzH = case shape $ getBlockInfo bID of
              Cube -> icSzW
              Half _ -> icSzW / 2.0
              _ -> icSzW 
    !rt = 30.0
    d2r d = pi * d / 180.0
    !texIdx = textureIndex $ getBlockInfo bID
    ![ct,_,_,cl,cf,_] = bcolor $ getBlockInfo bID
    setColor :: Double -> (Double,Double,Double) -> IO () 
    setColor c (r,g,b) =
      color $ Color3 (realToFrac (c * r)) (realToFrac (c * g))
                     (realToFrac (c * b) :: GLfloat) 
    calcUV (i',j') = [ ( i * (1/16), j * (1/16)) 
                     , ( (i + 1) * (1/16), j * (1/16))
                     , ( (i + 1) * (1/16), (j + 1) * (1/16))
                     , ( i * (1/16), (j + 1) * (1/16))
                     ]
      where i = fromIntegral i' ; j = fromIntegral j'
    drawf ((u,v),(x,y)) = do
      glTexCoord2f u v
      vertex $ Vertex2 x y


