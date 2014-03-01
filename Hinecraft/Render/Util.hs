{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Util
  ( loadTextures
  , blockNodeVertex
  , getVertexList
  , drawBackPlane
  , putTextLine
  , drawIcon
  )
  where

import Graphics.Rendering.OpenGL 
import Graphics.Rendering.OpenGL.Raw
import Control.Monad ( void )
import Foreign ( withForeignPtr, plusPtr, alloca, peek )
import qualified Codec.Picture as CdP
import qualified Data.Vector.Storable as Vct
import Debug.Trace as Dbg

-- Font
import Graphics.Rendering.FTGL as Ft

import Hinecraft.Render.Types
import Hinecraft.Types
import Hinecraft.Model

loadTextures :: FilePath -> IO GLuint
loadTextures path = do
  Dbg.traceIO path
  Just (w,h,(ptr,off,_),t) <- rdImg
  Dbg.traceIO $ unwords ["Image w = ", show w, "Image h = ", show h]
  tex <- alloca $ \p -> do
            glGenTextures 1 p
            peek p
  _ <- withForeignPtr ptr $ \p -> do
    let p' = p `plusPtr` off
        glNearest  = fromIntegral gl_NEAREST
    -- create linear filtered texture
    glBindTexture gl_TEXTURE_2D tex
    glTexImage2D gl_TEXTURE_2D 0 4
      (fromIntegral w) (fromIntegral h)
      0 t gl_UNSIGNED_BYTE p'
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER glNearest
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER glNearest

  return $! tex
  where
    showInfo n i = putStrLn $ unwords [n, show $ CdP.imageWidth i, show $ CdP.imageHeight i]
    getPtr i t = Just (CdP.imageWidth i
                     , CdP.imageHeight i
                     , Vct.unsafeToForeignPtr $ CdP.imageData i
                     , t )
    rdImg = do
      Right img <- CdP.readImage path
      case img of
        CdP.ImageY8     i -> showInfo "Y8"     i >> return Nothing
        CdP.ImageY16    i -> showInfo "Y16"    i >> return Nothing
        CdP.ImageYF     i -> showInfo "YF"     i >> return Nothing
        CdP.ImageYA8    i -> showInfo "YA8"    i >> return Nothing
        CdP.ImageYA16   i -> showInfo "YA16"   i >> return Nothing
        CdP.ImageRGB8   i -> showInfo "RGB8"   i
          >> return (getPtr i gl_RGB)
        CdP.ImageRGB16  i -> showInfo "RGB16"  i >> return Nothing
        CdP.ImageRGBF   i -> showInfo "RGBF"   i >> return Nothing
        CdP.ImageRGBA8  i -> showInfo "RGBA8"  i
          >> return (getPtr i gl_RGBA)
        CdP.ImageRGBA16 i -> showInfo "RGBA16" i >> return Nothing
        CdP.ImageYCbCr8 i -> showInfo "YCbCr8" i >> return Nothing
        CdP.ImageCMYK8  i -> showInfo "CMYK8"  i >> return Nothing
        CdP.ImageCMYK16 i -> showInfo "CMYK16" i >> return Nothing

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

getVertexList :: Shape -> Surface
              -> [(VrtxPos3D,(GLfloat,GLfloat))]
getVertexList Cube f = case f of
  STop    -> zip [p7,p6,p5,p4] uvLst
  SBottom -> zip [p0,p1,p2,p3] uvLst
  SFront  -> zip [p6,p7,p1,p0] uvLst
  SBack   -> zip [p4,p5,p3,p2] uvLst
  SRight  -> zip [p7,p4,p2,p1] uvLst
  SLeft   -> zip [p5,p6,p0,p3] uvLst
  where
    (p0:p1:p2:p3:p4:p5:p6:p7:_) = blockNodeVertex
    uvLst = [ (0.0,0.0)
            , (1.0,0.0)
            , (1.0,1.0)
            , (0.0,1.0)
            ]
getVertexList (Half b) f 
  | not b = case f of -- False
    STop    -> zip [p11,p10,p9,p8] uvLstD
    SBottom -> zip [p0,p1,p2,p3] uvLstD
    SFront  -> zip [p10,p11,p1,p0] uvLstD
    SBack   -> zip [p8,p9,p3,p2] uvLstD
    SRight  -> zip [p11,p8,p2,p1] uvLstD
    SLeft   -> zip [p9,p10,p0,p3] uvLstD
  | otherwise = case f of
    STop    -> zip [p7,p6,p5,p4] uvLstU 
    SBottom -> zip [p10,p11,p8,p9] uvLstU
    SFront  -> zip [p6,p7,p11,p10] uvLstU
    SBack   -> zip [p4,p5,p9,p8] uvLstU
    SRight  -> zip [p7,p4,p8,p11] uvLstU
    SLeft   -> zip [p5,p6,p10,p9] uvLstU
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
              -> Maybe GLuint -> (GLfloat,GLfloat) -> (GLfloat,GLfloat)
              -> (GLfloat,GLfloat,GLfloat,GLfloat) -> IO ()
drawBackPlane (xo,yo) (w,h) tex' (u0,v0) (tw,th) (r,g,b,a) =
  preservingMatrix $ do
    color $ Color4 r g b a 
    case tex' of
      Just t -> do
        texture Texture2D $= Enabled 
        glBindTexture gl_TEXTURE_2D t
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

drawIcon :: WorldResource -> GLfloat -> (GLfloat,GLfloat)
         -> BlockIDNum -> IO ()
drawIcon wldRes icSz (ox,oy) bID | null texIdx = return ()
                                 | otherwise = preservingMatrix $ do
  texture Texture2D $= Enabled 
  glBindTexture gl_TEXTURE_2D tex

  let [tt,_,_,tl,tf,_] = texIdx
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
    icSzW = icSz / 2.2
    icSzH = case shape $ getBlockInfo bID of
              Cube -> icSzW
              Half _ -> icSzW / 2.0
              _ -> icSzW 
    rt = 30.0
    tex = blockTexture wldRes
    d2r d = pi * d / 180.0
    texIdx = textureIndex $ getBlockInfo bID
    [ct,_,_,cl,cf,_] = bcolor $ getBlockInfo bID
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


