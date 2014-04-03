{-# LANGUAGE BangPatterns, OverloadedStrings #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.WorldView
  ( WorldViewVHdl (blockTexture)
  , initWorldView
  , drawWorldView
  , renderEnvCube
  , genChunkVAO
  , WorldVAOList
  , initWorldVAOList
  , getBlockVAOList
  , setBlockVAOList
  , updateVAOlist
  )
  where

--import Data.Tree 

import Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GU
import qualified Graphics.GLUtil.Camera3D as GU3
--import qualified Graphics.GLUtil.Camera2D as GU2
--import Linear ( M44 ) --  eye4
import Linear.V3
import qualified Data.Map as M
import Control.Monad (  forM_, forM , foldM {-,when, unless,void-} )
--import Debug.Trace as Dbg
import Data.IORef
import Control.Applicative

import Hinecraft.Types
import Hinecraft.Model
import Hinecraft.Render.Util
import Hinecraft.Render.Types
import Hinecraft.Render.WithBasicShader as BSd
import Hinecraft.Render.WithSimpleShader as SSd

data WorldViewVHdl = WorldViewVHdl 
  { basicShader :: BasicShaderProg
  , simpleShader :: SimpleShaderProg
  , envCube :: GU.VAO
  , blockTexture :: TextureObject
  , skyTexture :: TextureObject
  , blkVAOList :: IORef WorldVAOList
  , blkCursol :: GU.VAO
  --, starTexture ::
  }

type WorldVAOList = M.Map (Int,Int) [Maybe (Int,GU.VAO)]

updateVAOlist :: WorldViewVHdl -> [(((Int, Int), Int),SurfacePos)]
              -> IO ()
updateVAOlist wvhdl sufList = do
  vao <- getBlockVAOList wvhdl
  newvao <- foldM fn vao sufList 
  setBlockVAOList wvhdl newvao  
  where
    fn wvlst ((ij,bNo'),sfs) = 
      case M.lookup ij wvlst of
        Just d -> do
          case d !! bNo' of
            Just (_,v) -> GU.deleteVAO v
            Nothing -> return ()
          vao <- genChunkVAO wvhdl sfs 
          let newVAO = take bNo' d ++ [vao]
                       ++ drop (bNo' + 1) d
          return $! M.update (upfn newVAO) ij wvlst
        Nothing -> return wvlst
    upfn nv _ = Just nv


getBlockVAOList :: WorldViewVHdl -> IO WorldVAOList
getBlockVAOList vwHdl = readIORef (blkVAOList vwHdl)

setBlockVAOList :: WorldViewVHdl -> WorldVAOList -> IO ()
setBlockVAOList vwHdl = writeIORef (blkVAOList vwHdl)

initWorldView :: FilePath -> IO WorldViewVHdl
initWorldView home = do
  bsh <- initShaderProgram home
  ssh <- initShaderProgram home
  blks <- loadTexture blkPng
  sky <- loadTexture skyPng
  cb <- genEnvCubeVAO bsh
  vao <- newIORef M.empty 
  bCur <- genBlockCursol ssh
  return WorldViewVHdl 
    { basicShader = bsh
    , simpleShader = ssh
    , envCube = cb
    , blockTexture = blks
    , skyTexture = sky
    , blkVAOList = vao
    , blkCursol = bCur
    }
  where
    blkPng = home ++ "/.Hinecraft/terrain.png"
    skyPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/cloud1.png"
    --starPng = home ++ "/.Hinecraft/mcpatcher/sky/world0/star1.png"

initWorldVAOList :: WorldViewVHdl 
                 -> [((Int, Int), [SurfacePos])] -> IO ()
initWorldVAOList wvhdl suflst = do
  vao <- M.fromList <$> mapM (\ (ij,slst) -> do
    ds <- forM slst (genChunkVAO wvhdl)
    return (ij,ds)
    ) suflst
  writeIORef (blkVAOList wvhdl) vao

drawWorldView :: WorldViewVHdl -> (Int,Int)
              -> WorldVAOList -> UserStatus
              -> Maybe (WorldIndex,Surface) -> IO ()
drawWorldView wvhdl (w,h) vaos usrStat' pos = 
  GU.withViewport (Position 0 0) (Size (fromIntegral w)
                  (fromIntegral h)) $ do
    -- Draw 3D cube
    let prjMat = GU3.projectionMatrix (GU3.deg2rad 60)
                   (fromIntegral w/ fromIntegral h) 0.1 (2000::GLfloat)
    setProjViewMat pg prjMat
    setCamParam pg -- $ GU3.dolly (V3 0 (-uy - 1.5) 0)
                   $ GU3.pan ury $ GU3.tilt urx GU3.fpsCamera

    renderEnvCube pg cbVao skyTex 

    setCamParam pg $ GU3.pan ury $ GU3.tilt urx
                   $ GU3.dolly (V3 ux (uy + 1.5) uz)
                   GU3.fpsCamera
     
    forM_ vs (\ (_,vas) ->
      mapM_ (\ v -> renderChunk pg v blkTex) vas)

    -- Cursol 選択された面を強調
    case pos of 
      Just ((px,py,pz),s) -> do
        --lineWidth    $= 1.2
        setProjViewMat spg prjMat
        setCamParam spg 
                $ GU3.pan ury $ GU3.tilt urx
                $ GU3.dolly (V3 (ux - fromIntegral px)
                                ((uy + 1.5) - fromIntegral py)
                                (uz - fromIntegral pz))
                GU3.fpsCamera
        renderBlockCursol spg bCurVAO s
      Nothing -> return ()

  where
    pg = basicShader wvhdl
    spg = simpleShader wvhdl
    bCurVAO = blkCursol wvhdl
    vs = M.toList vaos
    cbVao = envCube wvhdl
    skyTex = skyTexture wvhdl
    blkTex = blockTexture wvhdl
    d2f (a,b,c) = (realToFrac a, realToFrac b, realToFrac c)
    (ux,uy,uz) = d2f $ userPos usrStat'
    (urx,ury,_) = d2f $ userRot usrStat' :: (GLfloat,GLfloat,GLfloat)

genBlockCursol :: SimpleShaderProg -> IO GU.VAO
genBlockCursol sh = makeSimpShdrVAO sh  vertLst vertClrLst texCdLst
  where
    vertLst = concatMap ajust $ concat vlst
    vertClrLst = concat $ replicate (4 * 6) [0.1,0.1,0.1,1.0]
    texCdLst = concat $ replicate (4 * 6) [0.0,1.0]
    extnd v = if v > 0 then v + 0.05 else  v - 0.05
    ajust (a,b,c) = [ extnd a, extnd b, extnd c]
    vlst = map (fst . getVertexList Cube ) [STop,SBottom,SFront,SBack
                                           ,SRight,SLeft]

renderBlockCursol :: SimpleShaderProg -> GU.VAO -> Surface -> IO ()
renderBlockCursol sh vao s = do
  currentProgram $= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled
  enableTexture sh False 
  GU.withVAO vao $ 
    drawArrays LineLoop (calcIdx s) 4
  currentProgram $= Nothing
  where 
    !shprg' = getShaderProgram sh
    calcIdx f = case f of
      STop -> 0
      SBottom -> 4
      SFront -> 8
      SBack -> 12
      SRight -> 16
      SLeft -> 20

renderChunk :: BasicShaderProg -> Maybe (Int,GU.VAO) 
            -> TextureObject -> IO ()
renderChunk _ Nothing _ = return ()
renderChunk _ (Just (0,_)) _ = return ()
renderChunk sh (Just (len,vao)) tex = do
  currentProgram $= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled
  setLightMode sh 1
  setColorBlendMode sh 0
  enableTexture sh True
  GU.withVAO vao $ 
    GU.withTextures2D [tex] $ drawArrays Quads 0 $ fromIntegral len * 4

  currentProgram $= Nothing
  where 
    !shprg' = getShaderProgram sh

genChunkVAO :: WorldViewVHdl -> SurfacePos -> IO (Maybe (Int,GU.VAO))
genChunkVAO wvhdl = genChunkVAO' sh
  where sh = basicShader wvhdl

genChunkVAO' :: BasicShaderProg -> SurfacePos -> IO (Maybe (Int,GU.VAO))
genChunkVAO' _ [] = return Nothing
genChunkVAO' sh bsf = do
  vao <- makeBasicShdrVAO sh (concat vertlst)
                      (concat clrlst)
                      (concat normlst)
                      (concat coordlst)
  return $! Just (ndnum, vao)
  where
    (clrlst, normlst, coordlst, vertlst, ndnum) = foldr 
      (\ e (cs,ns,cds,vs,s) -> let (c,n,cd,v,num) = genElem e
                             in (c:cs,n:ns,cd:cds,v:vs,num+s)
                             ) ([],[],[],[],0) bsf
  --

genElem :: (WorldIndex, BlockIDNum, [Surface])
        -> ([GLfloat],[GLfloat],[GLfloat],[GLfloat],Int)
genElem ((x,y,z),bid,fs) =
    ( colorList fs, normList fs, coordList fs
    , vertexList fs ,4 * length fs)
  where
    sp = shape $ getBlockInfo bid
    -- Top | Bottom | Right | Left | Front | Back
    [tt',tb',tr',tl',tf',tba'] = case textureIndex $ getBlockInfo bid of
      [] -> replicate 6 (0,0)
      ti -> ti

    colorList :: [Surface] -> [GLfloat]
    colorList = concatMap (\ f ->
      genColorList $ setColor $ case f of
        STop -> 0
        SBottom -> 1
        SRight -> 2
        SLeft -> 3
        SFront -> 4
        SBack -> 5)

    normList :: [Surface] -> [GLfloat]
    normList = concatMap (\ f ->
      genNormList $ case f of
        STop -> (0,1,0)
        SBottom -> (0,-1,0)
        SRight -> (1,0,0)
        SLeft -> (-1,0,0)
        SFront -> (0,0,-1) 
        SBack -> (0,0,1))

    coordList :: [Surface] -> [GLfloat]
    coordList = concatMap (\ f ->
      let (_,uv) = getVertexList sp f
      in genCoordList (case f of
        STop -> tt' 
        SBottom  -> tb'
        SRight -> tf'
        SLeft -> tba'
        SFront -> tr'
        SBack -> tl' ) uv
        )
    vertexList :: [Surface] -> [GLfloat]
    vertexList = concatMap (\ f ->
      let (vs,_) = getVertexList sp f
      in genVertLst vs 
        )

    d2fv3 :: (Double,Double,Double) -> VrtxPos3D 
    d2fv3 (a,b,c) = ( realToFrac a, realToFrac b, realToFrac c)
    setColor i = d2fv3 $ bcolor (getBlockInfo bid) !! i
    genColorList :: (GLfloat,GLfloat,GLfloat)
                 -> [GLfloat]
    genColorList (r,g,b) = (concat . replicate 4) [r,g,b,1.0]
    genNormList :: (GLfloat,GLfloat,GLfloat)
                 -> [GLfloat]
    genNormList (nx,ny,nz) = (concat . replicate 4) [nx,ny,nz]
    genCoordList :: (Int,Int) -> [(GLfloat,GLfloat)]
                 -> [GLfloat]
    genCoordList (i',j') =
      concatMap (\ (u,v) -> [i * (1/16) + (1/16) * u
                        , j * (1/16) + (1/16) * v ])
      where i = fromIntegral i' ; j = fromIntegral j'
    genVertLst :: [(GLfloat,GLfloat,GLfloat)]
           -> [GLfloat]
    genVertLst =
      concatMap (\ (x', y', z') -> 
               [ x' + fromIntegral x
               , y' + fromIntegral y
               , z' + fromIntegral z
               ]
               ) 

renderEnvCube :: BasicShaderProg -> GU.VAO -> TextureObject -> IO ()
renderEnvCube sh vao tex = do 
  currentProgram $= Just (GU.program shprg')
  --GL.clientState GL.VertexArray $= GL.Enabled
  setLightMode sh 0
  setColorBlendMode sh 1
  enableTexture sh True
  GU.withVAO vao $ 
    GU.withTextures2D [tex] $ drawArrays Quads 0 (4 * 6) 

  currentProgram $= Nothing
  where 
    !shprg' = getShaderProgram sh

genEnvCubeVAO :: BasicShaderProg  -> IO GU.VAO
genEnvCubeVAO sh = 
  makeBasicShdrVAO sh cubeVert qColor qnorm qTexCoord 
  where
    qColor :: [GL.GLfloat]
    qColor = concat $ replicate 24 [ (180/255),(226/255),(255/255), 1.0 ]

    p0,p1,p2,p3,p4,p5,p6,p7 :: [GLfloat]
    {-
    !p0 = [ -0.5, -0.25, -0.5]
    !p1 = [  0.5, -0.25, -0.5]
    !p2 = [  0.5, -0.25,  0.5]
    !p3 = [ -0.5, -0.25,  0.5]
    -} 
    !p0 = [ -0.5, -0.4, -0.5]
    !p1 = [  0.5, -0.4, -0.5]
    !p2 = [  0.5, -0.4,  0.5]
    !p3 = [ -0.5, -0.4,  0.5]
    !p4 = [  0.5,  0.5,  0.5]
    !p5 = [ -0.5,  0.5,  0.5]
    !p6 = [ -0.5,  0.5, -0.5]
    !p7 = [  0.5,  0.5, -0.5]

    cubeVert :: [GLfloat]
    !cubeVert = map (* 2000) $ concat
      [ p6, p7, p1, p0 -- Front
      , p7, p4, p2, p1 -- Right
      , p4, p5, p3, p2 -- Back
      , p5, p6, p0, p3 -- Left
      , p7, p6, p5, p4 -- Top
      , p0, p1, p2 ,p3 -- Bottom
      ]

    qTexCoord :: [GLfloat]
    !qTexCoord =  
      [ 1/3, 1/2 , 2/3, 1/2 , 2/3, 1.0 , 1/3, 1.0
      , 2/3, 1/2 , 1.0, 1/2 , 1.0, 1.0 , 2/3, 1.0
      , 2/3, 0.0 , 1.0, 0.0 , 1.0, 1/2 , 2/3, 1/2
      , 0.0, 1/2 , 1/3, 1/2 , 1/3, 1.0 , 0.0, 1.0
      , 2/3, 1/2 , 1/3, 1/2 , 1/3, 0.0 , 2/3, 0.0
      , 0.0, 0.0 , 1/3, 0.0 , 1/3, 1/2 , 0.0, 1/2
      ]

    qnorm = [ 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            , 0, 0, 0 , 0, 0, 0 , 0, 0, 0 , 0, 0, 0
            ]
