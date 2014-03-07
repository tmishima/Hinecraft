{-# LANGUAGE BangPatterns #-}
--
-- Copyright : (c) T.Mishima 2014
-- License : Apache-2.0
--
module Hinecraft.Render.Shader
  ( ShaderParam (..)
  , ShaderPrgSrc (..)
  , genShaderProg
  )
  where

import Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString as B
import Debug.Trace as Dbg

data ShaderParam = ShaderParam
  { shdprg :: Program
  }

data ShaderPrgSrc = ShaderPrgSrc
  { vSrc :: FilePath 
  , fSrc :: FilePath
  , vattrLoc :: [String]
  , fattrLoc :: [String]
  }


genShaderProg :: ShaderPrgSrc -> IO ShaderParam
genShaderProg sps = do
  Dbg.traceIO "\nload vert shader"
  vertSrc <- B.readFile $ vSrc sps
  vsh <- compShader vertSrc VertexShader

  Dbg.traceIO "load frag shader"
  frgSrc <- B.readFile $ fSrc sps
  fsh <- compShader frgSrc FragmentShader 

  prg <- createProgram
  attachShader prg vsh
  attachShader prg fsh

  mapM_ (\ (attr,idx) -> 
    attribLocation prg attr $= AttribLocation idx)
    $ zip (vattrLoc sps) [0 .. ]

  mapM_ (\ (attr,idx) ->
    bindFragDataLocation prg attr $= idx)
    $ zip (fattrLoc sps) [0 .. ]

  linkProgram prg
  ls <- get $ linkStatus prg
  if ls
    then Dbg.traceIO "prg link ok"
    else do
      Dbg.traceIO "prg link error"
      Dbg.traceIO =<< get (programInfoLog prg)  
  -- 
  --detachShader prg vsh
  --detachShader prg fsh
  releaseShaderCompiler
  --
  validateProgram prg
  --currentProgram $= Just prg
  ps <- get $ validateStatus prg
  if ps
    then Dbg.traceIO "prg validate ok"
    else do
      Dbg.traceIO "prg link error"
      Dbg.traceIO =<< get (programInfoLog prg)  

  return ShaderParam { shdprg = prg }
  where
    compShader src stype = do
      sh <- createShader stype 
      shaderSourceBS sh $= src 
      compileShader sh
      cs <- get $ compileStatus sh
      if cs
        then Dbg.traceIO "sh complie ok"
        else do
          Dbg.traceIO "sh complie error"
          Dbg.traceIO =<< get (shaderInfoLog sh)
      return $! sh


