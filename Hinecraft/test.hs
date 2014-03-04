module Main where
import Hinecraft.GUI.GLFWWindow
import Control.Exception ( bracket )
import Control.Monad ( unless {-,foldMwhen,forM,void,filterM-} )
import Debug.Trace as Dbg
import System.Directory ( getHomeDirectory )
import qualified Data.ByteString as B
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw
import Control.Concurrent

import Data.Array.MArray
import Data.Array.Storable
import Foreign.Storable
import Foreign.Ptr

main :: IO ()
main = bracket initHinecraft exitHinecraft runHinecraft

initHinecraft :: IO GLFWHandle
initHinecraft = do
  home <- getHomeDirectory
  Dbg.traceIO "Hinecraft Start"
  glfwHdl <- initGLFW winSize

  --initShader home
  --initGL
  return $! glfwHdl
  where
    winSize = (1366,768)

exitHinecraft :: GLFWHandle -> IO ()
exitHinecraft glfwHdl = do
  exitGLFW glfwHdl
  Dbg.traceIO "Hinecraft End"

myPoints :: [GLfloat]
myPoints = [(-1.0), (-1.0), (5.0), 1.0, (-1.0), (5.0), 0.0, 1.0, (5.0)]
  --concatMap (\k -> [sin(2*pi*k/120), cos(2*pi*k/120), 0.0]) [1..3]

runHinecraft :: GLFWHandle -> IO ()
runHinecraft glfwHdl = do
  _ <- getDeltTime glfwHdl
  [va] <- genObjectNames 1
  bindVertexArrayObject $= Just va
  [vb] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vb
  arr <- newListArray (0, length myPoints - 1) myPoints
  withStorableArray arr (\ptr ->
   bufferData ArrayBuffer $= (fromIntegral $ length myPoints * (sizeOf (head myPoints)), ptr, StaticDraw))

  mainLoop vb
  where
    mainLoop vb = do
      pollGLFW
      threadDelay 100000
      _ <- getDeltTime glfwHdl
      exitflg' <- getExitReqGLFW glfwHdl
      draw vb
      --drawView (glfwHdl,guiRes,wldRes) ntmstat' nplstat' runMode' d' 
      swapBuff glfwHdl
      unless exitflg' $ mainLoop vb

draw :: a -> IO ()
draw vb = do
  clear [ ColorBuffer ]
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 (plusPtr nullPtr 0))
  drawArrays Triangles 0 3
  vertexAttribArray (AttribLocation 0) $= Disabled
  flush


initGL :: IO ()
initGL = do
  tu <- get maxTextureUnit
  tm <- get maxTextureSize
  lt <- get maxLights
  Dbg.traceIO $ unwords [ "\n max texutere unit =", show tu
                        , "\n max texture size =", show tm
                        , "\n max lights =" , show lt
                        ]

  texture Texture2D $= Disabled -- Enabled
  shadeModel        $= Smooth
  clearColor        $= Color4 0 0 0 0.0
  clearDepth        $= 1.0
  depthFunc         $= Just Less

  --
  lineSmooth        $= Enabled
  lineWidth         $= 10.0
  blend             $= Enabled
  blendFunc         $= (SrcAlpha, OneMinusSrcAlpha)
  --blendFunc         $= (SrcAlpha, OneMinusConstantAlpha)
  alphaFunc         $= Just (Greater, 0.2)
  --lightModelAmbient $= Color4 0.1 0.1 0.1 0.2
  --lighting        $= Enabled
  colorMaterial     $= Just (FrontAndBack, AmbientAndDiffuse)
  --colorMaterial     $= Just (GL.Front, AmbientAndDiffuse)

  glHint gl_PERSPECTIVE_CORRECTION_HINT gl_NICEST

initShader :: FilePath -> IO ()
initShader home = do
  Dbg.traceIO "\nload basic.vert"
  vertSrc <- B.readFile $ home ++ "/.Hinecraft/shader/basic.vert"
  vsh <- compShader vertSrc VertexShader

  Dbg.traceIO "load basic.frag"
  frgSrc <- B.readFile $ home ++ "/.Hinecraft/shader/basic.frag"
  fsh <- compShader frgSrc FragmentShader 

  prg <- createProgram
  attachShader prg vsh
  attachShader prg fsh

  attribLocation prg "VertexPosition" $= AttribLocation 0
  attribLocation prg "VertexColor" $= AttribLocation 1
  bindFragDataLocation prg "FragColor" $= 0

  linkProgram prg
  ls <- get $ linkStatus prg
  if ls
    then Dbg.traceIO "prg link ok"
    else do
      Dbg.traceIO "prg link error"
      Dbg.traceIO =<< get (programInfoLog prg)  
  -- 
  releaseShaderCompiler
  --

  validateProgram prg
  ps <- get $ validateStatus prg
  if ps
    then Dbg.traceIO "prg validate ok"
    else do
      Dbg.traceIO "prg link error"
      Dbg.traceIO =<< get (programInfoLog prg)  

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




