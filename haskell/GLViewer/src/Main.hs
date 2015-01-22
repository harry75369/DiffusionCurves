module Main (main) where

import Data.Bits
import Control.Monad
import System.Exit
import Foreign
import Foreign.C
import qualified Text.PrettyPrint as Pretty

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW

fetch f x = alloca $ \ptr -> poke ptr 0 >> f x ptr >> peek ptr

initGL :: GLFW.Window -> IO GLuint
initGL win = do
  glClearColor 0.5 0.5 0.5 1

  vao <- fetch glGenVertexArrays 1
  glBindVertexArray vao

  let vertices = [-1.0, -1.0, 0.0,
                   1.0, -1.0, 0.0,
                   0.0,  1.0, 0.0] :: [Float]
      vsize = 4 * length vertices
  vbo <- fetch glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo
  withArray vertices $ \vsptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral vsize) vsptr gl_STATIC_DRAW

  return vbo

drawScene :: GLuint -> GLuint -> GLFW.Window -> IO ()
drawScene vbo pid win = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glMatrixLoadIdentity gl_PROJECTION
  glMatrixLoadIdentity gl_MODELVIEW

  glUseProgram pid

  glEnableVertexAttribArray 0
  glBindBuffer gl_ARRAY_BUFFER vbo
  glVertexAttribPointer 0 3 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
  glDrawArrays gl_TRIANGLES 0 3
  glDisableVertexAttribArray 0

  GLFW.swapBuffers win

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixLoadIdentity gl_PROJECTION
  glMatrixLoadIdentity gl_MODELVIEW
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed win _               _ _                     _ = return ()

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  vendor   <- liftM castPtr (glGetString gl_VENDOR) >>= peekCString
  version  <- liftM castPtr (glGetString gl_VERSION) >>= peekCString
  renderer <- liftM castPtr (glGetString gl_RENDERER) >>= peekCString
  samples  <- fetch glGetIntegerv gl_SAMPLES

  putStrLn $ Pretty.render $ Pretty.nest 0 (
    Pretty.text "Vendor:"   Pretty.<+> Pretty.text vendor Pretty.$+$
    Pretty.text "Version:"  Pretty.<+> Pretty.text version Pretty.$+$
    Pretty.text "Renderer:" Pretty.<+> Pretty.text renderer Pretty.$+$
    Pretty.text "Samples:"  Pretty.<+> (Pretty.text . show) samples
    )

checkShaderStatus shaderID =
  alloca $ \presult ->
    alloca $ \pinfoLogLength -> do
      poke presult 0
      poke pinfoLogLength 0
      glGetShaderiv shaderID gl_COMPILE_STATUS presult
      glGetShaderiv shaderID gl_INFO_LOG_LENGTH pinfoLogLength
      infoLogLength <- peek pinfoLogLength
      message <- alloca $ \pmessage ->
        glGetShaderInfoLog shaderID infoLogLength nullPtr pmessage
        >> peekCString pmessage
      putStrLn message

checkProgramStatus programID =
  alloca $ \presult ->
    alloca $ \pinfoLogLength -> do
      poke presult 0
      poke pinfoLogLength 0
      glGetProgramiv programID gl_LINK_STATUS presult
      glGetProgramiv programID gl_INFO_LOG_LENGTH pinfoLogLength
      infoLogLength <- peek pinfoLogLength
      message <- alloca $ \pmessage ->
        glGetProgramInfoLog programID infoLogLength nullPtr pmessage
        >> peekCString pmessage
      putStrLn message

loadShaders :: String -> String -> IO GLuint
loadShaders vs fs = do
  vsID <- glCreateShader gl_VERTEX_SHADER
  fsID <- glCreateShader gl_FRAGMENT_SHADER

  -- Compile vertex shader
  withCString vs $ \vsptr ->
    with vsptr $ \vspptr ->
      glShaderSource vsID 1 vspptr nullPtr
  glCompileShader vsID

  -- Check vertex shader
  checkShaderStatus vsID

  -- Compile fragment shader
  withCString fs $ \fsptr ->
    with fsptr $ \fspptr ->
      glShaderSource fsID 1 fspptr nullPtr
  glCompileShader fsID

  -- Check fragment shader
  checkShaderStatus fsID

  -- Link the program
  programID <- glCreateProgram
  glAttachShader programID vsID
  glAttachShader programID fsID
  glLinkProgram programID

  -- Check the program
  checkProgramStatus programID

  -- Finalize
  glDeleteShader vsID
  glDeleteShader fsID

  return programID

main :: IO ()
main = do
  -- Init
  True <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  Just win <- GLFW.createWindow 800 600 "GLViewer" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  printInformation win
  vbo <- initGL win
  vertShader <- readFile "vertex.glsl"
  fragShader <- readFile "fragment.glsl"
  pid <- loadShaders vertShader fragShader

  -- Setup callbacks
  GLFW.setWindowRefreshCallback win (Just $ drawScene vbo pid)
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GLFW.setKeyCallback win (Just keyPressed)

  -- Run
  forever $ do
    GLFW.pollEvents
    drawScene vbo pid win
