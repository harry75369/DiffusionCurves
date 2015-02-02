module Main (main) where

import Data.Bits
import Control.Monad
import System.Exit
import Foreign
import Foreign.C
import qualified Text.PrettyPrint as Pretty
import qualified Data.ByteString.Char8 as BS

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import qualified Graphics.UI.GLFW as GLFW

fetch f x = alloca $ \ptr -> poke ptr 0 >> f x ptr >> peek ptr

initGL :: GLFW.Window -> IO [GLuint]
initGL win = do
  glClearColor 0.5 0.5 0.5 1
  glEnable gl_DEPTH_TEST
  glDepthFunc gl_LESS

  -- Bind vertex array object
  vao <- fetch glGenVertexArrays 1
  glBindVertexArray vao

  -- Vertex buffer object
  let vertices = [
        -1.0,-1.0,-1.0,
        -1.0,-1.0, 1.0,
        -1.0, 1.0, 1.0,
        1.0, 1.0,-1.0,
        -1.0,-1.0,-1.0,
        -1.0, 1.0,-1.0,
        1.0,-1.0, 1.0,
        -1.0,-1.0,-1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0,-1.0,
        1.0,-1.0,-1.0,
        -1.0,-1.0,-1.0,
        -1.0,-1.0,-1.0,
        -1.0, 1.0, 1.0,
        -1.0, 1.0,-1.0,
        1.0,-1.0, 1.0,
        -1.0,-1.0, 1.0,
        -1.0,-1.0,-1.0,
        -1.0, 1.0, 1.0,
        -1.0,-1.0, 1.0,
        1.0,-1.0, 1.0,
        1.0, 1.0, 1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0,-1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0, 1.0,
        1.0,-1.0, 1.0,
        1.0, 1.0, 1.0,
        1.0, 1.0,-1.0,
        -1.0, 1.0,-1.0,
        1.0, 1.0, 1.0,
        -1.0, 1.0,-1.0,
        -1.0, 1.0, 1.0,
        1.0, 1.0, 1.0,
        -1.0, 1.0, 1.0,
        1-1.0,-1.0,-1.0,
        -1.0, 1.0,-1.0,
        1.0,-1.0, 1.0,
        -1.0,-1.0,-1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0,-1.0,
        1.0,-1.0,-1.0,
        -1.0,-1.0,-1.0,
        -1.0,-1.0,-1.0,
        -1.0, 1.0, 1.0,
        -1.0, 1.0,-1.0,
        1.0,-1.0, 1.0,
        -1.0,-1.0, 1.0,
        -1.0,-1.0,-1.0,
        -1.0, 1.0, 1.0,
        -1.0,-1.0, 1.0,
        1.0,-1.0, 1.0,
        1.0, 1.0, 1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0,-1.0,
        1.0,-1.0,-1.0,
        1.0, 1.0, 1.0,
        1.0,-1.0, 1.0,
        1.0, 1.0, 1.0,
        1.0, 1.0,-1.0,
        -1.0, 1.0,-1.0,
        1.0, 1.0, 1.0,
        -1.0, 1.0,-1.0,
        -1.0, 1.0, 1.0,
        1.0, 1.0, 1.0,
        -1.0, 1.0, 1.0,
        1.0,-1.0, 1.0] :: [GLfloat]
      vsize = 4 * length vertices
      colors = [
        0.583, 0.771, 0.014,
        0.609, 0.115, 0.436,
        0.327, 0.483, 0.844,
        0.822, 0.569, 0.201,
        0.435, 0.602, 0.223,
        0.310, 0.747, 0.185,
        0.597, 0.770, 0.761,
        0.559, 0.436, 0.730,
        0.359, 0.583, 0.152,
        0.483, 0.596, 0.789,
        0.559, 0.861, 0.639,
        0.195, 0.548, 0.859,
        0.014, 0.184, 0.576,
        0.771, 0.328, 0.970,
        0.406, 0.615, 0.116,
        0.676, 0.977, 0.133,
        0.971, 0.572, 0.833,
        0.140, 0.616, 0.489,
        0.997, 0.513, 0.064,
        0.945, 0.719, 0.592,
        0.543, 0.021, 0.978,
        0.279, 0.317, 0.505,
        0.167, 0.620, 0.077,
        0.347, 0.857, 0.137,
        0.055, 0.953, 0.042,
        0.714, 0.505, 0.345,
        0.783, 0.290, 0.734,
        0.722, 0.645, 0.174,
        0.302, 0.455, 0.848,
        0.225, 0.587, 0.040,
        0.517, 0.713, 0.338,
        0.053, 0.959, 0.120,
        0.393, 0.621, 0.362,
        0.673, 0.211, 0.457,
        0.820, 0.883, 0.371,
        0.982, 0.099, 0.879] :: [GLfloat]
      csize = 4 * length colors
  vbo1 <- fetch glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo1
  withArray vertices $ \vsptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral vsize) vsptr gl_STATIC_DRAW

  vbo2 <- fetch glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo2
  withArray colors $ \csptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral csize) csptr gl_STATIC_DRAW

  return [vbo1, vbo2]

drawScene :: [GLuint] -> GLuint -> GLFW.Window -> IO ()
drawScene vbos pid win = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45.0 1.333 0.1 100.0
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  gluLookAt 4 3 3 0 0 0 0 1 0

  glUseProgram pid

  glEnableVertexAttribArray 0
  glEnableVertexAttribArray 1

  glBindBuffer gl_ARRAY_BUFFER (head vbos)
  glVertexAttribPointer 0 3 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
  glBindBuffer gl_ARRAY_BUFFER (vbos!!1)
  glVertexAttribPointer 1 3 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr

  glDrawArrays gl_TRIANGLES 0 36

  glDisableVertexAttribArray 0
  glDisableVertexAttribArray 1

  GLFW.swapBuffers win

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
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

checkExtensions :: IO ()
checkExtensions = do
  exts <- liftM BS.pack $ liftM castPtr (glGetString gl_EXTENSIONS) >>= peekCString

  let targets = ["EXT_direct_state_access"]
      checkExt ext =
        let (_, matched) = BS.breakSubstring (BS.pack ext) exts
         in if BS.null matched then error ("required " ++ ext ++ " extension is not present")
                               else putStrLn ("found " ++ ext ++ " extension")
   in mapM_ checkExt targets

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
      putStrLn "=== Shader Status ==="
      if null message then putStrLn "OK" else putStr message

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
      putStrLn "=== Program Status ==="
      if null message then putStrLn "OK" else putStr message

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
  --checkExtensions
  vbos <- initGL win
  vertShader <- readFile "vertex.glsl"
  fragShader <- readFile "fragment.glsl"
  pid <- loadShaders vertShader fragShader

  -- Setup callbacks
  GLFW.setWindowRefreshCallback win (Just $ drawScene vbos pid)
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GLFW.setKeyCallback win (Just keyPressed)

  -- Run
  forever $ do
    GLFW.pollEvents
    drawScene vbos pid win
