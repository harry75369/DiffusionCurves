module Main (main) where

import Data.Bits
import Control.Monad
import System.Exit
import Foreign
import Foreign.C
import qualified Text.PrettyPrint as Pretty
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Codec.Picture

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
      uvs = [
        0.000059, 1.0-0.000004,
        0.000103, 1.0-0.336048,
        0.335973, 1.0-0.335903,
        1.000023, 1.0-0.000013,
        0.667979, 1.0-0.335851,
        0.999958, 1.0-0.336064,
        0.667979, 1.0-0.335851,
        0.336024, 1.0-0.671877,
        0.667969, 1.0-0.671889,
        1.000023, 1.0-0.000013,
        0.668104, 1.0-0.000013,
        0.667979, 1.0-0.335851,
        0.000059, 1.0-0.000004,
        0.335973, 1.0-0.335903,
        0.336098, 1.0-0.000071,
        0.667979, 1.0-0.335851,
        0.335973, 1.0-0.335903,
        0.336024, 1.0-0.671877,
        1.000004, 1.0-0.671847,
        0.999958, 1.0-0.336064,
        0.667979, 1.0-0.335851,
        0.668104, 1.0-0.000013,
        0.335973, 1.0-0.335903,
        0.667979, 1.0-0.335851,
        0.335973, 1.0-0.335903,
        0.668104, 1.0-0.000013,
        0.336098, 1.0-0.000071,
        0.000103, 1.0-0.336048,
        0.000004, 1.0-0.671870,
        0.336024, 1.0-0.671877,
        0.000103, 1.0-0.336048,
        0.336024, 1.0-0.671877,
        0.335973, 1.0-0.335903,
        0.667969, 1.0-0.671889,
        1.000004, 1.0-0.671847,
        0.667979, 1.0-0.335851] :: [GLfloat]
      uvsize = 4 * length uvs
  vbo1 <- fetch glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo1
  withArray vertices $ \vsptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral vsize) vsptr gl_STATIC_DRAW

  vbo2 <- fetch glGenBuffers 1
  glBindBuffer gl_ARRAY_BUFFER vbo2
  withArray uvs $ \uvsptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral uvsize) uvsptr gl_STATIC_DRAW

  return [vbo1, vbo2]

drawScene :: [GLuint] -> GLuint -> GLuint -> GLint -> GLFW.Window -> IO ()
drawScene vbos pid tbo tid win = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  gluPerspective 45.0 1.333 0.1 100.0
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  gluLookAt 4 3 3 0 0 0 0 1 0

  glUseProgram pid

  glActiveTexture gl_TEXTURE0
  glBindTexture gl_TEXTURE_2D tbo
  glUniform1i tid 0

  glEnableVertexAttribArray 0
  glEnableVertexAttribArray 1

  glBindBuffer gl_ARRAY_BUFFER (head vbos)
  glVertexAttribPointer 0 3 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
  glBindBuffer gl_ARRAY_BUFFER (vbos!!1)
  glVertexAttribPointer 1 2 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr

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

loadTexture :: FilePath -> IO GLuint
loadTexture path = do
  Right dynimg <- readTGA path
  let ImageRGB8 img = dynimg
  let w = imageWidth img
      h = imageHeight img

  tbo <- fetch glGenTextures 1
  glBindTexture gl_TEXTURE_2D tbo
  BS.useAsCString (BSL.toStrict $ encodeTga img) $ \imgdata ->
    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral gl_RGB) (fromIntegral w) (fromIntegral h) 0 (fromIntegral gl_RGB) gl_UNSIGNED_BYTE imgdata
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)
  glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)

  return tbo

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
  tbo <- loadTexture "uvtemplate.tga"
  let textureName = "myTextureSampler"
  tid <- withCString textureName $ \tnameptr -> glGetUniformLocation pid tnameptr

  -- Setup callbacks
  GLFW.setWindowRefreshCallback win (Just $ drawScene vbos pid tbo tid)
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setWindowCloseCallback win (Just shutdown)
  GLFW.setKeyCallback win (Just keyPressed)

  -- Run
  forever $ do
    GLFW.pollEvents
    drawScene vbos pid tbo tid win
