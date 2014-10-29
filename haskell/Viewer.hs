module Main (main) where

--------------------------------------------------------------------------------

import           Graphics.Rendering.OpenGL.Raw
import           Graphics.Rendering.GLU.Raw
import           Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Data.Bits
import Control.Monad
import System.Exit (exitWith, ExitCode(..))
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint (($+$), (<+>))
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String (withCStringLen, peekCString)
import qualified Data.ByteString.Char8 as BS

--------------------------------------------------------------------------------

pathObj = 42

--------------------------------------------------------------------------------

initGL :: GLFW.Window -> IO ()
initGL win = do
  glClearColor 0.5 0.5 0.5 1
  glClearStencil 0
  glStencilMask $ complement 0
  let path = "M100,180 L40,10 L190,120 L10,120 L160,10 z\
             \M300 300 C 100 400,100 200,300 100,500 200,500 400,300 300Z"
  withCStringLen path $ \(ptr, len) -> do
    glPathStringNV pathObj gl_PATH_FORMAT_SVG_NV (fromIntegral len) (castPtr ptr)
  glPathParameteriNV pathObj gl_PATH_JOIN_STYLE_NV gl_ROUND_NV
  glPathParameterfNV pathObj gl_PATH_STROKE_WIDTH_NV (realToFrac 6.5)

  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h

drawScene :: GLFW.Window -> IO ()
drawScene win = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_STENCIL_BUFFER_BIT
  glMatrixLoadIdentity gl_PROJECTION
  glMatrixLoadIdentity gl_MODELVIEW
  glMatrixOrtho gl_MODELVIEW 0 500 0 400 (-1) 1
  -- fill
  glStencilFillPathNV pathObj gl_COUNT_UP_NV 0x1F
  glEnable gl_STENCIL_TEST
  glStencilFunc gl_NOTEQUAL 0 0x1
  glStencilOp gl_KEEP gl_KEEP gl_ZERO
  glColor3f 0 1 0
  glCoverFillPathNV pathObj gl_BOUNDING_BOX_NV
  -- stroke
  glStencilStrokePathNV pathObj 0x1 (complement 0)
  glColor3f 1 1 0
  glCoverStrokePathNV pathObj gl_CONVEX_HULL_NV

resizeScene :: GLFW.FramebufferSizeCallback
resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  {-gluOrtho2D (-5.0) 5.0 (-5.0) 5.0-}
  glMatrixMode gl_MODELVIEW
  glLoadIdentity
  glFlush

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

--------------------------------------------------------------------------------

printInformation :: GLFW.Window -> IO ()
printInformation win = do
  vendor   <- glGetString gl_VENDOR >>= return.castPtr >>= peekCString
  version  <- glGetString gl_VERSION >>= return.castPtr >>= peekCString
  renderer <- glGetString gl_RENDERER >>= return.castPtr >>= peekCString
  {-exts     <- glGetString gl_EXTENSIONS >>= return.castPtr >>= peekCString-}
  samples  <- alloca $ \samples_ptr -> poke samples_ptr 0 >> glGetIntegerv gl_SAMPLES samples_ptr >> peek samples_ptr

  putStrLn $ Pretty.render $ Pretty.nest 0 (
    Pretty.text "------------------------------------------------------------" $+$
    Pretty.text "Vendor:"     <+> Pretty.text vendor   $+$
    Pretty.text "Version:"    <+> Pretty.text version  $+$
    Pretty.text "Renderer:"   <+> Pretty.text renderer $+$
    {-Pretty.text "Extensions:" <+> Pretty.text exts     $+$-}
    Pretty.text "Samples:"    <+> (Pretty.text . show) samples
    )

checkExtensions :: GLFW.Window -> IO ()
checkExtensions win = do
  exts     <- glGetString gl_EXTENSIONS >>= return.castPtr >>= peekCString >>= return.(BS.pack)

  let targets = ["NV_path_rendering", "EXT_direct_state_access"]
      checkExtension :: String -> IO ()
      checkExtension ext =
        let (_, matched) = BS.breakSubstring (BS.pack ext) exts
         in if BS.null matched then error ("required " ++ ext ++ " extension is not present")
                               else putStrLn ("found " ++ ext ++ " extension")
   in mapM_ checkExtension targets

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- init
  True <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  Just win <- GLFW.createWindow 800 600 "OpenGL" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  printInformation win
  checkExtensions win
  initGL win

  -- setup callbacks
  GLFW.setWindowRefreshCallback   win (Just drawScene)
  GLFW.setFramebufferSizeCallback win (Just resizeScene)
  GLFW.setWindowCloseCallback     win (Just shutdown)
  GLFW.setKeyCallback             win (Just keyPressed)

  -- run
  forever $ do
    GLFW.pollEvents
    drawScene win
    GLFW.swapBuffers win
