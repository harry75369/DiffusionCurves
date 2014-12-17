module Image (
    matricesToPng
  , vectorToPng
  )
where

------------------------------------------------------------

import Numeric.LinearAlgebra.HMatrix as H
import Codec.Picture
import Data.Word
import qualified Data.Vector.Unboxed as VU

------------------------------------------------------------

toWord8 :: Double -> Word8
toWord8 = truncate.(*255.0).(max 0.0).(min 1.0)

matricesToPng :: [Matrix Double] -> FilePath -> IO ()
matricesToPng ms path = do
  let (w, h) = size $ head ms
      mR = ms !! 0
      mG = ms !! 1
      mB = ms !! 2
      f i j = PixelRGB8 (toWord8 $ mR ! i ! j) (toWord8 $ mG ! i ! j) (toWord8 $ mB ! i ! j)
      image = generateImage f w h

  writePng path image

vectorToPng :: Int -> Int -> VU.Vector (Double, Double, Double) -> FilePath -> IO ()
vectorToPng w h v path = do
  let image = generateImage f w h
      f i j =
        let (r, g, b) = v VU.! (i*w+j)
         in PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)

  writePng path image
