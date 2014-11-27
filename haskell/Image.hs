module Image (
  matricesToPng
  )
where

------------------------------------------------------------

import Numeric.LinearAlgebra.HMatrix as H
import Codec.Picture
import Data.Word

------------------------------------------------------------

matricesToPng :: [Matrix Double] -> FilePath -> IO ()
matricesToPng ms path = do
  let (w, h) = size $ head ms
      toWord8 :: Double -> Word8
      toWord8 = truncate.(*255.0).(max 0.0).(min 1.0)
      mR = ms !! 0
      mG = ms !! 1
      mB = ms !! 2
      f i j = PixelRGB8 (toWord8 $ mR ! i ! j) (toWord8 $ mG ! i ! j) (toWord8 $ mB ! i ! j)
      image = generateImage f w h

  writePng path image
