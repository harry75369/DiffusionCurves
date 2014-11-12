module FastMultipole (
  solve
  )
where

------------------------------------------------------------

import qualified Data.Array.Repa as R
import Curves

import Dvgfile (readDvgfile)

------------------------------------------------------------

-- solve :: [DiffusionCurve] -> Int -> Int -> Image
solve ds w h = id

------------------------------------------------------------

main = do
  ds  <- readDvgfile "./data/sample-01.dvg"
  print ds
