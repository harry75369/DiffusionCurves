module Main (main) where

------------------------------------------------------------

import System.Environment
import qualified Dvgfile       as D
import qualified FastMultipole as F

------------------------------------------------------------

main = do
  -- Read diffusion curves
  ds  <- D.readDvgfile "./data/sample-01.dvg"
  print ds

  -- Solve using FastMultipole
  F.test
  n <- fmap head getArgs
  F.solve ds (read n) (read n)
