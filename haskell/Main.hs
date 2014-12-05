module Main (main) where

------------------------------------------------------------

import System.Environment
import qualified Dvgfile       as D
import qualified FastMultipole as F

------------------------------------------------------------

main = do
  ds  <- D.readDvgfile "./data/sample-01.dvg"
  print ds
  F.test
  n <- fmap head getArgs
  F.solve ds (read n) (read n)
