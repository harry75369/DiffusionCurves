module Main (main) where

------------------------------------------------------------

import qualified Dvgfile       as D
import qualified FastMultipole as F

------------------------------------------------------------

main = do
  ds  <- D.readDvgfile "./data/sample-01.dvg"
  print ds
  F.test
  F.solve ds 32 32
