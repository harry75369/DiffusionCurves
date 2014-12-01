module Main (main) where

------------------------------------------------------------

import Dvgfile
import FastMultipole

------------------------------------------------------------

main = do
  ds  <- readDvgfile "./data/sample-01.dvg"
  print ds
  solve ds 32 32
