module Dvgfile (
  readDvgfile
  )
where

------------------------------------------------------------

import qualified Text.Parsec as P
import qualified ParserElem  as E
import Curves

------------------------------------------------------------

p_tuple = E.parens values where
  values = do
    x <- E.float
    y <- E.comma >> E.float
    return (x,y)

p_quadruple = E.parens values where
  values = do
    x <- E.float
    y <- E.comma >> E.float
    z <- E.comma >> E.float
    w <- E.comma >> E.float
    return (x,y,z,w)

p_control = E.brackets $ P.sepBy p_tuple E.comma

p_color = E.brackets $ P.sepBy p_quadruple E.comma

p_blur = E.brackets $ P.sepBy p_tuple E.comma

p_dvgfile = E.brackets . P.many . E.braces $ do
  id      <- E.lexstr "id" >> E.colon >> E.integer
  control <- E.comma >> E.lexstr "control" >> E.colon >> p_control
  lcolor  <- E.comma >> E.lexstr "lcolor" >> E.colon >> p_color
  rcolor  <- E.comma >> E.lexstr "rcolor" >> E.colon >> p_color
  blur    <- E.comma >> E.lexstr "blur" >> E.colon >> p_blur
  return $ DiffusionCurve id control lcolor rcolor blur

readDvgfile :: FilePath -> IO [Curve]
readDvgfile path = readFile path >>= deal . P.parse p_dvgfile "error" where
  deal (Left err) = print err >> return []
  deal (Right ds) = return ds

