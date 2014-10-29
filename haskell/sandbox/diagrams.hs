import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

hilbert = iterate expand mempty where
  expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
    u = vcat [t, alignT $ vrule 1, rotateBy (3/4) t]

example :: Diagram B R2
example = pad 1.1 . centerXY $ hilbert!!5

main = mainWith example


