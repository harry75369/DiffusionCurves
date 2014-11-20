module FastMultipole (
  solve
  )
where

------------------------------------------------------------

import Prelude as P
import Curves
import Control.Monad (forM_)
import Numeric.LinearAlgebra.HMatrix as H

-- for main
import Dvgfile (readDvgfile)

------------------------------------------------------------

type V2 = (Double, Double)

(@-@) :: V2 -> V2 -> V2
(@-@) (x1,y1) (x2,y2) = (x1-x2,y1-y2)

dist :: V2 -> V2 -> Double
dist (x1,y1) (x2,y2) =
  let x = x1 - x2
      y = y1 - y2
   in sqrt $ x*x + y*y

area :: V2 -> V2 -> V2 -> Double
area (x1,y1) (x2,y2) (x3,y3) = (*0.5) . abs $ (x1-x3)*(y2-y1)-(x1-x2)*(y3-y1)

prod :: V2 -> V2 -> Double
prod (x1,y1) (x2,y2) = x1*x2 + y1*y2

normalize :: V2 -> V2
normalize (0,0) = (0,0)
normalize (x,y) = (x/len, y/len)
  where len = sqrt $ x*x + y*y

range :: Int -> [Int]
range n = [0..n-1]

------------------------------------------------------------

-- T. Sun, P. Thamjaroenporn, and C. Zheng, "Fast Multipole Representation
-- of Diffusion Curves and Points", ACM Trans. Graph. SIGGRAPH 2014
solve :: [Curve] -> Int -> Int -> IO ()
solve ds w h = do
  let dx = 8 -- pixels width a cell
      dy = 8 -- pixels height a cell
      nx = (ceiling $ fromIntegral w / dx) :: Int
      ny = (ceiling $ fromIntegral h / dy) :: Int
      grid = (nx><ny) $ replicate (nx*ny) 0.0
      nSegs = 10 :: Int

  disp 2 grid

  forM_ ds $ \d -> do
    -- first: BEM solves for E(y_i)
    let segs = discreteSegments d w h nSegs
        findMiddle (x0,y0) (x1,y1) = ((x0+x1)/2, (y0+y1)/2)
        matrixN = matrix nSegs [f i j | i <- range nSegs, j <- range nSegs]
          where f i j = let Segment s0 e0 = segs !! i
                            Segment s1 e1 = segs !! j
                            m0 = findMiddle s0 e0
                         in 2 * (area m0 s1 e1) / (dist e1 s1)
        matrixS = matrix nSegs [f i j | i <- range nSegs, j <- range nSegs]
          where f i j = let Segment s0 e0 = segs !! i
                            Segment s1 e1 = segs !! j
                            m0 = findMiddle s0 e0
                         in prod (normalize $ e1@-@s1) (m0@-@s1)
        matrixT = matrix nSegs [f i j | i <- range nSegs, j <- range nSegs]
          where f i j = let Segment s0 e0 = segs !! i
                            Segment s1 e1 = segs !! j
                            m0 = findMiddle s0 e0
                         in prod (normalize $ e1@-@s1) (e1@-@m0)
        matrixA = matrix nSegs [f i j | i <- range nSegs, j <- range nSegs]
          where f i j = let t = matrixT ! i ! j
                            s = matrixS ! i ! j
                            n = matrixN ! i ! j
                         in atan (t/n) - atan (s/n)
        matrixB = matrix nSegs [f i j | i <- range nSegs, j <- range nSegs]
          where f i j = let t = matrixT !  i ! j
                            s = matrixS !  i ! j
                            n = matrixN !  i ! j
                            f x = x * log (x*x + n*n)
                            g x = x - n * atan (x/n)
                         in 0.5 * (f t - f s) - (g t - g s)
        vectorCs = [vector $ map r c, vector $ map g c, vector $ map b c]
          where colorDiff (l, r) = zipWith (\(r0,g0,b0) (r1,g1,b1) -> (r0-r1,g0-g1,b0-b1)) l r
                c = colorDiff $ boundaryColors d nSegs
                r (x,_,_) = x
                g (_,x,_) = x
                b (_,_,x) = x
                vector = matrix 1
        matrixZs = [(matrixA - pi*(ident nSegs)) <> v | v <- vectorCs]

    disp 2 matrixN
    disp 2 matrixS
    disp 2 matrixT
    disp 2 matrixA
    disp 2 matrixB
    disp 2 $ tr $ vectorCs !! 0
    disp 2 $ tr $ vectorCs !! 1
    disp 2 $ tr $ vectorCs !! 2

    let vectorEs = [r,g,b] where [Just r, Just g, Just b] = map (linearSolve matrixB) matrixZs
    disp 2 $ tr $ vectorEs !! 0
    disp 2 $ tr $ vectorEs !! 1
    disp 2 $ tr $ vectorEs !! 2

  return ()

------------------------------------------------------------

main = do
  ds  <- readDvgfile "./data/sample-01.dvg"
  print ds
  solve ds 256 256
