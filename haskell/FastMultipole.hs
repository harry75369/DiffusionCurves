module FastMultipole (
  solve
  )
where

------------------------------------------------------------

import Prelude                       as P
import Numeric.LinearAlgebra.HMatrix as H
import Control.Monad
import Data.Complex
import Curves
import Image

-- for main
import Dvgfile (readDvgfile)

-- for test
import Test.QuickCheck

------------------------------------------------------------

data Cell = Cell {
    m_width    :: Int
  , m_height   :: Int
  , m_pos      :: Complex Double
  , m_moments  :: Complex Double
  , m_loccoef  :: Complex Double
  , m_children :: [Cell]
  } deriving (Show)

type V2 = (Double, Double)

------------------------------------------------------------

(@-@) :: V2 -> V2 -> V2
(@-@) (x1,y1) (x2,y2) = (x1-x2,y1-y2)

dist :: V2 -> V2 -> Double
dist (x1,y1) (x2,y2) =
  let x = x1 - x2
      y = y1 - y2
   in sqrt $ x*x + y*y

prod :: V2 -> V2 -> Double
prod (x1,y1) (x2,y2) = x1*x2 + y1*y2

normalize :: V2 -> V2
normalize (0,0) = (0,0)
normalize (x,y) = (x/len, y/len)
  where len = sqrt $ x*x + y*y

factorial n = product [1..n]

{-funcS :: Int -> Complex Double -> Complex Double-}
{-funcS 0 z = - log z-}
{-funcS k z = factorial (k-1) / z ^ k-}

{-funcR :: Int -> Complex Double -> Complex Double-}
{-funcR k z = (- z ^ k) / factorial k-}

{-funcM_term :: Int -> Complex Double -> (Complex Double -> Complex Double -> Complex Double -> Complex Double)-}
{-funcM_term k zc = (\vE vDs z0 -> vE * funcR k (z0 - zc) * vDs)-}

{-funcN_term :: Int -> Complex Double -> (Complex Double -> Complex Double -> Complex Double -> Complex Double)-}
{-funcN_term k zc = (\vC vN vDs z0 -> vC * vN * funcR (k-1) (z0 - zc) * vDs)-}

nearestPower :: Int -> Int
nearestPower x
  | x < 2     = x
  | otherwise = (2^) . (+1) . truncate . (logBase 2) . fromIntegral $ x-1

--------------------------------------------------------------------------------
--
-- T. Sun, P. Thamjaroenporn, and C. Zheng, "Fast Multipole Representation of 
-- Diffusion Curves and Points", ACM Trans. Graph. SIGGRAPH 2014
--
--------------------------------------------------------------------------------

solve :: [Curve] -> Int -> Int -> IO ()
solve ds width height = do
  let nx = (nearestPower width) :: Int
      ny = (nearestPower height) :: Int
      maxLevel = (truncate.(logBase 2) $ min nx ny) :: Int
      cellWidth  = (fromIntegral width  / fromIntegral nx) :: Double
      cellHeight = (fromIntegral height / fromIntegral ny) :: Double
      k = 4 :: Int

  -- At the finest level (maxLevel), discretize each diffusion curve into 
  -- segments so that every segment is wholy contained in a cell.
  --
  -- For each segment, calculate the missing info, e.g. boundary color 
  -- derivative (using BEM solver) and normal.
  --
  -- And then for each cell, find all the contained segments, calculate the 
  -- moments, up to k's order.
  forM ds $ \d -> do
    let segs = discretizeCurve d nx ny cellWidth cellHeight

{-
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

    let vectorEs = [r,g,b] where [Just r, Just g, Just b] = map (linearSolve matrixB) matrixZs
    disp 2 $ tr $ vectorEs !! 0
    disp 2 $ tr $ vectorEs !! 1
    disp 2 $ tr $ vectorEs !! 2
-}

  return ()

------------------------------------------------------------

main = do
  ds  <- readDvgfile "./data/sample-01.dvg"
  print ds
  solve ds 256 256

test = do
  let prop_nearestPower1 x = (x >= 0) ==> nearestPower x >= x
      prop_nearestPower2 n = (n >= 0 && n < 32) ==> nearestPower t == t where t = 2^n

  quickCheck (prop_nearestPower1 :: Int -> Property)
  quickCheck (prop_nearestPower2 :: Int -> Property)

