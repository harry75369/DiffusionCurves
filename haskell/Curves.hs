module Curves (
  Curve(..),
  Segment(..),
  Color(..),

  -- for debug
  deCasteljau,
  controlPoints,
  discreteSegments,
  getColor,
  boundaryColorDifferences,
  boundaryColorDerivatives,

  discretizeCurve
  )
where

------------------------------------------------------------

import Data.Complex
import Numeric.LinearAlgebra.HMatrix

-- for debug
import Debug.Trace

------------------------------------------------------------

data Curve =
  DiffusionCurve {
     m_id      :: Integer
  ,  m_control :: [(Double,Double)]
  ,  m_lcolor  :: [(Double,Double,Double,Double)]
  ,  m_rcolor  :: [(Double,Double,Double,Double)]
  ,  m_blur    :: [(Double,Double)]
  } deriving (Show)

data Segment =
  Segment {
     m_start    :: Complex Double
  ,  m_end      :: Complex Double
  ,  m_length   :: Double
  ,  m_normal   :: Complex Double
  ,  m_difcolor :: Color          -- boundary color difference
  ,  m_dercolor :: Color          -- boundary color derivative
  ,  m_cell     :: Index
  } deriving (Show)

data Color =
  Color {
     m_red   :: Complex Double
  ,  m_green :: Complex Double
  ,  m_blue  :: Complex Double
  } deriving (Show)

type Index = (Int, Int)
type Seg = ((Complex Double, Double), (Complex Double, Double))

------------------------------------------------------------

deCasteljau :: Double -> [Complex Double] -> Complex Double
deCasteljau t [b] = b
deCasteljau t coefs =
  let lerp t a b = (1 - t) * a + t * b
      lerpP t (x0 :+ y0) (x1 :+ y1) = (lerp t x0 x1) :+ (lerp t y0 y1)
      reduced = zipWith (lerpP t) coefs (tail coefs)
   in deCasteljau t reduced

controlPoints :: Curve -> Double -> Double -> [Complex Double]
controlPoints curve w h = map (\(x, y) -> (x*w) :+ (y*h)) $ m_control curve

discreteSegments :: Curve -> Double -> Double -> Int -> [Seg]
discreteSegments curve w h nSegs =
  let cps = controlPoints curve w h
      dt  = 1.0 / fromIntegral nSegs
      ts  = zip (map (flip deCasteljau $ cps) rs) rs where rs = [0, 0+dt .. 1]
   in zip ts $ tail ts

getColor :: [(Color, Double)] -> Double -> Color
getColor [] = (\t -> Color 0 0 0)
getColor cs = (\t -> f $ (min 1.0).(max 0.0) $ t)
  where f t = let (Color r0 g0 b0, t0) = findDown t cs
                  (Color r1 g1 b1, t1) = findUp   t cs
                  a = (t1 - t)  :+ 0
                  b = (t  - t0) :+ 0
                  d = (t1 - t0) :+ 0
                  f :: Complex Double -> Complex Double -> Complex Double
                  f c0 c1 = (a/d) * c0 + (b/d) * c1
               in Color (f r0 r1) (f g0 g1) (f b0 b1)
        findDown t list = iter (Color 0 0 0, 0.0) list
          where iter cand [] = cand
                iter cand@(Color _ _ _, t0) list@((Color _ _ _, t1):xs)
                  | t1 >= t0 && t1 <= t = iter (head list) xs
                  | otherwise           = iter cand xs
        findUp   t list = iter (Color 0 0 0, 1.0) list
          where iter cand [] = cand
                iter cand@(Color _ _ _, t0) list@((Color _ _ _, t1):xs)
                  | t1 >= t && t1 <= t0 = iter (head list) xs
                  | otherwise           = iter cand xs

boundaryColorDifferences :: Curve -> Int -> [(Color, Double)]
boundaryColorDifferences curve nSegs =
  let dt = 1.0 / (fromIntegral nSegs)
      rs = map (*dt) [0.5,1.5..(fromIntegral nSegs)-0.5]
      toColor (r,g,b,t) = (Color (r:+0) (g:+0) (b:+0), t)
      lcolors = map (getColor $ map toColor $ m_lcolor curve) rs
      rcolors = map (getColor $ map toColor $ m_rcolor curve) rs
      dcolors = zipWith (\(Color r0 g0 b0) (Color r1 g1 b1) ->
                  Color (r0-r1) (g0-g1) (b0-b1)) lcolors rcolors
   in zip dcolors rs

boundaryColorDerivatives :: [Seg] -> [(Color, Double)] -> [(Color, Double)]
boundaryColorDerivatives segs difcolors = do
  True <- return $ length segs == length difcolors
  let n = length segs
      range n = [0..n-1]
      area  (x1:+y1) (x2:+y2) (x3:+y3) = (*0.5) . abs $ (x1-x3)*(y2-y1)-(x1-x2)*(y3-y1)
      dot   (x1:+y1) (x2:+y2) = x1*x2 + y1*y2
      matrixN = (n><n) [f i j | i <- range n, j <- range n]
        where f i j = let ((s0, _), (e0, _)) = segs !! i
                          ((s1, _), (e1, _)) = segs !! j
                          m0 = (s0 + e0) / 2
                       in 2 * (area m0 s1 e1) / (magnitude $ e1-s1)
      matrixS = (n><n) [f i j | i <- range n, j <- range n]
        where f i j = let ((s0, _), (e0, _)) = segs !! i
                          ((s1, _), (e1, _)) = segs !! j
                          m0 = (s0 + e0) / 2
                          vec = e1 - s1
                       in dot (vec / (magnitude vec :+ 0)) (m0 - s1)
      matrixT = (n><n) [f i j | i <- range n, j <- range n]
        where f i j = let ((s0, _), (e0, _)) = segs !! i
                          ((s1, _), (e1, _)) = segs !! j
                          m0 = (s0 + e0) / 2
                          vec = e1 - s1
                       in dot (vec / (magnitude vec :+ 0)) (e1 - m0)
      matrixA = (n><n) [f i j | i <- range n, j <- range n]
        where f i j = let t = matrixT ! i ! j
                          s = matrixS ! i ! j
                          n = matrixN ! i ! j
                       in atan (t/n) - atan (s/n)
      matrixB = (n><n) [f i j | i <- range n, j <- range n]
        where f i j = let t = matrixT ! i ! j
                          s = matrixS ! i ! j
                          n = matrixN ! i ! j
                          f x = x * log (x*x + n*n)
                          g x = x - n * atan (x/n)
                       in 0.5 * (f t - f s) - (g t - g s)
      vectorCs = [(n><1) $ map (extract.fst) difcolors | extract <- getComponent]
        where getComponent = [\(Color r g b) -> r,
                              \(Color r g b) -> g,
                              \(Color r g b) -> b]
      makeComplexMatrix m = toComplex (m, (n><n) $ iterate id 0)
      matrixZs = [(makeComplexMatrix $ matrixA - pi*(ident n)) <> v | v <- vectorCs]
      [Just vectorEr, Just vectorEg, Just vectorEb] = map (linearSolve $ makeComplexMatrix matrixB) matrixZs
      dercolors = [Color (vectorEr!i!0) (vectorEg!i!0) (vectorEb!i!0) | i <- range n]
   in zip dercolors $ map snd difcolors
  {-disp 2 matrixN-}
  {-disp 2 matrixS-}
  {-disp 2 matrixT-}
  {-disp 2 matrixA-}
  {-disp 2 matrixB-}

------------------------------------------------------------

discretizeCurve :: Curve -> Int -> Int -> Double -> Double -> [Segment]
discretizeCurve curve nx ny cw ch =
  let width  = fromIntegral nx * cw
      height = fromIntegral ny * ch
      properNumSegs :: Int
      properNumSegs = iter $ max nx ny
        where
          cps = controlPoints curve width height
          startPoint = head cps
          lowerBound = (*0.5) $ min cw ch
          upperBound = 2 * lowerBound
          iter guess
            | len < lowerBound = iter $ guess `div` 2
            | len > upperBound = iter $ guess * 2
            | otherwise        = trace ("properNumSegs = " ++ show guess) guess
            where
              dt = (1.0 / fromIntegral guess) :: Double
              endPoint = deCasteljau dt cps
              len = magnitude $ startPoint - endPoint
      segs      = discreteSegments curve width height properNumSegs
      difcolors = boundaryColorDifferences curve properNumSegs
      dercolors = boundaryColorDerivatives segs difcolors
      cut :: Seg -> (Seg, Seg)
      cut ((start@(xi:+yi), ti), (end@(xj:+yj), tj)) =
        let x_l = fromIntegral.floor $ xi / cw
            x_u = x_l + 1.0
            y_l = fromIntegral.floor $ yi / ch
            y_u = y_l + 1.0
            ts  = (if xi == xj then -1 else (x_l-xi)/(xj-xi))
                : (if xi == xj then -1 else (x_u-xi)/(xj-xi))
                : (if yi == yj then -1 else (y_l-yi)/(yj-yi))
                : (if yi == yj then -1 else (y_u-yj)/(yj-yi)) : []
            t   = head.(filter (<1)).(filter (>0)) $ ts
            mt  = start + (end-start) * (t:+0)
            tt  = (1-t)*ti + t*tj
         in (((start, ti), (mt, tt)), ((mt, tt), (end, tj)))
      makeSegments :: [Seg] -> [Segment]
      makeSegments [] = []
      makeSegments (x:xs) =
        let ((start, ti), (end, tj)) = x
            (ix, iy) = getIndex start
            (jx, jy) = getIndex end
            getIndex :: Complex Double -> (Int, Int)
            getIndex (x :+ y) = (truncate $ x / cw, truncate $ y / ch)
            makeSegment :: Seg -> Segment
            makeSegment ((start, ti), (end, tj)) =
              let vec    = end - start
                  len    = magnitude vec
                  normal = (0:+1) * vec / (len:+0)
                  color  = getColor difcolors $ (ti+tj) / 2
                  bcolor = getColor dercolors $ (ti+tj) / 2
               in Segment start end len normal color bcolor (getIndex start)
         in if (ix == jx && iy == jy)
               then makeSegment x : makeSegments xs
               else makeSegment x0 : makeSegments (x1:xs) where (x0, x1) = cut x
   in makeSegments segs
