{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Curves (
    Curve(..)
  , Segment(..)
  , Color(..)

  -- for debug
  , deCasteljau
  , controlPoints
  , discreteSegments
  , getColor
  , boundaryColorDifferences
  , boundaryColorDerivatives
  {-, properNumSegs-}
  {-, cut-}
  {-, makeSegments-}

  , discretizeCurve
  )
where

------------------------------------------------------------

import Data.Complex
import Numeric.LinearAlgebra.HMatrix
import qualified Data.Vector as V

-- for debug
import Debug.Trace

------------------------------------------------------------

data Curve =
  DiffusionCurve {
    m_id      :: Integer
  , m_control :: [(Double,Double)]
  , m_lcolor  :: [(Double,Double,Double,Double)]
  , m_rcolor  :: [(Double,Double,Double,Double)]
  , m_blur    :: [(Double,Double)]
  } deriving (Show)

data Segment =
  Segment {
    m_start    :: Complex Double
  , m_end      :: Complex Double
  , m_length   :: Double
  , m_normal   :: Complex Double
  , m_difcolor :: Color          -- boundary color difference
  , m_dercolor :: Color          -- boundary color derivative
  , m_cell     :: Index
  }

data Color =
  Color {
    m_red   :: Complex Double
  , m_green :: Complex Double
  , m_blue  :: Complex Double
  } deriving (Show)

type Index = (Int, Int)
type Seg = ((Complex Double, Double), (Complex Double, Double))

instance Show Segment where
  show (Segment s e l n di de c) =
    "segment: (" ++ show s ++ ") -> (" ++ show e ++ ")\n" ++
    "length:   " ++ show l ++ "\n" ++
    "normal:   (" ++ show n ++ ")\n" ++
    "difcolor: " ++ show di ++ "\n" ++
    "dercolor: " ++ show de ++ "\n" ++
    "index:    " ++ show c ++ "\n"

to255Color :: Color -> Color
to255Color (Color r g b) =
  let s = 255.0 :+ 0.0
   in Color (s*r) (s*g) (s*b)

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
                  f c0 c1 = (a/d) * c0 + (b/d) * c1
                  g c0 c1 = (c0+c1) / (2:+0)
               in if t0 == t1 then Color (g r0 r1) (g g0 g1) (g b0 b1)
                  else Color (f r0 r1) (f g0 g1) (f b0 b1)
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

getColorOrdered :: [(Color, Double)] -> Double -> Color
getColorOrdered [] _ = Color 0 0 0
getColorOrdered cs t =
  let s = (min 1.0).(max 0.0) $ t
      n = length cs
      i = truncate $ s * fromIntegral n
      j = min (n-1) (i+1)
      (Color r0 g0 b0, s0) = cs !! i
      (Color r1 g1 b1, s1) = cs !! j
      a = if i==j then (0.5 :+ 0) else (s1-s)/(s1-s0) :+ 0
      b = if i==j then (0.5 :+ 0) else (s-s0)/(s1-s0) :+ 0
   in Color (a*r0+b*r1) (a*g0+b*g1) (a*b0+b*b1)

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
      ssegs = V.fromList segs

      {-# INLINE range #-}
      range n = [0..n-1]
      {-# INLINE area #-}
      area  (x1:+y1) (x2:+y2) (x3:+y3) = (*0.5) . abs $ (x1-x3)*(y2-y1)-(x1-x2)*(y3-y1)
      {-# INLINE dot #-}
      dot   (x1:+y1) (x2:+y2) = x1*x2 + y1*y2
      {-# INLINE makeComplexMatrix #-}
      makeComplexMatrix m = toComplex (m, (n><n) $ iterate id 0)

      -- matrixN = (n><n) [f i j | i <- range n, j <- range n]
      matrixN = build (n, n) f :: Matrix Double
        where
          {-# INLINE f #-}
          f :: Double -> Double -> Double
          f !i !j = let ((s0, _), (e0, _)) = ssegs V.! (truncate i)
                        ((s1, _), (e1, _)) = ssegs V.! (truncate j)
                        m0 = (s0 + e0) / 2
                     in 2 * (area m0 s1 e1) / (magnitude $ e1-s1)
      -- matrixS = (n><n) [f i j | i <- range n, j <- range n]
      matrixS = build (n, n) f :: Matrix Double
        where
          {-# INLINE f #-}
          f :: Double -> Double -> Double
          f !i !j = let ((s0, _), (e0, _)) = ssegs V.! (truncate i)
                        ((s1, _), (e1, _)) = ssegs V.! (truncate j)
                        m0 = (s0 + e0) / 2
                        vec = e1 - s1
                     in dot (vec / (magnitude vec :+ 0)) (s1 - m0)
      -- matrixT = (n><n) [f i j | i <- range n, j <- range n]
      matrixT = build (n, n) f :: Matrix Double
        where
          {-# INLINE f #-}
          f :: Double -> Double -> Double
          f !i !j = let ((s0, _), (e0, _)) = ssegs V.! (truncate i)
                        ((s1, _), (e1, _)) = ssegs V.! (truncate j)
                        m0 = (s0 + e0) / 2
                        vec = e1 - s1
                     in dot (vec / (magnitude vec :+ 0)) (e1 - m0)
      -- matrixA = (n><n) [f i j | i <- range n, j <- range n]
      matrixA = build (n, n) f :: Matrix Double
        where
          {-# INLINE f #-}
          f :: Double -> Double -> Double
          f !i !j = let t = matrixT ! (truncate i) ! (truncate j)
                        s = matrixS ! (truncate i) ! (truncate j)
                        n = matrixN ! (truncate i) ! (truncate j)
                     in atan (t/n) - atan (s/n)
      -- matrixB = (n><n) [f i j | i <- range n, j <- range n]
      matrixB = build (n, n) f :: Matrix Double
        where
          {-# INLINE f #-}
          f :: Double -> Double -> Double
          f !i !j = let t = matrixT ! (truncate i) ! (truncate j)
                        s = matrixS ! (truncate i) ! (truncate j)
                        n = matrixN ! (truncate i) ! (truncate j)
                        f x = x * log (x*x + n*n)
                        g x = x - n * atan (x/n)
                     in 0.5 * (f t - f s) - (g t - g s)
      vectorCs =
        let getComponent = [\(Color r g b) -> r, \(Color r g b) -> g, \(Color r g b) -> b]
         in [(n><1) $ map (extract.fst) difcolors | extract <- getComponent]
      matrixZs = [(makeComplexMatrix $ matrixA - 2*pi*(ident n)) <> v | v <- vectorCs]
      [Just vectorEr, Just vectorEg, Just vectorEb] = map (linearSolve $ makeComplexMatrix matrixB) matrixZs
      dercolors = [Color (vectorEr!i!0) (vectorEg!i!0) (vectorEb!i!0) | i <- range n]
   in zip dercolors $ map snd difcolors

------------------------------------------------------------
{-
properNumSegs :: Curve -> Int -> Int -> Double -> Double -> Int
properNumSegs curve nx ny cw ch = iter $ max nx ny
  where
    width = fromIntegral nx * cw
    height = fromIntegral ny * ch
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

cut :: Double -> Double -> Seg -> (Seg, Seg)
cut cw ch ((start@(xi:+yi), ti), (end@(xj:+yj), tj)) =
  let x_l = fromIntegral.floor $ xi / cw
      x_u = x_l + 1.0
      y_l = fromIntegral.floor $ yi / ch
      y_u = y_l + 1.0
      ts  = (if xi == xj then -1 else (x_l-xi)/(xj-xi))
          : (if xi == xj then -1 else (x_u-xi)/(xj-xi))
          : (if yi == yj then -1 else (y_l-yi)/(yj-yi))
          : (if yi == yj then -1 else (y_u-yi)/(yj-yi)) : []
      tss = (filter (<1)).(filter (>0)) $ ts
      t   = if null tss then 1 else head tss
      mt  = start + (end-start) * (t:+0)
      tt  = (1-t)*ti + t*tj
   in (((start, ti), (mt, tt)), ((mt, tt), (end, tj)))

makeSegments :: Double -> Double -> [(Color,Double)] -> [(Color,Double)] -> [Seg] -> [Segment]
makeSegments _ _ _ _ [] = []
makeSegments cw ch difcolors dercolors (x:xs) =
  let ((start, ti), (end, tj)) = x
      (ix, iy) = getIndex start
      (jx, jy) = getIndex end
      len = magnitude $ end - start
      getIndex :: Complex Double -> (Int, Int)
      getIndex (x :+ y) = (truncate $ x / cw, truncate $ y / ch)
      makeSegment :: Seg -> Segment
      makeSegment ((start, ti), (end, tj)) =
        let vec    = end - start
            len    = magnitude vec
            normal = if len < 1e-8 then (0:+0) else (0:+1) * vec / (len:+0)
            color  = getColorOrdered difcolors $ (ti+tj) / 2
            bcolor = getColorOrdered dercolors $ (ti+tj) / 2
         in Segment start end len normal color bcolor (getIndex start)
   in if (ix == jx && iy == jy)
         then if len < 1e-8 then makeSegments xs else makeSegment x : makeSegments cw ch difcolors dercolors xs
         else makeSegment x0 : makeSegments cw ch difcolors dercolors (x1:xs) where (x0, x1) = cut cw ch x
-}
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
                : (if yi == yj then -1 else (y_u-yi)/(yj-yi)) : []
            tss = (filter (<1)).(filter (>0)) $ ts
            t   = if null tss then 1 else head tss
            mt  = start + (end-start) * (t:+0)
            tt  = (1-t)*ti + t*tj
         in (((start, ti), (mt, tt)), ((mt, tt), (end, tj)))
      makeSegments :: [Seg] -> [Segment]
      makeSegments [] = []
      makeSegments (x:xs) =
        let ((start, ti), (end, tj)) = x
            (ix, iy) = getIndex start
            (jx, jy) = getIndex end
            len = magnitude $ end - start
            getIndex :: Complex Double -> (Int, Int)
            getIndex (x :+ y) = (ix, iy)
              where tx = truncate $ x / cw
                    ty = truncate $ y / ch
                    ix = if tx >= nx then nx-1 else tx
                    iy = if ty >= ny then ny-1 else ty
            makeSegment :: Seg -> Segment
            makeSegment ((start, ti), (end, tj)) =
              let vec    = end - start
                  len    = magnitude vec
                  normal = if len < 1e-8 then (0:+0) else (0:+1) * vec / (len:+0)
                  color  = id $ getColorOrdered difcolors $ (ti+tj) / 2
                  bcolor = id $ getColorOrdered dercolors $ (ti+tj) / 2
               in Segment start end len normal color bcolor (getIndex start)
         in if (ix == jx && iy == jy)
               then if len < 1e-8 then makeSegments xs else makeSegment x : makeSegments xs
               else makeSegment x0 : makeSegments (x1:xs) where (x0, x1) = cut x
   in makeSegments segs

