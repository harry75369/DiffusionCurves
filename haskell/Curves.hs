module Curves (
  Curve(..),
  Segment(..),
  deCasteljau,
  discreteSegments,
  boundaryColors
  )
where

------------------------------------------------------------

data Curve =
  DiffusionCurve {
    m_id :: Integer,
    m_control :: [(Double,Double)],
    m_lcolor :: [(Double,Double,Double,Double)],
    m_rcolor :: [(Double,Double,Double,Double)],
    m_blur :: [(Double,Double)]
  } deriving (Show)

data Segment =
  Segment {
    m_start :: (Double, Double),
    m_end   :: (Double, Double)
  } deriving (Show)

type Color = (Double, Double, Double)

------------------------------------------------------------

deCasteljau :: Double -> [(Double,Double)] -> (Double,Double)
deCasteljau t [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (lerpP t) coefs (tail coefs)
    lerpP t (x0, y0) (x1, y1) = (lerp t x0 x1, lerp t y0 y1)
    lerp t a b = (1 - t) * a + t * b

discreteSegments :: Curve -> Int -> Int -> Int -> [Segment]
discreteSegments curve w h nSegs =
  let cps = map (\(x, y) -> (x * fromIntegral w, y * fromIntegral h)) $ m_control curve
      dt  = 1.0 / fromIntegral nSegs
      ts  = map (flip deCasteljau $ cps) [0, 0+dt .. 1]
      ts2 = tail ts
   in map (\(a, b) -> Segment a b) $ zip ts ts2

boundaryColors :: Curve -> Int -> ([Color], [Color])
boundaryColors curve nSegs =
  let dt = 1.0 / (fromIntegral nSegs)
      rs = map (*dt) [0.5,1.5..(fromIntegral nSegs)-0.5]
      lcolors = map (toContinuous $ m_lcolor curve) rs
      rcolors = map (toContinuous $ m_rcolor curve) rs
      toContinuous :: [(Double, Double, Double, Double)] -> Double -> (Double, Double, Double)
      toContinuous [] = (\x -> (0.0, 0.0, 0.0))
      toContinuous discretes = (\x -> f $ (min 1.0).(max 0.0) $ x)
        where f t = let (r0, g0, b0, t0) = findDown t discretes
                        (r1, g1, b1, t1) = findUp   t discretes
                        a = t1 - t
                        b = t - t0
                        dt = t1 - t0
                     in (a*r0/dt+b*r1/dt,a*g0/dt+b*g1/dt,a*b0/dt+b*b1/dt)
              findDown t list = iter (0.0, 0.0, 0.0, 0.0) list
                where iter cand [] = cand
                      iter cand@(_,_,_,t0) list@((_,_,_,t1):xs)
                        | t1 >= t0 && t1 <= t = iter (head list) xs
                        | otherwise         = iter cand xs
              findUp t list = iter (0.0, 0.0, 0.0, 1.0) list
                where iter cand [] = cand
                      iter cand@(_,_,_,t0) list@((_,_,_,t1):xs)
                        | t1 >= t && t1 <= t0 = iter (head list) xs
                        | otherwise         = iter cand xs
   in (lcolors, rcolors)
