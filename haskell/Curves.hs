module Curves (
  DiffusionCurve(..)
  )
where

data DiffusionCurve =
  DiffusionCurve {
    m_id :: Integer,
    m_control :: [(Double,Double)],
    m_lcolor :: [(Double,Double,Double,Double)],
    m_rcolor :: [(Double,Double,Double,Double)],
    m_blur :: [(Double,Double)]
  } deriving (Show)

