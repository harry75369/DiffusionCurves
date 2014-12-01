module FastMultipole (

    -- for debug
    factorial
  , nearestPower,

    solve
  , test
  )
where

------------------------------------------------------------

import Prelude                       as P
import Numeric.LinearAlgebra.HMatrix as H
import Control.Monad
import Data.Complex
import Curves
import Image

-- for test
import Test.QuickCheck

------------------------------------------------------------

data Cell = Cell {
    m_width    :: Int
  , m_height   :: Int
  , m_pos      :: Complex Double
  , m_moments  :: [Complex Double]
  , m_loccoef  :: [Complex Double]
  , m_children :: [Cell]
  } deriving (Show)

------------------------------------------------------------

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
      maxLevel = (truncate.(logBase 2).fromIntegral $ min nx ny) :: Int
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
  segments <-  (forM ds $ \d -> return $ discretizeCurve d nx ny cellWidth cellHeight)
           >>= return.concat

  print segments

  return ()

------------------------------------------------------------

test = do
  let prop_nearestPower1 x = (x >= 0) ==> nearestPower x >= x
      prop_nearestPower2 n = (n >= 0 && n < 32) ==> nearestPower t == t where t = 2^n

  quickCheck (prop_nearestPower1 :: Int -> Property)
  quickCheck (prop_nearestPower2 :: Int -> Property)

