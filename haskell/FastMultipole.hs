module FastMultipole (

    -- for debug
    nearestPower
  ,

    solve
  , test
  )
where

------------------------------------------------------------

import Prelude                       as P
import Numeric.LinearAlgebra.HMatrix as H
import Data.Vector.Unboxed.Mutable   as VM
import Control.Monad
import Control.Monad.Primitive
import Data.HashMap.Lazy             as HM
import Control.Exception
import Data.Complex
import Curves
import Image

-- for test
import Test.QuickCheck

------------------------------------------------------------

data Cell = Cell {
    m_moments  :: Moments
  , m_loccoef  :: Coefficients
  }

type Index        = (Int, Int)
type Value        = Complex Double
type Value3       = (Value, Value, Value)
type Moments      = VM.MVector RealWorld Value3
type Coefficients = Moments
type CellTable    = HM.HashMap Index Cell

addValue3 (x0, y0, z0) (x1, y1, z1) = (x0+x1, y0+y1, z0+z1)
mulValue3 (x, y, z) k = (k*x, k*y, k*z)
sumValue3 = foldl addValue3 (0,0,0)

------------------------------------------------------------

nearestPower :: Int -> Int
nearestPower x
  | x < 2     = x
  | otherwise = (2^) . (+1) . truncate . (logBase 2) . fromIntegral $ x-1

fromDouble :: Double -> Value
fromDouble a = a :+ 0.0

factorial :: Int -> Int
factorial k
  | k == 0    = 1
  | k >  0    = product [1..k]
  | otherwise = assert False undefined

funS :: Int -> Value -> Value
funS k z
  | k == 0    = -(log z)
  | k >= 1    = (fromIntegral $ factorial (k-1)) / (z^k)
  | otherwise = assert False undefined

funR :: Int -> Value -> Value
funR k z
  | k >= 0    = -(z^k) / (fromIntegral $ factorial k)
  | otherwise = assert False undefined

funM :: Int -> Value -> Segment -> Value3
funM k zc segment =
  let Color er eg eb = m_dercolor segment
      z = (m_start segment + m_end segment) / 2
      l = fromDouble $ m_length segment
      r = funR k (zc-z)
   in (er*r*l, eg*r*l, eb*r*l)

funN :: Int -> Value -> Segment -> Value3
funN k zc segment =
  let Color cr cg cb = m_difcolor segment
      n = m_normal segment
      z = (m_start segment + m_end segment) / 2
      l = fromDouble $ m_length segment
      r = funR (k-1) (zc-z)
   in (cr*n*r*l, cg*n*r*l, cb*n*r*l)

funA :: Int -> Value -> Segment -> Value3
funA k zc segment = addValue3 (funN k zc segment) (funM k zc segment)

offset :: Int -> Double
offset i = fromIntegral i + 0.5 :: Double

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
      maxOrder = 4 :: Int

  -- At the finest level (maxLevel), discretize each diffusion curve into 
  -- segments so that every segment is wholy contained in a cell.
  --
  -- For each segment, calculate the missing info, e.g. boundary color 
  -- derivative (using BEM solver) and normal.
  --
  -- And then for each cell, find all the contained segments, calculate the 
  -- moments, up to maxOrder's order.
  let doDiscretization :: Curve -> IO [Segment]
      doDiscretization d = return $ discretizeCurve d nx ny cellWidth cellHeight
  segments <- mapM doDiscretization ds >>= return.concat

  let emptyTable = HM.empty :: CellTable
      buildTable :: CellTable -> Segment -> IO CellTable
      buildTable table segment = do
        let idx@(ix, iy) = m_cell segment
            zc = (offset ix * cellWidth) :+ (offset iy * cellHeight)
        case HM.lookup idx table of
          Nothing -> do
            mom <- VM.new maxOrder :: IO Moments
            loc <- VM.new maxOrder :: IO Coefficients
            forM_ [0..maxOrder-1] $ \k -> do
              VM.write mom k $ funA k zc segment
              VM.write loc k (0,0,0)
            return $ HM.insert idx (Cell mom loc) table
          Just cell -> do
            let mom = m_moments cell
            forM_ [0..maxOrder-1] $ \k -> do
              mk <- VM.read mom k
              VM.write mom k $ addValue3 mk (funA k zc segment)
            return table

  -- Calculate moments at the finest level
  table <- foldM buildTable emptyTable segments

  -- Upward propagation to calculate moments at each level using M2M Translation
  let initialTables = [table] :: [CellTable]
      buildTables :: [CellTable] -> Int -> IO [CellTable]
      buildTables tables@(parent:_) level = do
        let newnx = nx `div` (2^level) :: Int
            newny = ny `div` (2^level) :: Int
            indices = [(i,j) | i <- [0..newnx-1], j <- [0..newny-1]]
            w = cellWidth  * (2**fromIntegral level) :: Double
            h = cellHeight * (2**fromIntegral level) :: Double
            emptyTable = HM.empty :: CellTable

            -- For each cell of this level, find corresponding four cells in the
            -- parent, sum up contributions from each parent cell.
            reduceTable :: CellTable -> Double -> Double -> CellTable -> Index -> IO CellTable
            reduceTable pTable w h table idx@(i,j) = do
              let pIndices = [(2*i+ii, 2*j+jj) | ii <- [0..1], jj <- [0..1]]
                  zc = (offset i * w) :+ (offset j * h)

              -- init child cell
              mom <- VM.new maxOrder :: IO Moments
              loc <- VM.new maxOrder :: IO Coefficients
              forM_ [0..maxOrder-1] $ \k -> do
                VM.write mom k $ (0,0,0)
                VM.write loc k $ (0,0,0)

              -- for each parent cell
              forM_ pIndices $ \pIdx@(pi,pj) -> do
                let pzc = (offset pi * w/2) :+ (offset pj * h/2)
                    pCell = case HM.lookup pIdx pTable of
                              Nothing -> assert False undefined
                              Just cell -> cell
                    pMom = m_moments pCell
                forM_ [0..maxOrder-1] $ \k -> do
                  mk <- VM.read mom k
                  ts <- forM [0..k] $ \t -> do
                    a <- VM.read pMom t
                    let r = -(funR (k-t) (zc-pzc))
                    return $ mulValue3 a r
                  VM.write mom k $ sumValue3 (mk:ts)

              return $ HM.insert idx (Cell mom loc) table

        child <- foldM (reduceTable parent w h) emptyTable indices
        return $ child : tables

  tables <- foldM buildTables initialTables [1..maxLevel]

  -- Downward propagation

  return ()

------------------------------------------------------------

test = do
  let prop_nearestPower1 x = (x >= 0) ==> nearestPower x >= x
      prop_nearestPower2 n = (n >= 0 && n < 32) ==> nearestPower t == t where t = 2^n

  quickCheck (prop_nearestPower1 :: Int -> Property)
  quickCheck (prop_nearestPower2 :: Int -> Property)

