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
import qualified Data.Vector.Unboxed as VU
import Control.Monad
import Control.Monad.Primitive
import Data.HashMap.Lazy             as HM
import Control.Exception
import Data.Complex
import Curves
import Image

-- for test
import Test.QuickCheck

-- for debug
import Debug.Trace
import Text.Printf

------------------------------------------------------------

data Cell = Cell {
    m_moments  :: Moments
  , m_loccoef  :: Coefficients
  }

type Index        = (Int, Int)
type Value        = Complex Double
type Value3       = (Value, Value, Value)
type Double3      = (Double, Double, Double)
type Moments      = VM.MVector RealWorld Value3
type Coefficients = Moments
type CellTable    = HM.HashMap Index Cell

addValue3 (x0, y0, z0) (x1, y1, z1) = (x0+x1, y0+y1, z0+z1)
mulValue3 (x, y, z) k = (k*x, k*y, k*z)
sumValue3 = foldl addValue3 (0,0,0)
fromValue3 (x,y,z) = (realPart x, realPart y, realPart z)

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
  | k == -1   = 0
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

--------------------------------------------------------------------------------
--
-- T. Sun, P. Thamjaroenporn, and C. Zheng, "Fast Multipole Representation of 
-- Diffusion Curves and Points", ACM Trans. Graph. SIGGRAPH 2014
--
--------------------------------------------------------------------------------

solve :: [Curve] -> Int -> Int -> IO ()
solve ds width height = do

  -- Calculate size and cell size at finest level, and define max order.
  let nx = (nearestPower width) :: Int
      ny = (nearestPower height) :: Int
      cellWidth  = (fromIntegral width  / fromIntegral nx) :: Double
      cellHeight = (fromIntegral height / fromIntegral ny) :: Double
      maxLevel = (truncate.(logBase 2).fromIntegral $ min nx ny) :: Int
      maxOrder = 4 :: Int

  printf "size = %d x %d\n" nx ny
  printf "cell size = %.2f x %.2f\n" cellWidth cellHeight
  printf "maxLevel = %d\n" maxLevel
  printf "maxOrder = %d\n" maxOrder

  -- Define utility functions
  let nxAtLevel level = nx `div` (2^(maxLevel-level)) :: Int
      nyAtLevel level = ny `div` (2^(maxLevel-level)) :: Int
      cwAtLevel level = cellWidth  * (2**(fromIntegral $ maxLevel-level)) :: Double
      chAtLevel level = cellHeight * (2**(fromIntegral $ maxLevel-level)) :: Double
      getCell :: CellTable -> Index -> Cell
      getCell table index =
        case HM.lookup index table of
          Nothing -> assert False undefined
          Just cell -> cell
      getPosition cw ch (i,j) = (offset i * cw) :+ (offset j * ch)
        where offset :: Int -> Double
              offset i = fromIntegral i + 0.5 :: Double

  -- At the finest level (maxLevel), discretize each diffusion curve into 
  -- segments so that every segment is wholy contained in a cell.
  --
  -- For each segment, calculate the missing info, e.g. boundary color 
  -- derivative (using BEM solver) and normal.
  let doDiscretization d = discretizeCurve d nx ny cellWidth cellHeight
  segments <- mapM (return.doDiscretization) ds >>= return.concat
  print segments
  let drawSegments = H.build (nx, ny) f :: H.Matrix Double
        where indices = P.map m_cell segments
              f :: Double -> Double -> Double
              f i j = fromIntegral $ P.length $ P.filter (\(x,y) -> fromIntegral x == i && fromIntegral y == j) indices
  H.disp 0 $ drawSegments
  printf "Total number of segments: %d\n" (P.length segments)

  -- And then for each cell, find all the contained segments, calculate the 
  -- moments, up to maxOrder's order.
  let emptyTable = HM.empty :: CellTable
      initTable :: CellTable -> Index -> IO CellTable
      initTable table idx = do
        mom <- VM.new maxOrder :: IO Moments
        loc <- VM.new maxOrder :: IO Coefficients
        forM_ [0..maxOrder-1] $ \k -> do
           VM.write mom k (0,0,0)
           VM.write loc k (0,0,0)
        return $ HM.insert idx (Cell mom loc) table
      buildTable :: CellTable -> Segment -> IO CellTable
      buildTable table segment = do
        let idx = m_cell segment
            zc = getPosition cellWidth cellHeight idx
        case HM.lookup idx table of
          Nothing -> print idx >> assert False undefined
          Just cell -> do
            let mom = m_moments cell
            forM_ [0..maxOrder-1] $ \k -> do
              mk <- VM.read mom k
              VM.write mom k $ addValue3 mk (funA k zc segment)
            return table

  -- Calculate moments at the finest level
  table <- foldM initTable emptyTable [(i,j) | i <- [0..nx-1], j <- [0..ny-1]]
       >>= (\table -> foldM buildTable table segments)

  let printTable :: Int -> Int -> CellTable -> IO ()
      printTable w h table = do
        let indices = [(i,j) | i <- [0..w-1], j <- [0..h-1]]
        forM_ indices $ \idx@(i,j) -> do
          let c = getCell table idx
              m = m_moments c
              l = m_loccoef c
          printf "Cell (%d, %d)\n" i j
          forM_ [0..maxOrder-1] $ \k -> do
            VM.read m k >>= print
            VM.read l k >>= print

  printTable nx ny table


  -- Upward propagation to calculate moments at each level using M2M Translation
  let initialTables = [table] :: [CellTable]
      buildTables :: [CellTable] -> Int -> IO [CellTable]
      buildTables tables@(pTable:_) level = do
        let nx = nxAtLevel $ maxLevel - level
            ny = nyAtLevel $ maxLevel - level
            cw = cwAtLevel $ maxLevel - level
            ch = chAtLevel $ maxLevel - level
            indices = [(i,j) | i <- [0..nx-1], j <- [0..ny-1]]
            emptyTable = HM.empty :: CellTable

            -- For each cell of this level, find corresponding four cells in the
            -- parent, sum up contributions from each parent cell.
            reduceTable :: CellTable -> Double -> Double -> CellTable -> Index -> IO CellTable
            reduceTable pTable cw ch table idx@(i,j) = do
              let pIndices = [(2*i+ii, 2*j+jj) | ii <- [0..1], jj <- [0..1]]
                  zc = getPosition cw ch idx

              -- init child cell
              mom <- VM.new maxOrder :: IO Moments
              loc <- VM.new maxOrder :: IO Coefficients
              forM_ [0..maxOrder-1] $ \k -> do
                VM.write mom k $ (0,0,0)
                VM.write loc k $ (0,0,0)

              -- for each parent cell
              forM_ pIndices $ \pIdx@(pi,pj) -> do
                let pzc = getPosition (cw/2) (ch/2) pIdx
                    pMom = m_moments $ getCell pTable pIdx
                forM_ [0..maxOrder-1] $ \k -> do
                  mk <- VM.read mom k
                  ts <- forM [0..k] $ \t -> do
                    a <- VM.read pMom t
                    let r = -(funR (k-t) (zc-pzc))
                    return $ mulValue3 a r
                  VM.write mom k $ sumValue3 (mk:ts)

              return $ HM.insert idx (Cell mom loc) table

        child <- foldM (reduceTable pTable cw ch) emptyTable indices
        return $ child : tables

  tables <- foldM buildTables initialTables [1..maxLevel]

  -- Downward propagation
  let pairs = assert (maxLevel > 2) $ P.zip (P.drop 2 tables) (P.drop 3 tables)
      toChildIndex (i,j) = (i `div` 2, j `div` 2)
      findProperIndices idx = P.filter isProper
        where isNeighbor (xi,xj) (yi,yj) = (abs (xi-yi) < 2) && (abs (xj-yj) < 2)
              isProper i = (not $ isNeighbor idx i)
                        && (isNeighbor (toChildIndex idx) (toChildIndex i))

      translateM2L level table = do
        let nx = nxAtLevel level
            ny = nyAtLevel level
            cw = cwAtLevel level
            ch = chAtLevel level
            indices = [(i,j) | i <- [0..nx-1], j <- [0..ny-1]]

        -- For each cell, translate the moments of proper cells to its 
        -- local coefficients using M2L formula
        forM_ indices $ \i -> do
          let pIndices = findProperIndices i indices
              zl = getPosition cw ch i
              loc = m_loccoef $ getCell table i

          forM_ pIndices $ \j -> do
            let zc = getPosition cw ch j
                mom = m_moments $ getCell table j

            forM_ [0..maxOrder-1] $ \t -> do
              sa <- forM [0..maxOrder-1] $ \k -> do
                a <- VM.read mom k
                let s = funS (k+t) (zl-zc)
                return $ mulValue3 a s
              let c = if odd t then 1/(2*pi) else (-1)/(2*pi) :: Double
              VM.write loc t $ mulValue3 (sumValue3 sa) (fromDouble c)

      translateL2L level child parent = do
        let nx = nxAtLevel $ level + 1
            ny = nyAtLevel $ level + 1
            ccw = cwAtLevel level
            cch = chAtLevel level
            pcw = cwAtLevel $ level + 1
            pch = cwAtLevel $ level + 1
            indices = [(i,j) | i <- [0..nx-1], j <- [0..ny-1]]

        -- For each cell in parent, translate child's local coefficients 
        -- to it using L2L formula
        forM_ indices $ \i -> do
          let j = toChildIndex i
              czl = getPosition ccw cch j
              pzl = getPosition pcw pch i
              cloc = m_loccoef $ getCell child j
              ploc = m_loccoef $ getCell parent i

          forM_ [0..maxOrder-1] $ \s -> do
            lr <- forM [0..maxOrder-1-s] $ \t -> do
              l <- VM.read cloc (s+t)
              let r = funR t (pzl-czl)
              return $ mulValue3 l r
            VM.write ploc s $ mulValue3 (sumValue3 lr) (fromDouble (-1))

      translateMoments (level, (child, parent)) = do
        translateM2L (level+1) parent
        translateL2L level child parent

  translateM2L 2 (tables !! 2)
  mapM_ translateMoments $ P.zip [2..] pairs

  printTable nx ny table

  let calcResult :: IO (VU.Vector Double3)
      calcResult = do
        let f idx = do
              let i = idx `div` nx
                  j = idx `rem` nx
                  loc = m_loccoef $ getCell table (i,j)
              lr <- forM [0..maxOrder-1] $ \i -> do
                l <- VM.read loc i
                let r = funR i (fromDouble 0)
                return $ mulValue3 l r
              return $ fromValue3 $ sumValue3 lr
        VU.generateM (nx*ny) f

  result <- calcResult
  print result
  vectorToPng nx ny result "result.png"

  print $ VU.map (\(x,y,z)->(255*x,255*y,255*z)) result

  return ()

------------------------------------------------------------

test = do
  let prop_nearestPower1 x = (x >= 0) ==> nearestPower x >= x
      prop_nearestPower2 n = (n >= 0 && n < 32) ==> nearestPower t == t where t = 2^n

  quickCheck (prop_nearestPower1 :: Int -> Property)
  quickCheck (prop_nearestPower2 :: Int -> Property)

