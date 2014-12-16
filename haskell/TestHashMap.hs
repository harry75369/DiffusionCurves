import Prelude as P
import Data.Complex
import Control.Monad
import Control.Monad.Primitive
import Data.HashMap.Lazy             as HM
import Data.Vector.Unboxed.Mutable   as VM
import Text.Printf

data Cell = Cell {
  m_moments :: Moments
}

data Segment = Segment {
  m_value :: Value
, m_cell  :: Index
} deriving (Show)

type Index     = (Int, Int)
type Value     = Complex Double
type Moments   = VM.MVector RealWorld Value
type CellTable = HM.HashMap Index Cell

test = do
  let emptyTable = HM.empty :: CellTable
      n     = 2
      order = 3
      segments = [Segment (fromIntegral i :+ fromIntegral j) (i, j) | i<-[0..n-1], j<-[0..n-1]]
      buildTable :: CellTable -> Segment -> IO CellTable
      buildTable table segment = do
        let idx = m_cell segment
        case HM.lookup idx table of
          Nothing -> do
            mom <- VM.new order :: IO Moments
            forM_ [0..order-1] $ \k -> do
              VM.write mom k (fromIntegral k :+ fromIntegral k)
            return $ HM.insert idx (Cell mom) table
          Just cell -> do
            let mom = m_moments cell
            forM_ [0..order-1] $ \k -> do
              v <- VM.read mom k
              VM.write mom k (2*v)
            return table
      buildTables :: [CellTable] -> [Segment] -> IO [CellTable]
      buildTables tables@(t:_) segments = do
        newt <- foldM buildTable t segments
        return $ newt : tables

  -- The goal of this test is to verify modification to the value of pair (key, value) 
  -- of an hashmap will persist.
  tables <- foldM buildTables [emptyTable] $ P.replicate 4 segments

  let printTable table = do
        printf "-----------------\n"
        forM_ [0..n-1] $ \i -> do
          forM_ [0..n-1] $ \j -> do
            let Just cell = HM.lookup (i,j) $ head tables
                moments = m_moments cell
            printf "Cell (%d,%d):\n" i j
            forM_ [0..order-1] $ \k -> do
              VM.read moments k >>= print

  mapM_ printTable tables

main = do
  putStrLn "Testing HashMap..."
  test
