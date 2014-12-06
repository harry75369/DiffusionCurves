import System.Environment
import Data.Complex
import qualified Data.Array.Repa as R
import           Data.Array.Repa (Z (..), (:.) (..))
import qualified Data.Vector     as V

list :: Int -> V.Vector (Complex Double)
list n = V.fromList [fromIntegral i :+ fromIntegral i | i <- [1..n]]

test1 :: V.Vector (Complex Double) -> IO (R.Array R.U R.DIM2 Double)
test1 l = do
  let n = V.length l
      f = R.fromFunction (Z:.n:.n) $ \(Z:.i:.j) -> do
        let x = l V.! i
            y = l V.! j
         in magnitude $ x - y

  R.computeP $ f :: IO (R.Array R.U R.DIM2 Double)

test2 :: V.Vector (Complex Double) -> R.Array R.U R.DIM2 Double
test2 l =
  let n = V.length l
      f = R.fromFunction (Z:.n:.n) $ \(Z:.i:.j) -> do
        let x = l V.! i
            y = l V.! j
         in magnitude $ x - y

   in R.computeS $ (f :: R.Array R.D R.DIM2 Double)

main :: IO ()
main = do
  n <- fmap head getArgs >>= return.read :: IO Int
  let l = list n
  result <- test1 l
  {-let result = test2 l-}
  print result
  return ()


