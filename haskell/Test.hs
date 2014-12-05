import Data.Complex
import           Data.Array.Repa ((:.) (..), Z (..))
import qualified Data.Array.Repa as R
import qualified Data.Vector     as V

segs :: [Complex Double]
segs = [i :+ i | i <- [1..100]]

test segs = do
  let n     = length segs
      ssegs = V.fromList segs

  R.computeP $ R.fromFunction (Z:.n:.n) $ \(Z:.i:.j) -> do
    let x = ssegs V.! i
        y = ssegs V.! j
     in magnitude $ x - y

