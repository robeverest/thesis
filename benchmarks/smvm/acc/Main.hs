
module Main where

import SMVM
import Matrix

import Control.Monad
import Criterion.Main
import Prelude                                          as P
import System.Directory
import System.Environment
import System.Exit
import System.Random.MWC
import Text.Printf
import qualified Data.Vector.Storable                   as S

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.IO                         as A
import Data.Array.Accelerate.LLVM.Native                as CPU
import Data.Array.Accelerate.LLVM.PTX                   as PTX

import Data.Array.Accelerate.Debug                      ( accInit )


usage :: String
usage = unlines
  [ "Usage: smvm-acc matrix.mtx [criterion options] +RTS -Nwhatever -RTS"
  ]


main :: IO ()
main = do
  accInit

  argv    <- getArgs
  when (P.null argv) $ do
    putStrLn usage
    exitSuccess

  let matrixFile    = P.head argv
      criterionOpts = P.tail argv

  yes     <- doesFileExist matrixFile
  unless yes $ do
    printf "File does not exist: %s\n" matrixFile
    putStrLn usage
    exitFailure

  -- Read matrix and convert to CSR format
  -- If this is a pattern matrix also generate random values
  csr     <- withSystemRandom $ \gen -> readCSRMatrix gen matrixFile

  -- generate random vector
  xs      <- withSystemRandom $ \gen -> uniformVector gen (rows csr) :: IO (S.Vector Double)

  -- Convert to Accelerate arrays
  --
  let vec       = fromVectors (Z :. S.length xs) xs
      segd      = fromVectors (Z :. S.length (csr_segd_length csr)) (csr_segd_length csr)
      svec      = fromVectors (Z :. nnz csr) (((), csr_indices csr), csr_values csr)
      smat      = use (segd, svec)  :: Acc (SparseMatrix Double)

      smat'     = streamIn $ go 0
        where
          go i | i P.>= S.length (csr_segd_length csr) = []
               | otherwise                             = srow : go (i+1)
                  where
                    start = P.fromIntegral (csr_segd_offset csr S.! i)
                    len   = P.fromIntegral (csr_segd_length csr S.! i)
                    --
                    srow :: SparseVector Double
                    srow  = fromVectors (Z :. len) (((), S.slice start len (csr_indices csr)), S.slice start len (csr_values csr))

  printf "input matrix: %s\n" matrixFile
  printf "        size: %d x %d\n" (rows csr) (columns csr)
  printf "   non-zeros: %d\n\n" (nnz csr)

  -- Benchmark
  --
  withArgs criterionOpts $
    defaultMain
      [ bgroup "smvm"
        [ bgroup "accelerate-llvm-native"
          [ bench "foldSeg"   $ whnf (CPU.run1 (smvm smat))    vec
          , bgroup "sequences"
            [ bench "fromShapes" $ whnf (CPU.run1 (smvmSeq  smat)) vec
            , bench "streamIn"   $ whnf (CPU.run1 (smvmSeq' smat')) vec
            ]
          ]

        , bgroup "accelerate-llvm-ptx"
          [ bench "foldSeg"   $ whnf (PTX.run1 (smvm smat))    vec
          , bgroup "sequences"
            [ bench "fromShapes" $ whnf (PTX.run1 (smvmSeq  smat)) vec
            , bench "streamIn"   $ whnf (PTX.run1 (smvmSeq' smat')) vec
            ]
          ]
        ]
      ]

