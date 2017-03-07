{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Matrix

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.Char
import Data.Int
import Data.Vector.Storable                                         ( Vector )
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.Environment
import System.Exit
import System.Random.MWC
import Text.Printf
import qualified Data.Vector.Storable                               as S

#include <mkl.h>


usage :: String
usage = unlines
  [ "Usage: env MKL_NUM_THREADS=N smvm-mkl matrix.mtx [criterion options]"
  ]


main :: IO ()
main = do
  argv    <- getArgs
  when (null argv) $ do
    putStrLn usage
    exitSuccess

  let matrixFile    = head argv
      criterionOpts = tail argv

  yes     <- doesFileExist matrixFile
  unless yes $ do
    printf "File does not exist: %s\n" matrixFile
    putStrLn usage
    exitFailure

  -- Read matrix and convert to CSR format
  -- If this is a pattern matrix also generate random values
  csr     <- withSystemRandom $ \gen -> readCSRMatrix gen matrixFile

  -- generate random vector
  vec     <- withSystemRandom $ \gen -> uniformVector gen (rows csr) :: IO (Vector Double)

  csr `deepseq` vec `deepseq` return ()
  printf "input matrix: %s\n" matrixFile
  printf "        size: %d x %d\n" (rows csr) (columns csr)
  printf "   non-zeros: %d\n\n" (nnz csr)

  -- benchmark the operation
  withArgs criterionOpts $
    defaultMain
      [ bgroup "smvm"
        [ bench "mkl" $ nfIO (mkl_cspblas_dcsrgemv csr vec)
        ]
      ]


-- Matrix-vector product of sparse general matrix and dense vector in CSR
-- (3-array variant) format with zero-based indexing.
--
-- <https://software.intel.com/en-us/node/520815#D840F0E5-E41A-4E91-94D2-FEB320F93E91>
--
-- void mkl_cspblas_dcsrgemv
--   ( const char *transa
--   , const MKL_INT *m
--   , const double *a
--   , const MKL_INT *ia
--   , const MKL_INT *ja
--   , const double *x
--   , double *y
--   )
--
mkl_cspblas_dcsrgemv
    :: CSRMatrix Double
    -> Vector Double
    -> IO (Vector Double)
mkl_cspblas_dcsrgemv csr vec = do
  fp <- mallocForeignPtrArray (rows csr)
  let y = S.unsafeFromForeignPtr0 fp (rows csr)
  mkl_cspblas_dcsrgemv' 'N' (rows csr) (csr_values csr) (csr_segd_offset csr) (csr_indices csr) vec y
  return y
  where
    {# fun unsafe mkl_cspblas_dcsrgemv as mkl_cspblas_dcsrgemv'
      { withCharConv* `Char'
      , withIntConv*  `Int'
      , withVecD*     `Vector Double'
      , withVecI*     `Vector Int32'
      , withVecI*     `Vector Int32'
      , withVecD*     `Vector Double'
      , withVecD*     `Vector Double'
      } -> `()' #}
      where
        withCharConv :: Char -> (Ptr CChar -> IO a) -> IO a
        withCharConv = with . fromIntegral . ord

        withIntConv :: (Storable b, Integral a, Integral b) => a -> (Ptr b -> IO c) -> IO c
        withIntConv = with . fromIntegral

        withVecD :: Vector Double -> (Ptr CDouble -> IO a) -> IO a
        withVecD v f = S.unsafeWith v (f . castPtr)

        withVecI :: Vector Int32 -> (Ptr CInt -> IO a) -> IO a
        withVecI v f = S.unsafeWith v (f . castPtr)

