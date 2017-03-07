{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Matrix

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Data.Int
import Data.Vector.Storable                                         ( Vector )
import Foreign.C.Types
import Foreign.CUDA.Ptr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.Directory
import System.Environment
import System.Exit
import System.Random.MWC
import Text.Printf
import qualified Data.Vector.Storable                               as S
import qualified Foreign.CUDA.Driver                                as CUDA

#include <cusparse.h>


usage :: String
usage = unlines
  [ "Usage: smvm-cuda matrix.mtx [criterion options]"
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

  CUDA.initialise []
  dev <- CUDA.device 0
  ctx <- CUDA.create dev []
  hdl <- createHandle

  CUDA.allocaArray (nnz csr)          $ \d_A  -> do
   CUDA.allocaArray (columns csr)     $ \d_x  -> do
    CUDA.allocaArray (S.length (csr_segd_offset csr)) $ \d_iA -> do
     CUDA.allocaArray (nnz csr)           $ \d_jA -> do
       S.unsafeWith (csr_values csr)      $ \p -> CUDA.pokeArray (nnz csr) p d_A
       S.unsafeWith (csr_indices csr)     $ \p -> CUDA.pokeArray (nnz csr) p d_jA
       S.unsafeWith (csr_segd_offset csr) $ \p -> CUDA.pokeArray (S.length (csr_segd_offset csr)) p d_iA
       S.unsafeWith vec                   $ \p -> CUDA.pokeArray (columns csr) p d_x
       CUDA.sync

       -- benchmark the operation
       withArgs criterionOpts $
         defaultMain
           [ bgroup "smvm"
             [ bench "cuSPARSE" $ nfIO $ do
                 dsc <- createMatrixDescriptor MatrixTypeGeneral IndexBaseZero DiagTypeNonUnit Nothing
                 d_y <- cusparseDcsrmv hdl NonTranspose (rows csr) (columns csr) (nnz csr) 1.0 dsc d_A d_iA d_jA d_x
                 CUDA.sync
                 CUDA.free d_y
                 destroyMatrixDescriptor dsc
                 CUDA.sync
             ]
           ]

  destroyHandle hdl
  CUDA.destroy ctx

  return ()


newtype Handle = Handle { useHandle :: {# type cusparseHandle_t #}}
  deriving (Eq, Show)

newtype MatrixDescriptor = MatDescr { useMatDescr :: {# type cusparseMatDescr_t #}}
  deriving (Eq, Show)

{# enum cusparseStatus_t as Status
    { underscoreToCase }
    with prefix="CUSPARSE_STATUS" deriving (Eq, Show) #}

{# enum cusparseDiagType_t as DiagonalType
    { underscoreToCase }
    with prefix="CUSPARSE" deriving (Eq, Show) #}

{# enum cusparseFillMode_t as FillMode
    { underscoreToCase }
    with prefix="CUSPARSE" deriving (Eq, Show) #}

{# enum cusparseIndexBase_t as IndexBase
    { underscoreToCase }
    with prefix="CUSPARSE" deriving (Eq, Show) #}

{# enum cusparseMatrixType_t as MatrixType
    { underscoreToCase }
    with prefix="CUSPARSE" deriving (Eq, Show) #}

{# enum cusparseOperation_t as Operation
    { underscoreToCase }
    with prefix="CUSPARSE_OPERATION" deriving (Eq, Show, Bounded) #}


-- Level 2
-- -------

-- Matrix-vector product of sparse general matrix with dense vector in CSR
-- format with zero-based indexing.
--
--   y = alpha * op(A) * x + beta * y
--
-- <http://docs.nvidia.com/cuda/cusparse/index.html#cusparse-lt-t-gt-csrmv>
--
-- cusparseDcsrmv
--   ( cusparseHandle_t handle
--   , cusparseOperation_t transA
--   , int m
--   , int n, int nnz
--   , const double *alpha
--   , const cusparseMatDescr_t descrA
--   , const double *csrValA
--   , const int *csrRowPtrA
--   , const int *csrColIndA
--   , const double *x
--   , const double *beta
--   , double *y
--   )
--
cusparseDcsrmv
    :: Handle
    -> Operation
    -> Int                        -- #rows of A
    -> Int                        -- #cols of A
    -> Int                        -- #non-zero elements in A
    -> Double                     -- alpha
    -> MatrixDescriptor
    -> DevicePtr Double           -- A
    -> DevicePtr Int32            -- 'scanl (+) 0' of row lengths
    -> DevicePtr Int32            -- column index of non-zero element
    -> DevicePtr Double           -- x
    -> IO (DevicePtr Double)
cusparseDcsrmv h op m n nnz alpha descrA a iA jA x = do
  y <- case op of
         NonTranspose -> CUDA.mallocArray m
         _            -> CUDA.mallocArray n
  _ <- nothingIfOk =<< cusparseDcsrmv' h op m n nnz alpha descrA a iA jA x 0 y
  return y
  where
  {# fun unsafe cusparseDcsrmv as cusparseDcsrmv'
      { useHandle      `Handle'
      , cFromEnum      `Operation'
      , cIntConv       `Int'
      , cIntConv       `Int'
      , cIntConv       `Int'
      , withFloatConv* `Double'
      , useMatDescr    `MatrixDescriptor'
      , useDevicePtrD  `DevicePtr Double'
      , useDevicePtrI  `DevicePtr Int32'
      , useDevicePtrI  `DevicePtr Int32'
      , useDevicePtrD  `DevicePtr Double'
      , withFloatConv* `Double'
      , useDevicePtrD  `DevicePtr Double'
      }
      -> `Status' cToEnum #}
      where
        useDevicePtrD :: DevicePtr Double -> Ptr CDouble
        useDevicePtrD = castPtr . useDevicePtr

        useDevicePtrI :: DevicePtr Int32 -> Ptr CInt
        useDevicePtrI = castPtr . useDevicePtr


-- Context management
-- ------------------

createHandle :: IO Handle
createHandle = resultIfOk =<< cusparseCreate
  where
  {# fun unsafe cusparseCreate
    { alloca- `Handle' peekH* } -> `Status' cToEnum #}
    where
      peekH = liftM Handle . peek

destroyHandle :: Handle -> IO ()
destroyHandle h = nothingIfOk =<< cusparseDestroy h
  where
  {# fun unsafe cusparseDestroy
    { useHandle `Handle' } -> `Status' cToEnum #}

createMatrixDescriptor :: MatrixType -> IndexBase -> DiagonalType -> Maybe FillMode -> IO MatrixDescriptor
createMatrixDescriptor matrixType indexBase diagType mfillMode = do
  matDescr <- resultIfOk =<< cusparseCreateMatDescr
  nothingIfOk =<< cusparseSetMatType matDescr matrixType
  nothingIfOk =<< cusparseSetMatIndexBase matDescr indexBase
  nothingIfOk =<< cusparseSetMatDiagType matDescr diagType
  case mfillMode of
    Just fillMode -> nothingIfOk =<< cusparseSetMatFillMode matDescr fillMode
    Nothing       -> return ()
  return matDescr
  where
  {# fun unsafe cusparseCreateMatDescr
    { alloca- `MatrixDescriptor' peekMD* } -> `Status' cToEnum #}
    where
      peekMD = liftM MatDescr . peek
  --
  {# fun unsafe cusparseSetMatDiagType
    { useMatDescr `MatrixDescriptor'
    , cFromEnum   `DiagonalType'
    }
    -> `Status' cToEnum #}
  --
  {# fun unsafe cusparseSetMatFillMode
    { useMatDescr `MatrixDescriptor'
    , cFromEnum   `FillMode'
    }
    -> `Status' cToEnum #}
  --
  {# fun unsafe cusparseSetMatIndexBase
    { useMatDescr `MatrixDescriptor'
    , cFromEnum   `IndexBase'
    }
    -> `Status' cToEnum #}
  --
  {# fun unsafe cusparseSetMatType
    { useMatDescr `MatrixDescriptor'
    , cFromEnum   `MatrixType'
    }
    -> `Status' cToEnum #}

destroyMatrixDescriptor :: MatrixDescriptor -> IO ()
destroyMatrixDescriptor m = nothingIfOk =<< cusparseDestroyMatDescr m
  where
  {# fun unsafe cusparseDestroyMatDescr
    { useMatDescr `MatrixDescriptor' } -> `Status' cToEnum #}


-- Error handling
-- --------------
resultIfOk :: (Status, a) -> IO a
resultIfOk (status, result) =
    case status of
        Success -> return result
        _       -> error (show status)

nothingIfOk :: Status -> IO ()
nothingIfOk status =
    case status of
        Success -> return ()
        _       -> error (show status)

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

withIntConv   :: (Storable b, Integral a, Integral b) => a -> (Ptr b -> IO c) -> IO c
withIntConv    = with . cIntConv

withFloatConv :: (Storable b, RealFloat a, RealFloat b) => a -> (Ptr b -> IO c) -> IO c
withFloatConv  = with . cFloatConv

{-# INLINE [1] cFloatConv #-}
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES
  "cFloatConv/Float->Float"    forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double"  forall (x::Double). cFloatConv x = x;
  "cFloatConv/Float->CFloat"   forall (x::Float).  cFloatConv x = CFloat x;
  "cFloatConv/CFloat->Float"   forall (x::Float).  cFloatConv CFloat x = x;
  "cFloatConv/Double->CDouble" forall (x::Double). cFloatConv x = CDouble x;
  "cFloatConv/CDouble->Double" forall (x::Double). cFloatConv CDouble x = x
 #-}

