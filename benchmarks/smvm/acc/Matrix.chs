{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Matrix (

  CSRMatrix(..),
  readCSRMatrix,
  nnz, rows, columns,

) where

import Control.Monad
import Control.DeepSeq
import Data.Int
import Data.Vector.Storable                                         ( Vector, Storable )
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics                                                 ( Generic )
import System.Random.MWC
import qualified Data.Vector.Algorithms.Intro                       as V
import qualified Data.Vector.Storable                               as S
import qualified Data.Vector.Unboxed                                as U

#include "matrix_market.h"


data CSRMatrix a =
  CSRMatrix { csr_segd_length :: Vector Int32
            , csr_segd_offset :: Vector Int32
            , csr_indices     :: Vector Int32
            , csr_values      :: Vector a
            , csr_dim         :: (Int,Int)
            }
  deriving (Show, Generic, NFData)

nnz :: Storable a => CSRMatrix a -> Int
nnz = S.length . csr_values

rows :: CSRMatrix a -> Int
rows = fst . csr_dim

columns :: CSRMatrix a -> Int
columns = snd . csr_dim


-- Read the given matrix market file into CSR format. Generate random values for
-- matrix entries if necessary.
--
readCSRMatrix
    :: GenIO
    -> FilePath
    -> IO (CSRMatrix Double)
readCSRMatrix gen file = do
  (rs, cs, iA, jA, mA) <- readMatrix file
  vA <- case mA of
          Just a  -> return (U.convert a)
          Nothing -> uniformVector gen (S.length iA)

  return $ toCSR (rs,cs) $ U.zip3 (U.convert iA) (U.convert jA) vA


toCSR :: (Int,Int)
      -> U.Vector (Int32, Int32, Double)
      -> CSRMatrix Double
toCSR dim@(r,_) entries =
  let cmp (r1,c1,_) (r2,c2,_)
        | r1 == r2  = compare c1 c2
        | otherwise = compare r1 r2

      !sorted       = U.create $ do
                        x <- U.unsafeThaw entries
                        V.sortBy cmp x
                        return x

      (rx,ix,vs)    = U.unzip3 sorted
      segd_len      = S.unfoldrN r (\v -> case U.null v of
                                            True  -> Nothing
                                            False -> let (h,t) = U.span (== U.unsafeHead v) v
                                                     in  Just (fromIntegral (U.length h), t)) rx

      segd_off      = S.scanl (+) 0 segd_len
  in
  CSRMatrix { csr_segd_length = segd_len
            , csr_segd_offset = segd_off
            , csr_indices     = S.convert ix
            , csr_values      = S.convert vs
            , csr_dim         = dim
            }



readMatrix
    :: String
    -> IO (Int, Int, Vector Int32, Vector Int32, Maybe (Vector Double))
readMatrix file = do
  (status, rs, cs, nz, p_iA, p_jA, p_A) <- mm_read_general_real file
  case status of
    Success -> do
      fp_iA <- newForeignPtr finalizerFree p_iA
      fp_jA <- newForeignPtr finalizerFree p_jA
      mfp_A <- if p_A == nullPtr
                 then return Nothing
                 else Just <$> newForeignPtr finalizerFree p_A

      let iA = S.unsafeFromForeignPtr0 fp_iA nz
          jA = S.unsafeFromForeignPtr0 fp_jA nz
          vA = case mfp_A of
                 Nothing -> Nothing
                 Just fp -> Just $ S.unsafeFromForeignPtr0 fp nz

      return (rs, cs, iA, jA, vA)

    _       -> error (show status)
  where
    {# fun unsafe mm_read_general_real
      { withCString*  `String'
      , alloca-       `Int'        peekIntConv*
      , alloca-       `Int'        peekIntConv*
      , alloca-       `Int'        peekIntConv*
      , alloca-       `Ptr Int32'  peekI*
      , alloca-       `Ptr Int32'  peekI*
      , alloca-       `Ptr Double' peekD*
      }
      -> `Status' cToEnum #}
      where
        peekI :: Ptr (Ptr CInt) -> IO (Ptr Int32)
        peekI = liftM castPtr . peek

        peekD :: Ptr (Ptr CDouble) -> IO (Ptr Double)
        peekD = liftM castPtr . peek


{# enum MM_status_t as Status
    { underscoreToCase
    , MM_SUCCESS as Success
    }
    with prefix="MM_STATUS" deriving (Eq, Show) #}


cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM cIntConv . peek

