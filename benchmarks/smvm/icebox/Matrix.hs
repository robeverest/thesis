{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Matrix where

import MatrixMarket

import Control.DeepSeq
import Data.Int
import Data.Vector.Storable                                         ( Vector, Storable )
import GHC.Generics
import System.IO.Unsafe
import System.Random.MWC
import qualified Data.Vector.Storable                               as S
import qualified Data.Vector.Unboxed                                as U
import qualified Data.Vector.Algorithms.Intro                       as V


data CSRMatrix a =
  CSRMatrix { csr_segd_length :: !(Vector Int64)
            , csr_segd_offset :: !(Vector Int64)
            , csr_indices     :: !(Vector Int64)
            , csr_values      :: !(Vector a)
            , csr_dim         :: !(Int,Int)
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
readCSRMatrix gen file =
  matrixToCSR gen =<< readMatrix file


-- Convert data read from MatrixMarket format into compressed-sparse-row format
-- with zero-based indexing.
--
{-# INLINE matrixToCSR #-}
matrixToCSR
    :: GenIO
    -> Matrix
    -> IO (CSRMatrix Double)
matrixToCSR _ (RealMatrix !dim !n !structure entries)
  = return
  $ case structure of
      General  ->
        let go []               = []
            go (!(!r,!c,!v):xs) =
              let !r' = fromIntegral (r-1)
                  !c' = fromIntegral (c-1)
              in
              (r',c',v) : go xs
        in
        toCSR dim n (go entries)

      _        ->
        let go []               = []
            go (!(!r,!c,!v):xs) =
              let !r' = fromIntegral (r-1)
                  !c' = fromIntegral (c-1)
                  xs' = go xs
              in if r' == c'
                   then (r',c',v)             : xs'
                   else (r',c',v) : (c',r',v) : xs'
        in
        toCSR dim (n*2) (go entries)

matrixToCSR gen (PatternMatrix !dim !n !structure entries)
  = case structure of
      General -> do
        let go []            = return []
            go (!(!r,!c):xs) = do
              let !r' = fromIntegral (r-1)
                  !c' = fromIntegral (c-1)
              !v  <- uniform gen
              xs' <- unsafeInterleaveIO (go xs)
              return $ (r',c',v) : xs'
        --
        entries' <- go entries
        return $ toCSR dim n entries'

      _       -> do
        let go []            = return []
            go (!(!r,!c):xs) = do
              let !r' = fromIntegral (r-1)
                  !c' = fromIntegral (c-1)
              !v  <- uniform gen
              !u  <- uniform gen
              xs' <- unsafeInterleaveIO (go xs)
              if r' == c'
                then return $ (r',c',v)             : xs'
                else return $ (r',c',v) : (c',r',u) : xs'

        entries' <- go entries
        return $ toCSR dim (n*2) entries'

matrixToCSR _ ComplexMatrix{} = error "matrixToCSR: complex matrices not supported"
matrixToCSR _ IntMatrix{}     = error "matrixToCSR: integer matrices not supported"


{-# INLINE toCSR #-}
toCSR :: (Int,Int)                        -- matrix dimensions
      -> Int                              -- #non-zero elements (hint)
      -> [(Int64,Int64,Double)]           -- (row,column,value)
      -> CSRMatrix Double
toCSR dim@(!r,!_) !n entries =
  let cmp (r1,c1,_) (r2,c2,_)
        | r1 == r2  = compare c1 c2
        | otherwise = compare r1 r2

      !sorted       = U.create $ do
                        x <- U.unsafeThaw $!! U.fromListN n entries
                        V.sortBy cmp x
                        return x

      (!rx,!ix,!vs) = U.unzip3 sorted
      !segd_len     = S.unfoldrN r (\v -> case U.null v of
                                            True  -> Nothing
                                            False -> let (h,t) = U.span (== U.unsafeHead v) v
                                                     in  Just (fromIntegral (U.length h), t)) rx

      !segd_off     = S.scanl (+) 0 segd_len
  in
  CSRMatrix { csr_segd_length = segd_len
            , csr_segd_offset = segd_off
            , csr_indices     = S.convert ix
            , csr_values      = S.convert vs
            , csr_dim         = dim
            }

