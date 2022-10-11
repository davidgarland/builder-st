module Data.Builder.ST.ByteString
  ( ByteBuilder
  , toByteString
  , fromTextBuilder
  , fromWord8#
  , fromWord8
  , fromByteString
  , unsafeFromAddr#
  ) where

import Data.Builder.ST.Internal
import Data.ByteString.Internal (ByteString (..))
import GHC.ForeignPtr (ForeignPtr (..))
import GHC.Exts
import GHC.Word

-- | Create a `ByteBuilder` from a `TextBuilder`. This is a zero-cost operation because the two are identical internally.
--
-- Going the other direction is possible but unsafe: see `Data.Builder.ST.Text.unsafeFromByteBuilder`.
fromTextBuilder :: TextBuilder -> ByteBuilder
fromTextBuilder (TextBuilder f) = ByteBuilder f
{-# INLINE CONLIKE fromTextBuilder #-}

-- | Create a `ByteBuilder` that appends a `Word8#`.
fromWord8# :: Word8# -> ByteBuilder
fromWord8# w = ByteBuilder $ go w
  where
    go :: Word8# -> Buf# s -> State# s -> (# State# s, Buf# s #)
    go w (# len, arr #) s
      | isTrue# (len <# sizeofMutableByteArray# arr) = (# writeWord8Array# arr len w s, (# len +# 1#, arr #) #)
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf# len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go w buf' s''

-- | Create a `ByteBuilder` that appends a `Word8`.
fromWord8 :: Word8 -> ByteBuilder
fromWord8 (W8# w) = fromWord8# w
{-# INLINE fromWord8 #-}

-- | Create a `ByteBuilder` that appends a `ByteString`.
fromByteString :: ByteString -> ByteBuilder
fromByteString (BS (ForeignPtr a _) (I# al)) = ByteBuilder $ go a al
  where
    go :: Addr# -> Int# -> Buf# s -> State# s -> (# State# s, Buf# s #)
    go a al (# len, arr #) s
      | isTrue# (len +# al <=# sizeofMutableByteArray# arr) =
        (# copyAddrToByteArray# a arr len al s, (# len +# al, arr #) #)
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf# len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go a al buf' s''

-- | Create a `ByteBuilder` that appends an `Addr#`.
--
-- This is unsafe because it relies on the fact that the address points to a null-terminated string.
unsafeFromAddr# :: Addr# -> ByteBuilder
unsafeFromAddr# a = ByteBuilder $ go a (cstringLength# a)
  where
    go :: Addr# -> Int# -> Buf# s -> State# s -> (# State# s, Buf# s #)
    go a al (# len, arr #) s
      | isTrue# (len +# al <=# sizeofMutableByteArray# arr) =
        (# copyAddrToByteArray# a arr len al s, (# len +# al, arr #) #)
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf# len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go a al buf' s''

