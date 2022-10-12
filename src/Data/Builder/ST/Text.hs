module Data.Builder.ST.Text
  ( TextBuilder
  , toText
  , fromChar
  , fromText
  , unsafeFromByteBuilder
  ) where

import Data.Builder.ST.Internal
import Data.Text.Array (Array (..))
import Data.Text.Internal (Text (..))
import GHC.Exts

-- | Create a `TextBuilder` from a `Text`.
fromText :: Text -> TextBuilder
fromText (Text (ByteArray t_arr) (I# t_off) (I# t_len)) = TextBuilder $ go t_arr t_off t_len
  where
    go :: ByteArray# -> Int# -> Int# -> Buf# s -> State# s -> (# State# s, Buf# s #)
    go t_arr t_off t_len (# len, arr #) s
      | isTrue# (len +# t_len <=# sizeofMutableByteArray# arr) =
        (# copyByteArray# t_arr t_off arr len t_len s, (# len +# t_len, arr #) #)
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf# len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go t_arr t_off t_len buf' s''

-- | Create a `TextBuilder` from a `ByteBuilder`. This is a zero-cost operation because the two are identical internally.
--
-- This is unsafe because the bytes may not be valid UTF-8 strings. Going in the other direction, via `Data.Builder.ST.ByteString.fromTextBuilder`, is perfectly safe.
unsafeFromByteBuilder :: ByteBuilder -> TextBuilder
unsafeFromByteBuilder (ByteBuilder f) = TextBuilder f
{-# INLINE CONLIKE unsafeFromByteBuilder #-}
