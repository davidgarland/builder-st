module Data.Text.Builder.ST
  ( fromChar
  , fromText
  , runBuilder
  ) where

import Control.Monad.ST
import Data.Char
import Data.String
import Data.Text ()
import Data.Text.Array (Array (ByteArray))
import Data.Text.Internal (Text (..), text)
import Data.Text.Internal.Encoding.Utf8
import GHC.Exts
import GHC.ST (ST (..))
import GHC.Word (Word8 (..))

unI# :: Int -> Int#
unI# (I# i) = i
{-# INLINE unI# #-}

type Buf s = (# Int#, MutableByteArray# s #)

newBuf :: Int# -> Int# -> State# s -> (# State# s, Buf s #)
newBuf len cap s =
  let !(# s', arr #) = newByteArray# cap s in
  (# s', (# len, arr #) #)
{-# INLINE newBuf #-}

fromChar :: Char -> Builder
fromChar !c = Builder $ go (unI# $ utf8Length c) c
  where
    go :: Int# -> Char -> Buf s -> State# s -> (# State# s, Buf s #)
    go cl !c (# len, arr #) s
      | isTrue# (len +# cl <=# sizeofMutableByteArray# arr) =
        case cl of
          1# -> (# writeWord8Array# arr len (wordToWord8# (int2Word# (unI# (ord c)))) s, (# len +# 1#, arr #) #)
          2# ->
            let !(W8# w1, W8# w2) = ord2 c in
            (# writeWord8Array# arr (len +# 1#) w2
                 (writeWord8Array# arr len w1 s)
             , (# len +# 2#, arr #)
            #)
          3# ->
            let !(W8# w1, W8# w2, W8# w3) = ord3 c in
            (# writeWord8Array# arr (len +# 2#) w3
                 (writeWord8Array# arr (len +# 1#) w2
                   (writeWord8Array# arr len w1 s))
             , (# len +# 3#, arr #)
            #)
          4# ->
            let !(W8# w1, W8# w2, W8# w3, W8# w4) = ord4 c in
            (# writeWord8Array# arr (len +# 3#) w4
                 (writeWord8Array# arr (len +# 2#) w3
                   (writeWord8Array# arr (len +# 1#) w2
                     (writeWord8Array# arr len w1 s)))
             , (# len +# 4#, arr #)
            #)
          _ -> error "unreachable"
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go cl c buf' s''

fromText :: Text -> Builder
fromText (Text (ByteArray t_arr) (I# t_off) (I# t_len)) = Builder $ go t_arr t_off t_len
  where
    go :: ByteArray# -> Int# -> Int# -> Buf s -> State# s -> (# State# s, Buf s #)
    go t_arr t_off t_len (# len, arr #) s
      | isTrue# (len +# t_len <=# sizeofMutableByteArray# arr) =
        (# copyByteArray# t_arr t_off arr len t_len s, (# len +# t_len, arr #) #)
      | otherwise =
        let !(# s', buf'@(# _, arr' #) #) = newBuf len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go t_arr t_off t_len buf' s''

newtype Builder
  = Builder (forall s. Buf s -> State# s -> (# State# s, Buf s #))

instance IsString Builder where
  fromString = foldMap fromChar

instance Show Builder where
  show = show . runBuilder

instance Semigroup Builder where
  Builder l <> Builder r = Builder $ \buf s ->
    case l buf s of
      (# s', buf' #) -> r buf' s'
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = Builder (\buf s -> (# s, buf #))
  {-# INLINE mempty #-}

runBuilder :: Builder -> Text
runBuilder (Builder b) = runST $ ST (\s ->
  let !(# s', buf #) = newBuf 0# 32# s in
  let !(# s'', (# len, arr #) #) = b buf s' in
  let !(# s''', arr' #) = unsafeFreezeByteArray# arr s'' in
  (# s''', text (ByteArray arr') 0 (I# len) #))

