module Data.Builder.ST.Internal
  ( Buf#
  , newBuf#
  , ByteBuilder (..)
  , toByteString
  , TextBuilder (..)
  , toText
  , fromChar
  ) where

import Data.Char
import Data.ByteString.Internal
import Data.Text ()
import Data.Text.Array
import Data.Text.Internal
import Data.Text.Internal.Encoding.Utf8
import GHC.Exts
import GHC.ForeignPtr
import GHC.ST
import GHC.IO
import GHC.Word

-- | An unboxed buffer type, pairing a length with a mutable array.
--
-- When the length plus the length of the desired addition is greater than or equal to the capacity of the array, a new array of double the size is allocated.
type Buf# s = (# Int#, MutableByteArray# s #)

-- | Allocates a new `Buf#`.
newBuf# :: Int# -- ^ The starting length value.
        -> Int# -- ^ The capacity of the `MutableByteArray#` to allocate.
        -> State# s -- ^ A state token, as found inside `ST` or `IO`.
        -> (# State# s, Buf# s #)
newBuf# len cap s =
  let !(# s', arr #) = newByteArray# cap s in
  (# s', (# len, arr #) #)
{-# INLINE newBuf# #-}

{-
require# :: Int# -> Buf# s -> State# s -> (# State# s, Buf# s #)
require# l buf@(# len, arr #) s
  | isTrue# (l +# len <# sizeOfMutableArray# arr #) = (# s, buf #)
  | otherwise = undefined
{-# INLINE require# #-}

-}

-- | An efficient `ByteString` builder.
newtype ByteBuilder = ByteBuilder (forall s. Buf# s -> State# s -> (# State# s, Buf# s #))
  deriving (IsString) via TextBuilder

instance Show ByteBuilder where
  show = show . toByteString

instance Semigroup ByteBuilder where
  ByteBuilder l <> ByteBuilder r = ByteBuilder $ \buf s -> let !(# s', buf' #) = l buf s in r buf' s'
  {-# INLINE (<>) #-}

instance Monoid ByteBuilder where
  mempty = ByteBuilder $ \buf s -> (# s, buf #)
  {-# INLINE CONLIKE mempty #-}

-- | Convert a `ByteBuilder` to a `ByteString`.
toByteString :: ByteBuilder -> ByteString
toByteString (ByteBuilder b) = unsafePerformIO $ IO (\s ->
  let !(# s', buf #) = newBuf# 0# 32# s in
  let !(# s'', (# len, arr #) #) = b buf s' in
  let !(# s''', arr' #) = unsafeFreezeByteArray# arr s'' in
  (# s''', BS (ForeignPtr (byteArrayContents# arr') (PlainPtr arr)) (I# len) #))

-- | An efficient `Text` builder.
newtype TextBuilder = TextBuilder (forall s. Buf# s -> State# s -> (# State# s, Buf# s #))
  deriving (Semigroup, Monoid) via ByteBuilder

instance Show TextBuilder where
  show = show . toText

instance IsString TextBuilder where
  fromString = foldMap fromChar

-- | Create a `TextBuilder` that appends a `Char`.
fromChar :: Char -> TextBuilder
fromChar !c = TextBuilder $ go (unI# $ utf8Length c) c
  where
    go :: Int# -> Char -> Buf# s -> State# s -> (# State# s, Buf# s #)
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
        let !(# s', buf'@(# _, arr' #) #) = newBuf# len (sizeofMutableByteArray# arr *# 2#) s in
        let s'' = copyMutableByteArray# arr 0# arr' 0# len s' in
        go cl c buf' s''

-- | Convert a `TextBuilder` to a `Text`.
toText :: TextBuilder -> Text
toText (TextBuilder b) = runST $ ST (\s ->
  let !(# s', buf #) = newBuf# 0# 32# s in
  let !(# s'', (# len, arr #) #) = b buf s' in
  let !(# s''', arr' #) = unsafeFreezeByteArray# arr s'' in
  (# s''', text (ByteArray arr') 0 (I# len) #))

{-
-- Internal Utility Functions
-}

unI# :: Int -> Int#
unI# (I# i) = i
{-# INLINE unI# #-}
