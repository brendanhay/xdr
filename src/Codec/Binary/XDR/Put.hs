module Codec.Binary.XDR.Put where

import Data.Binary.Put
import Data.Binary.IEEE754
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as BL

putVoid :: Put
putVoid = return ()

putInt :: Int32 -> Put
putInt = putWord32be . fromIntegral

putUnsignedInt :: Word32 -> Put
putUnsignedInt = putWord32be

putEnum :: Enum a => a -> Put
putEnum = putInt . fromIntegral . fromEnum

putEnumBy :: (a -> Maybe Int32) -> a -> Put
putEnumBy f i = case f i of
    Nothing -> fail "putEnumBy: enum tag lookup failed"
    Just t  -> putInt t

putBool :: Bool -> Put
putBool = putEnum

putHyper :: Int64 -> Put
putHyper = putWord64be . fromIntegral

putUnsignedHyper :: Word64 -> Put
putUnsignedHyper = putWord64be

putFloat :: Float -> Put
putFloat = putFloat32be

putDouble :: Double -> Put
putDouble = putFloat64be

putFixedOpaque :: Word32 -> BL.ByteString -> Put
putFixedOpaque n bs
    | toInteger len /= toInteger n
        = fail "putFixedOpaque: supplied bytestring has incorrect length"
    | otherwise = do
        putLazyByteString bs
        case fromIntegral len .&. 3 of
            0 -> return ()
            m -> putByteString (BS.replicate (4-m) 0)
    where
      len = BL.length bs

putVariableOpaque :: Maybe Word32 -> BL.ByteString -> Put
putVariableOpaque mbLen bs = case mbLen of
    Just n | toInteger len > toInteger n
        -> fail "putVariableOpaque: supplied bytestring is too long"
    _   -> do
        putUnsignedInt (fromIntegral len)
        putLazyByteString bs
        case fromIntegral len .&. 3 of
            0 -> return ()
            m -> putByteString (BS.replicate (4-m) 0)
    where
      len = BL.length bs

putString :: Maybe Word32 -> String -> Put
putString word = putVariableOpaque word . BL.pack

putFixedArray :: (a -> Put) -> Word32 -> [a] -> Put
putFixedArray putItem n items = loop n items
    where
        loop 0 [] = return ()
        loop 0 _  = fail "putFixedArray: too many items"
        loop n [] = fail "putFixedArray: too few items"
        loop n (x:xs) = putItem x >> loop (n-1) xs

putVariableArray :: (a -> Put) -> Maybe Word32 -> [a] -> Put
putVariableArray putItem Nothing     items = putVariableArray putItem (Just maxBound) items
putVariableArray putItem (Just maxN) items = case loop 0 (return ()) items of
    Right (nItems, putItems) -> putUnsignedInt nItems >> putItems
    Left err                 -> fail err
    where
        loop n put [] = Right (n, put)
        loop n put (x:xs)
            | n >= maxN = Left "putVariableArray: too many items"
            | otherwise = loop (n+1) (put >> putItem x) xs

putStruct :: [Put] -> Put
putStruct = sequence_

putUnion :: Enum tag => (a -> tag) -> (a -> Put) -> a -> Put
putUnion tagItem putItem item = do
    putEnum (tagItem item)
    putItem item

putUnionBy :: (a -> Maybe Int32) -> (a -> Put) -> a -> Put
putUnionBy tagItem putItem item = do
    putEnumBy tagItem item
    putItem item

putOptional :: (a -> Put) -> Maybe a -> Put
putOptional putThing Nothing = putBool False
putOptional putThing (Just thing) = putBool True >> putThing thing
