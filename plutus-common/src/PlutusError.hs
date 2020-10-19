module PlutusError
    ( ErrorCode(..)
    ) where

class ErrorCode a where
    errorCode :: a -> Word
    {-# MINIMAL errorCode #-}
