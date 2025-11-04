module GHC.IO.Exception where
import System.IO.Error

unsupportedOperation :: IOError
unsupportedOperation =
   (IOError Nothing UnsupportedOperation ""
        "Operation is not supported" Nothing Nothing)

