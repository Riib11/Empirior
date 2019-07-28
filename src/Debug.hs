module Debug where

import           System.IO.Unsafe

debug :: String -> ()
debug = unsafePerformIO.putStrLn.("[>] "++)
