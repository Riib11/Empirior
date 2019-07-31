module Debug where

import           System.IO.Unsafe

debug :: Show a => a -> ()
debug = unsafePerformIO.putStrLn.("[>] "++).show
