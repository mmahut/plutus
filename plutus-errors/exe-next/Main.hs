module Main where

import Codes

-- | Executable to help developers by returning a currently-unused error code
main :: IO ()
main =  putStrLn $ "An error code that is not currently in-use is: "
                 ++ show (maximum allCodes + 1)
