module Main where

import Codes

main =  putStrLn $ "An error code that is not currently used is:"
                 ++ show (maximum codes + 1)
