module Main where

import System.Environment

import FileSig

handleArgs :: [String] -> IO ()
handleArgs []                = error "You must supply arguments!"
handleArgs [filePathToCheck] =
  signatureMatch filePathToCheck >>= prettyMatchPrint
handleArgs _                 = error "Only one argument, please!"

main :: IO ()
main = getArgs >>= handleArgs
