module Main where

import System.Environment

import FileSig

handleArgs :: [String] -> IO ()
handleArgs []                = error "You must supply arguments!"
handleArgs [filePathToCheck] = do
  magic <- decodedJSON
  match <- signatureMatch magic filePathToCheck
  prettyMatchPrint magic match
handleArgs _                 = error "Only one argument, please!"

main :: IO ()
main = do
  args <- getArgs
  handleArgs args
