{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mapM_)
import Control.SentenceJP (generateMessage, GenerateOption (IgnoreSigns, IgnoreAlphaNums))
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  texts    <- Text.lines <$> TIO.readFile "sentences.txt"
  putStr "plain: "
  generateMessage [] texts >>= mapM_ TIO.putStrLn
  putStr "ignore signs: "
  generateMessage [IgnoreSigns] texts >>= mapM_ TIO.putStrLn
  putStr "ignore alphanums: "
  generateMessage [IgnoreAlphaNums] texts >>= mapM_ TIO.putStrLn
  putStr "ignore signs alphanums: "
  generateMessage [IgnoreSigns] texts >>= mapM_ TIO.putStrLn
