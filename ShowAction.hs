{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.SentenceJP
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  texts    <- Text.lines <$> TIO.readFile "sentences.txt"
  sentence <- generateSentence texts
  TIO.putStrLn sentence
