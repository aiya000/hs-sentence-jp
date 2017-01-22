{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.SentenceJP (generateMessage)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  texts    <- Text.lines <$> TIO.readFile "sentences.txt"
  sentence <- generateMessage texts
  case sentence of
    Left  e -> error e
    Right a -> TIO.putStrLn a
