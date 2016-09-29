{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.SentenceJP
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  sentence <- generateSentence
    [ "僕は貴方ではない"
    , "貴方は魔術師だ"
    , "魔術師は強い"
    , "汝、強さを求めるか"
    , "あっ、やっちまった"
    ]
  TIO.putStrLn sentence
