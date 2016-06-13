# hs-sentence-jp

You can generate bot tick japanese message easily.

Example:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.SentenceJP
import Data.Text
import qualified Data.Text.IO as TIO

sources :: [Text]
sources =
  [ "明日は晴れだ。"
  , "僕は陽気です。"
  , "これはハラショーですね。"
  ]

main :: IO ()
main = generateSentence sources >>= TIO.putStrLn
```

- - -

This library was used [this project](https://github.com/aiya000/hs-gorira).

- - -

This project depends mecab.  
Please see [here](https://github.com/morishin/hsmecab).
