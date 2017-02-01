-- | General logics
module Data.Sentence.Japanese.Internal
  ( isAlphaNum'
  , mapInnerStr
  ) where

import Data.Text (Text)
import qualified Data.Text as T


-- |
-- The instead of Data.Char.isAlphaNum.
-- (`isAlphaNum 'あ'` returns True, why ?)
isAlphaNum' :: Char -> Bool
isAlphaNum' c = c `elem` ['A'..'Z']
             || c `elem` ['a'..'z']
             || c `elem` ['0'..'9']

-- | Apply the function to the String in the Text
mapInnerStr :: (String -> String) -> Text -> Text
mapInnerStr f = T.pack . f . T.unpack
