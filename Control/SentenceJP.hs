{-# LANGUAGE OverloadedStrings #-}
module Control.SentenceJP
  ( generateSentence
  ) where

import Control.Monad.IO.Class ( MonadIO (), liftIO )
import Data.List ( concat, foldl1 )
import Data.Text ( Text () )
import Prelude hiding ( foldl1 )
import System.Random.Shuffle ( shuffleM )
import Text.MeCab ( new, parseToNodes, Node (..) )
import qualified Data.Text as T


--- Define data types
--
data PositionText = Begin Text | Middle Text | End Text deriving (Show)

extractPositionText :: PositionText -> Text
extractPositionText (Begin  x) = x
extractPositionText (Middle x) = x
extractPositionText (End    x) = x

isEnd :: PositionText -> Bool
isEnd (End _) = True
isEnd _       = False

type PositionTextCons = (Text -> PositionText)
getPosition :: PositionText -> PositionTextCons
getPosition (Begin  _) = Begin
getPosition (Middle _) = Middle
getPosition (End    _) = End


data ChainableWords = ChainableWords PositionText PositionText deriving (Show)

areChainable :: ChainableWords -> ChainableWords -> Bool
areChainable (ChainableWords _ y1) (ChainableWords x2 _) =
  let jointL = extractPositionText y1
      jointR = extractPositionText x2
  in jointL == jointR

toText :: ChainableWords -> Text
toText (ChainableWords x y) = extractPositionText x `T.append` extractPositionText y

hasBegin :: ChainableWords -> Bool
hasBegin (ChainableWords (Begin _) _) = True
hasBegin _                            = False


--- Define functions
--

-- | Generating Sentence as Text from [Text]
generateSentence :: MonadIO m => [Text] -> m Text
generateSentence sources = do
  mecab      <- liftIO . new $ ["mecab"]
  nodes      <- liftIO . mapM (parseToNodes mecab) $ sources
  let sentences = map (toChainable . filter (/= "") . map nodeSurface) $ nodes
  sentences' <- liftIO . shuffleM . concat $ sentences
  return . chainWords $ sentences'

toChainable :: [Text] -> [ChainableWords]
toChainable [x] = [ChainableWords (Begin x) (End "")]
toChainable xs  = zipWith ChainableWords xs' (tail xs')
  where
    beginWord   = Begin . head $ xs
    endWord     = End   . last $ xs
    middleWords = map Middle . init . tail $ xs
    xs'         = [beginWord] ++ middleWords ++ [endWord]

chainWords :: [ChainableWords] -> Text
chainWords []  = error "aho-baka-hoge"
chainWords [x] = toText x
chainWords xs  = toText . foldl1 chain . dropWhile (not . hasBegin) $ xs
  where
    chain :: ChainableWords -> ChainableWords -> ChainableWords
    chain a@(ChainableWords _ y1) b@(ChainableWords _ y2)
      | areChainable a b = if isEnd y1
                              then a
                              else let l = Begin . toText $ a
                                   in ChainableWords l y2
      | otherwise = a
