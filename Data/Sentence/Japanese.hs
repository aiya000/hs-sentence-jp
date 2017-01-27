{-# LANGUAGE OverloadedStrings #-}

module Data.Sentence.Japanese
  ( generateMessage
  , GenerateOption (..)
  ) where

import Control.Monad.State.Lazy (State, put, get, evalState)
import Data.Bifunctor (first)
import Data.Char (isPunctuation, isLetter)
import Data.List (delete)
import Data.Sentence.Japanese.Internal (applyWhen, isAlphaNum')
import Data.Text (Text)
import System.Random.Shuffle (shuffleM)
import Text.MeCab (new, parseToNodes, Node (..))
import qualified Data.Text as T

-- The sentence without the position
type SimpleSentence = [Text]

-- The position of a sentence
data Position a = NonEnd a | End a
  deriving (Eq)

-- The sentence with the position
type Sentence = [Position Text]

data MarkovChainer a = MarkovChainer
  { markovCurrenet :: a
  , markovResult   :: [a]
  } deriving (Show)

-- | An option of `generateMessage`
data GenerateOption = IgnoreSigns      -- ^ ignore 1 byte sign chars
                    | IgnoreAlphaNums  -- ^ ignore chars of 1 byte alphabet and 1 byte num
  deriving (Eq, Show)


unPosition :: Position a -> a
unPosition (NonEnd x) = x
unPosition (End    x) = x

-- | :D
generateMessage :: [GenerateOption] -> [Text] -> IO (Either String Text)
generateMessage options sources = do
  mecab      <- new $ ["mecab"]
  --TODO: Refactoring
  let sources'    = map T.unpack sources
      sources''   = applyWhen (IgnoreSigns `elem` options) (map (filter $ not . isPunctuation)) sources'
      sources'''  = applyWhen (IgnoreAlphaNums `elem` options) (map (filter $ not . isAlphaNum')) sources''
      sources'''' = map T.pack sources'''
  sentences  <- map (toSentence . filter (/= "") . toSimpleSentence) <$> mapM (parseToNodes mecab) sources''''
  mixedWords <- shuffleM . concat $ sentences
  return $ markovChain mixedWords
  where
    toSimpleSentence :: [Node Text] -> SimpleSentence
    toSimpleSentence = map nodeSurface

    toSentence :: SimpleSentence -> Sentence
    toSentence [] = []
    toSentence xs = let (y:ys) = reverse xs
                    in reverse $ End y : map NonEnd ys

markovChain :: [Position Text] -> Either String Text
markovChain []         = Left "Please take words to me"
markovChain [_]        = Left "Please take two or more words"
markovChain (txt:txts) =
  let pairs     = map (first unPosition) . zip txts $ tail txts
      firstWord = unPosition txt
      result = evalState (markovChain' pairs) $ MarkovChainer firstWord []
  in Right result
  where
    markovChain' :: [(Text, Position Text)] -> State (MarkovChainer Text) Text
    markovChain' ps = do
      (MarkovChainer current result) <- get
      case lookup current ps of
        -- Finish recurse if the result is Nothing or End
        Nothing         -> return . T.concat . reverse $ "。" : result
        Just (End word) -> return . T.concat . reverse $ word : result
        Just a@(NonEnd word) -> do
          put $ MarkovChainer word $ word : result
          -- Consume the one of pair
          let ps' = delete (current, a) ps
          markovChain' ps'
