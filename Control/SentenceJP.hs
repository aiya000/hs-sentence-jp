{-# LANGUAGE OverloadedStrings #-}

module Control.SentenceJP
  ( generateMessage
  ) where

import Control.Monad.State.Lazy (State, put, get, evalState)
import Data.Bifunctor (first)
import Data.List (delete)
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


unPosition :: Position a -> a
unPosition (NonEnd x) = x
unPosition (End    x) = x


-- | :D
generateMessage :: [Text] -> IO (Either String Text)
generateMessage sources = do
  mecab      <- new $ ["mecab"]
  let sources' = map ignoreSigns sources
  sentences  <- map (toSentence . filter (/= "") . toSimpleSentence) <$> mapM (parseToNodes mecab) sources'
  mixedWords <- shuffleM . concat $ sentences
  return $ markovChain mixedWords
  where
    -- [A-Z][a-z][0-9][あ-ん][亜-腕]
    ignoreSigns :: Text -> Text
    ignoreSigns = T.filter $ not . \c -> c `elem` ['A'..'Z']
                                      || c `elem` ['a'..'z']
                                      || c `elem` ['0'..'9']
                                      || c `elem` ['あ'..'ん']
                                      || c `elem` ['亜'..'腕']

    toSimpleSentence :: [Node Text] -> SimpleSentence
    toSimpleSentence = map nodeSurface

    toSentence :: SimpleSentence -> Sentence
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
