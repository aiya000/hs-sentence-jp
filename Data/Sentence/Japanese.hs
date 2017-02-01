{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Sentence.Japanese
  ( generateMessage
  , GenerateOption (..)
  ) where

import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (State, put, get, evalState, runState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Bifunctor (first)
import Data.List (delete)
import Data.Sentence.Japanese.Internal (isAlphaNum', isNonJapanesePunctuation, mapInnerStr)
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

-- | The type for `markovChain`, with logging and state
newtype MarkovApp a = MarkovApp
  { _runMarkovApp :: WriterT [String] (State (MarkovChainer Text)) a
  } deriving ( Functor, Applicative, Monad
             , MonadWriter [String], MonadState (MarkovChainer Text)
             )

-- Extract to (('result', 'log'), 'last_state')
runMarkovApp :: MarkovApp a -> MarkovChainer Text -> ((a, [String]), MarkovChainer Text)
runMarkovApp s a = flip runState a . runWriterT . _runMarkovApp $ s

unPosition :: Position a -> a
unPosition (NonEnd x) = x
unPosition (End    x) = x


-- | :D
generateMessage :: [GenerateOption] -> [Text] -> IO (Either String Text)
generateMessage options sources = do
  result <- generateMessageWithLog options sources
  case result of
    Left  e -> return $ Left e
    Right a -> return . Right $ fst a

-- | :)
generateMessageWithLog :: [GenerateOption] -> [Text] -> IO (Either String (Text, [String]))

-- Remove the sign chars from sources,
-- but don't remove if the char is Japanese punctuation (Ex: '。', '、')
generateMessageWithLog options sources | IgnoreSigns `elem` options = do
  let sources' = map (mapInnerStr filterSigns) sources
  flip generateMessageWithLog sources' $ delete IgnoreSigns options
  where
    filterSigns = filter $ not . isNonJapanesePunctuation

-- Remove the alphabet chars and the number chars from sources
generateMessageWithLog options sources | IgnoreAlphaNums `elem` options = do
  let sources' = map (mapInnerStr filterAlNums) sources
  flip generateMessageWithLog sources' $ delete IgnoreAlphaNums options
  where
    filterAlNums = filter $ not . isAlphaNum'

-- Generate the message
generateMessageWithLog options sources = do
  mecab      <- new $ ["mecab"]
  sentences  <- map (toSentence . filter (/= "") . toSimpleSentence) <$> mapM (parseToNodes mecab) sources
  mixedWords <- shuffleM . concat $ sentences
  return $ markovChain mixedWords
  where
    toSimpleSentence :: [Node Text] -> SimpleSentence
    toSimpleSentence = map nodeSurface

    toSentence :: SimpleSentence -> Sentence
    toSentence [] = []
    toSentence xs = let (y:ys) = reverse xs
                    in reverse $ End y : map NonEnd ys


-- | Generate a sentence with logs
markovChain :: [Position Text] -> Either String (Text, [String])
markovChain []         = Left "Please take words to me"
markovChain [_]        = Left "Please take two or more words"
markovChain (txt:txts) =
  let pairs     = map (first unPosition) . zip txts $ tail txts
      firstWord = unPosition txt
      result    = fst . runMarkovApp (markovChain' pairs) $ MarkovChainer firstWord []
  in Right result
  where
    markovChain' :: [(Text, Position Text)] -> MarkovApp Text
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
