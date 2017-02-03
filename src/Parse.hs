module Parse
    ( skiParse
    , skiDict
    ) where

import           Control.Arrow
import           Data.Char
import           Data.Map.Lazy                 (Map)
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe
import           Reflection
import           Ski
import           Text.Parsec                   hiding (runParser, try)
import           Text.ParserCombinators.Parsec

type Level = Int

type SkiParser = GenParser Char (Level, Map String (Ski (Level, String)))

readBracesContent :: SkiParser String
readBracesContent = do
  char '{'
  content <- many (readBraces <|> fmap (: []) (noneOf "{}"))
  char '}'
  return $ concat content

readBraces :: SkiParser String
readBraces = do
  content <- readBracesContent
  return $ "{" ++ content ++ "}"

primitive :: Char -> SkiParser (Ski a)
primitive 'S' = return S
primitive 'K' = return K
primitive 'I' = return I
primitive c   = parserFail $ c : " is not a primitive"

readParens :: SkiParser (Ski (Level, String))
readParens = do
  char '('
  result <- skiTokens
  char ')'
  return result

-- Inserts definition into the state map and returns the old level and map.
definition :: SkiParser (Level, Map String (Ski (Level, String)))
definition = do
  char ':'
  varname <- skiVar
  char '='
  val <- skiTokens
  char ';'
  (curLevel, curMap) <- getState
  putState (curLevel + 1, Map.insert varname val curMap)
  return (curLevel, curMap)

defineExpr :: SkiParser (Ski (Level, String))
defineExpr = do
  oldState <- definition
  result <- skiTokens
  putState oldState
  return result

lambdaExpr :: SkiParser (Ski (Level, String))
lambdaExpr = do
  char '\\'
  varname <- skiVar
  (curLevel, curMap) <- getState
  putState (curLevel + 1, Map.insert varname (Var (curLevel, varname)) curMap)
  expr <- skiTokens
  putState (curLevel, curMap)
  return $ lambda (curLevel, varname) expr

var :: String -> SkiParser (Ski (Level, String))
var v = fromMaybe (Var (0, v)) . Map.lookup v . snd <$> getState

reflectToken :: SkiParser (Ski (Level, String))
reflectToken = reflect id <$> skiToken

skiToken :: SkiParser (Ski (Level, String))
skiToken =
      (readBracesContent >>= var)
  <|> readParens
  <|> defineExpr
  <|> lambdaExpr
  <|> (char 'R' >> reflectToken)
  <|> (char '$' >> skiTokens)
  <|> (upper >>= primitive)
  <|> (singleCharVar >>= var)

skiVar :: SkiParser String
skiVar = do
  spaces
  result <- readBracesContent <|> singleCharVar
  spaces
  return result

singleCharVar :: SkiParser String
singleCharVar = do
  c <- noneOf "{}()\\:=;$"
  if isUpper c
    then parserFail $ "Expected variable; got primitive " ++ [c]
    else return [c]

skiTokens :: SkiParser (Ski (Level, String))
skiTokens = do
  spaces
  tokens <- endBy1 skiToken spaces
  return $ foldl1 (!^) tokens

getResult :: SkiParser a -> String -> a
getResult m s =
  case runParser m (1, Map.empty) "" s of
    Left err     -> error $ show err
    Right result -> result

skiParse :: String -> Ski String
skiParse = fmap snd . getResult skiTokens

skiDict :: String -> Map String (Ski String)
skiDict = fmap (fmap snd) . snd . getResult helper
  where
    helper = do
      endBy definition spaces
      getState
