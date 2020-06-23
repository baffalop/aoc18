module FabricParser
  ( parseRect
  ) where

import qualified FabricClaim as F
import Text.Parsec (many1, parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)

int :: Parser Int
int = do
  n <- many1 digit
  return $ read n

claimId :: Parser Int
claimId = do
  char '#'
  int

coord :: Parser (Int, Int)
coord = do
  x <- int
  char ','
  y <- int
  return (x, y)

dimensions :: Parser (Int, Int)
dimensions = do
  w <- int
  char 'x'
  h <- int
  return (w, h)

rect :: Parser F.Rect
rect = do
  id <- claimId
  string " @ "
  (x, y) <- coord
  string ": "
  (w, h) <- dimensions
  return $ F.Rect id x y w h False

parseRect :: String -> Either ParseError F.Rect
parseRect = parse rect ""
