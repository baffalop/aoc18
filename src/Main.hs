module Main where

import Data.Semigroup ((<>))
import Options.Applicative

data Options =
  Options
    { day :: Int
    , part :: DayPart
    , input :: Maybe String
    }
  deriving (Show)

data DayPart
  = A
  | B
  | Both
  deriving (Show, Read)

cli =
  let opts =
        Options <$>
        (option auto $
         short 'd' <>
         long "day" <> metavar "N" <> help "Which day's solution to run") <*>
        (option auto $
         short 'p' <>
         long "part" <>
         value Both <> help "Which part of the day's solution to run") <*>
        (optional $
         strOption $
         long "input" <>
         short 'i' <>
         metavar "FILE" <> help "Override file to use as puzzle's input")
   in info (opts <**> helper) $
      fullDesc <>
      header "Solutions to Advent of Code 2018" <>
      progDesc "Run solution(s) for the AoC puzzle of the given day"

main :: IO ()
main = print =<< execParser cli
