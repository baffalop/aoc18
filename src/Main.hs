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
  = PartA
  | PartB
  | BothParts
  deriving (Show, Read)

cli =
  let opts =
        Options <$>
        (option auto $
         short 'd' <>
         long "day" <> metavar "N" <> help "Which day's solution to run") <*>
        (buildDayPart <$>
         (switch $ short 'a' <> help "Run only part A of the day's solution") <*>
         (switch $ short 'b' <> help "Run only part B of the day's solution")) <*>
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

buildDayPart :: Bool -> Bool -> DayPart
buildDayPart True False = PartA
buildDayPart False True = PartB
buildDayPart _ _ = BothParts
