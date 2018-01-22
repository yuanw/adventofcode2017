module Day07  where


import Text.ParserCombinators.Parsec

day07 :: IO ()
day07 = putStrLn "Part I"

data Program = Program
  { pName :: String
  , pWeight :: Int
  , pChildren :: [String]
  } deriving (Show)


parseProgram :: GenParser Char st Program
parseProgram = do
    name <- many1 undefined
    weight <- undefined
    return Program{pName=name, pWeight=weight, pChildren=[]}
