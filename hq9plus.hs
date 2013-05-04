import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (<$>))
import Data.List (intercalate)

data HQNI = H | Q | N | I

instance Show HQNI where
  show H = "H"
  show Q = "Q"
  show N = "9"
  show I = "+"

program :: Parser [HQNI]
program = many statement

statement :: Parser HQNI
statement = do
  atom <- hqnp
  return $ constructor atom
  where
    constructor 'H' = H
    constructor 'Q' = Q
    constructor '9' = N
    constructor '+' = I

hqnp = char 'H'
     <|> char 'Q'
     <|> char '9'
     <|> char '+'

bottles howMany takeDown oneFewer =
  howMany ++ " bottles of beer on the wall\n" ++
  howMany ++ " bottles of beer\n" ++
  "Take " ++ takeDown ++ " down, pass it around\n" ++
  oneFewer ++ " bottles of beer on the wall"

bottlesFrom 1 = [bottles "1" "1" "no"]
bottlesFrom n = (bottles (show n) "one" (show $ n - 1)):bottlesFrom (n - 1)

quine [] = ""
quine (x:xs) = show x ++ quine xs

eval :: [HQNI] -> [String]
eval p = eval' p 0 p
  where eval' q acc (H:xs) = "Hello, world" : eval' q acc xs
        eval' q acc (Q:xs) = (quine q) : eval' q acc xs
        eval' q acc (N:xs) = (intercalate "\n" (bottlesFrom 99)) : eval' q acc xs
        eval' q acc (I:xs) = eval' q (acc + 1) xs
        eval' _ _ []       = []

main = do
  input <- getContents
  case parse program "(unknown)" input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ putStrLn $ eval r
