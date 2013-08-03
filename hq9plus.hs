import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (<$>))
import Data.List (intercalate)
import Control.Monad.Writer

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

eval :: [HQNI] -> Writer (Sum Int) String
eval p = foldM (eval' p) "" (reverse p)
  where eval' q s H =
          return $ "Hello, world\n" ++ s
        eval' q s Q =
          return $ (quine q) ++ "\n" ++ s
        eval' q s N =
          return $ (intercalate "\n" (bottlesFrom 99)) ++ "\n" ++ s
        eval' q s I = do
          tell (Sum 1)
          return s

main = do
  input <- getContents
  case parse program "(unknown)" input of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> do
      let (result, acc) = runWriter $ eval r
      putStr result
      print $ getSum acc
