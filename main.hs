import Text.Nouns.Parser
import Text.Parsec.String
import Text.Show.Pretty (ppShow)

main = do
  result <- parseFromFile sourceFile "test.nouns"
  case result of
    Left err -> print err
    Right ast -> putStr (ppShow ast)
