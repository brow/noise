import Language.Nouns.Core
import Text.Parsec.String

main = do
  parseFromFile document "test.nouns" >>= print
