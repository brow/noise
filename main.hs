import Text.Nouns.Parser (parseFile)
import Text.Nouns.Compiler (compile)
import Text.Nouns.Renderer (render)

main = parseFile "test.nouns" >>= putStr . render . compile
