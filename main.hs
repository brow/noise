import qualified System.Environment
import System.Console.GetOpt as Options
import Text.Nouns.Parser (parseFile)
import Text.Nouns.Compiler (compile)
import Text.Nouns.Renderer (render)

main :: IO ()
main = do
  (flags, files, errors) <- getOptions
  runWithOptions flags files errors

runWithOptions :: [Flag] -> [String] -> [String] -> IO ()
runWithOptions [] [file] [] = do
  parseFile file >>= putStr . render . compile
runWithOptions _ _ _ = putStr helpText

data Flag = Help

optionsSpec :: [OptDescr Flag]
optionsSpec =
  [ Option ['h'] ["help"] (NoArg Help) "Print this help text."
  ]

getOptions :: IO ([Flag], [String], [String])
getOptions = do
  args <- System.Environment.getArgs
  return (getOpt Permute optionsSpec args)

helpText :: String
helpText = usageInfo "Usage: nouns source.nouns" optionsSpec
