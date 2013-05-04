import qualified System.Environment
import           System.Console.GetOpt
import           System.IO (hPrint, stderr)
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import           Text.Nouns.Renderer (render)

main :: IO ()
main = do
  (flags, files, errors) <- getOptions
  runWithOptions flags files errors

runWithOptions :: [Flag] -> [String] -> [String] -> IO ()
runWithOptions [] [file] [] = do
  source <- readFile file
  case Parser.parse source of
    Left err -> hPrint stderr err
    Right ast -> case Compiler.compile ast of
      Left err -> hPrint stderr err
      Right doc -> putStr (render doc)

runWithOptions _ _ _ = putStr helpText
data Flag = Help

optionsSpec :: [OptDescr Flag]
optionsSpec =
  [ Option "h" ["help"] (NoArg Help) "Print this help text."
  ]

getOptions :: IO ([Flag], [String], [String])
getOptions = do
  args <- System.Environment.getArgs
  return (getOpt Permute optionsSpec args)

helpText :: String
helpText = usageInfo "Usage: nouns source.nouns" optionsSpec
