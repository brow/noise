import qualified System.Environment
import           System.Console.GetOpt
import           System.IO (hPrint, stderr)
import           Data.Maybe (fromMaybe)
import qualified Text.Nouns as Nouns

main :: IO ()
main = do
  (flags, files, errors) <- getOptions
  runWithOptions flags files errors

runWithOptions :: [Flag] -> [String] -> [String] -> IO ()
runWithOptions [] [file] [] = do
  source <- readFile file
  let Nouns.Output svg errors _ = Nouns.process source
  mapM_ (hPrint stderr) errors
  putStr $ fromMaybe "" svg

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
