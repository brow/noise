import           Control.Applicative
import qualified System.Environment as Env
import qualified System.Console.GetOpt as Opt
import           System.IO (hPrint, stderr)
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import           Text.Nouns.Renderer (render)

main :: IO ()
main = do
  (flags, files, errors) <- getOptions
  runWithOptions flags files errors

runWithOptions :: [Flag] -> [String] -> [String] -> IO ()
runWithOptions [] files [] = do
  source <- case files of
    [file] -> readFile file
    []     -> getContents
    _      -> ioError (userError "multiple input files")
  case Parser.parse source of
    Left err -> hPrint stderr err
    Right ast -> case Compiler.compile ast of
      Left err -> hPrint stderr err
      Right doc -> putStr (render doc)
runWithOptions _ _ _ = putStr helpText

data Flag = Help

optionsSpec :: [Opt.OptDescr Flag]
optionsSpec =
  [ Opt.Option "h" ["help"] (Opt.NoArg Help) "Print this help text."
  ]

getOptions :: IO ([Flag], [String], [String])
getOptions = Opt.getOpt Opt.Permute optionsSpec <$> Env.getArgs

helpText :: String
helpText = Opt.usageInfo "Usage: nouns [file]" optionsSpec
