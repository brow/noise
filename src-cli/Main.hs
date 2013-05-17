{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import qualified System.Environment as Env
import qualified System.Console.GetOpt as Opt
import qualified System.IO as IO
import qualified Text.Nouns.Error as Error
import qualified Text.Nouns.Parser as Parser
import qualified Text.Nouns.Compiler as Compiler
import qualified Text.Nouns.Renderer as Renderer
import qualified Text.Nouns.SourceRange as SourceRange

main :: IO ()
main = do
  (flags, files, errors) <- getOptions
  runWithOptions flags files errors

runWithOptions :: [Flag] -> [String] -> [String] -> IO ()
runWithOptions [] files [] = do
  (sourceName, source) <- case files of
    [file] -> (file,) <$> readFile file
    []     -> ("-",)  <$> getContents
    _      -> ioError (userError "multiple input files")
  case Parser.parse sourceName source of
    Left err -> printErr err
    Right ast -> case Compiler.compile ast of
      Left err -> printErr err
      Right doc -> putStr (Renderer.render doc)
  where printErr err = IO.hPutStrLn IO.stderr (showError err)
runWithOptions _ _ _ = putStr helpText

showError :: (Error.Error a) => a -> String
showError err = sourceName ++ ":" ++ line ++ ":" ++ column ++ ": " ++ message
  where message = Error.message err
        range = SourceRange.rangeInSource err
        line = show (SourceRange.startLine range)
        column = show (SourceRange.startColumn range)
        sourceName = SourceRange.sourceName range

data Flag = Help

optionsSpec :: [Opt.OptDescr Flag]
optionsSpec =
  [ Opt.Option "h" ["help"] (Opt.NoArg Help) "Print this help text."
  ]

getOptions :: IO ([Flag], [String], [String])
getOptions = Opt.getOpt Opt.Permute optionsSpec <$> Env.getArgs

helpText :: String
helpText = Opt.usageInfo "Usage: noise [file]" optionsSpec
