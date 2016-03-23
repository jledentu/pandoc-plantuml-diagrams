
-- | Module: Text.Pandoc.PlantUML.Filter.Render
-- Defines the actual rendering done with PlantUML
module Text.Pandoc.PlantUML.Filter.IORender() where

import System.IO (hClose, hPutStr, IOMode(..), withBinaryFile, Handle, hSetEncoding, utf8)
import Data.ByteString.Lazy (hGetContents, hPut)
import System.Process
import System.Directory

import Text.Pandoc.PlantUML.Filter.Types

instance ImageIO IO where
  renderImage imageFileName (DiagramSource source) = do
    (Just hIn, Just hOut, _, _) <- createProcess $ plantUmlProcess imageFileName
    hSetEncoding hIn utf8
    hSetEncoding hOut utf8
    hPutStr hIn source
    hClose hIn
    withImageFile $ pipe hOut
    hClose hOut
      where withImageFile = withBinaryFile (show imageFileName) WriteMode
  doesImageExist imageFileName = doesFileExist $ show imageFileName

plantUmlProcess :: ImageFileName -> CreateProcess
plantUmlProcess (ImageFileName _ fileType) = (proc "java" ["-jar", "plantuml.jar", "-config", "\"uml.config\"", "-charset", "UTF-8", "-pipe", "-t" ++ fileType])
  { std_in = CreatePipe, std_out = CreatePipe }

pipe :: Handle -> Handle -> IO ()
pipe hIn hOut = do
  input <- hGetContents hIn
  hPut hOut input
