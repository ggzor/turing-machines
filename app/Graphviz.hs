{-# LANGUAGE TemplateHaskell #-}

module Graphviz where

import TuringMachines.Core
import TuringMachines.Graphviz

import Control.Lens (both, makeLenses, mapMOf, (^.))
import Data.Function ((&))
import Data.Text (Text, splitOn, unpack)
import qualified Data.Text.Lazy as TL
import RIO (encodeUtf8, toStrictBytes)
import RIO.ByteString.Lazy (fromStrict)
import RIO.List (headMaybe)
import RIO.Prelude (decodeUtf8', readMaybe)
import System.Exit
import System.Process.Typed
import Text.XML (def, parseText)
import Text.XML.Lens

data GraphRenderData = GraphRenderData
  { _width :: !Int
  , _height :: !Int
  }

makeLenses ''GraphRenderData

getGraphRenderData :: Program Integer -> IO (Maybe GraphRenderData)
getGraphRenderData program = do
  mSvg <- graphvizSvg (generateGraph program)
  pure do
    doc <- mSvg
    (_width, _height) <-
      ("width", "height") & mapMOf both \key -> do
        let value = doc ^. root . attr key
        strValue <- headMaybe . splitOn "pt" $ value
        readMaybe . unpack $ strValue
    pure GraphRenderData{_width, _height}

graphvizSvg :: Text -> IO (Maybe Document)
graphvizSvg graph = do
  svg <- readProcessMaybe "dot -Tsvg" graph
  pure $
    svg
      >>= either (const Nothing) Just
        . parseText def
        . TL.fromStrict

readProcessMaybe :: ProcessConfig a b c -> Text -> IO (Maybe Text)
readProcessMaybe process input = do
  (exitCode, out, _) <- readProcess (setStdin (encodeAsInput input) process)
  case exitCode of
    ExitSuccess ->
      pure . either (const Nothing) Just
        . decodeUtf8'
        . toStrictBytes
        $ out
    _ -> pure Nothing

encodeAsInput :: Text -> StreamSpec 'STInput ()
encodeAsInput = byteStringInput . fromStrict . encodeUtf8
