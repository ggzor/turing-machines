module SVG where

import Data.Either (fromRight)
import Data.Function ((&))
import Data.String.Interpolate
import Data.Text (Text, pack, splitOn, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Fmt (fmt, (|+))
import RIO (encodeUtf8, toStrictBytes, view, (%~), (.~), (^.))
import RIO.ByteString.Lazy (fromStrict)
import RIO.List (headMaybe)
import RIO.Prelude (decodeUtf8', readMaybe)
import System.Exit
import System.Process.Typed
import Text.XML (def, parseText, renderText)
import Text.XML.Lens
import TuringMachines.Core
import TuringMachines.Eval (readTape)
import TuringMachines.Graphviz

data RenderTapeSettings = RenderTapeSettings
  { targetWidth :: Integer,
    size :: Integer,
    marginLeft :: Integer,
    marginTop :: Integer,
    padX :: Integer,
    tapeHeight :: Integer
  }

data NodeSettings = NodeSettings
  { idx :: Integer,
    bit :: Bit,
    originalIdx :: Integer,
    stateIdx :: Integer
  }

printImage :: Program Integer -> State Integer -> IO ()
printImage program state = do
  let tapeHeight = 200
  svg <- readProcessMaybe (generateStatefulGraph program state)
  let result =
        ( do
            svgTxt <- svg
            doc <- either (const Nothing) Just . parseText def . TL.fromStrict $ svgTxt
            let heightAttr = doc ^. root . attr "height"
            height <- readMaybe . unpack =<< (headMaybe . splitOn "pt" $ heightAttr)
            let widthAttr = doc ^. root . attr "width"
            width <- readMaybe . unpack =<< (headMaybe . splitOn "pt" $ widthAttr)
            let newHeight = tapeHeight + height
            (x, y, w) <- case T.words $ doc ^. root . attr "viewBox" of
              [x, y, w, _] -> Just (x, y, w)
              _ -> Nothing
            let newViewBox = T.unwords [x, y, w, pack . show $ newHeight]
            let settings =
                  RenderTapeSettings
                    { targetWidth = width,
                      marginTop = height,
                      tapeHeight,
                      size = 40,
                      marginLeft = 10,
                      padX = 10
                    }
            let newDoc =
                  doc & root . attr "height" .~ fmt (newHeight |+ "pt")
                    & root . attr "viewBox" .~ newViewBox
                    & root . nodes %~ (++ renderTape settings state)
            pure . TL.toStrict . renderText def $ newDoc
        )
  case result of
    Just newDoc -> runProcess_ $ setStdin (encodeAsInput newDoc) "magick convert - out.png"
    Nothing -> putStrLn "Failed to process document"

template :: RenderTapeSettings -> NodeSettings -> [Node]
template RenderTapeSettings {size, marginLeft, marginTop, padX, tapeHeight} NodeSettings {idx, bit, originalIdx, stateIdx} =
  let fill :: Text = if stateIdx == originalIdx then "#d3d3d3" else "none"
      templateValue =
        [i|
<svg xmlns="http://www.w3.org/2000/svg">
  <rect x="#{marginLeft + (padX + size) * idx}" y="#{marginTop + (tapeHeight `div` 2) - (size `div` 2)}"
        width="#{size}" height="#{size}" stroke="#000" fill="#{fill}">
  </rect>
  <text x="#{marginLeft + (padX + size) * idx + (size `div` 2)}"
        y="#{marginTop + (tapeHeight `div` 2)}"
        font-family="Times,serif" font-size="#{size `div` 2}"
        dominant-baseline="middle" text-anchor="middle" fill="#000">
    #{bit}
  </text>
  <text x="#{marginLeft + (padX + size) * idx + (size `div` 2)}"
        y="#{marginTop + (tapeHeight `div` 2) - (size `div` 2) - 10}"
        font-family="Times,serif" font-size="#{size `div` 3}"
        dominant-baseline="middle" text-anchor="middle" fill="#000">
    #{originalIdx}
  </text>
</svg>
          |]
      txt = view (root . nodes) <$> parseText def templateValue
   in fromRight [] txt

renderTape :: RenderTapeSettings -> State Integer -> [Node]
renderTape settings@RenderTapeSettings {targetWidth, size, padX} (State _ idx tape) =
  let nodeCount = targetWidth `div` (size + padX)
      leftNodes = (nodeCount `div` 2)
      rightNodes = nodeCount - leftNodes
      idxInteger :: Integer = fromIntegral idx
      marginLeft = (targetWidth - nodeCount * (size + padX)) `div` 2
   in template (settings {marginLeft})
        . ( \(screenIdx, j) ->
              NodeSettings
                { idx = screenIdx,
                  bit = readTape (fromIntegral j) tape,
                  originalIdx = j,
                  stateIdx = idxInteger
                }
          )
        =<< zip [0 ..] [0 - leftNodes .. 0 + rightNodes - 1]

readProcessMaybe :: Text -> IO (Maybe Text)
readProcessMaybe input = do
  (exitCode, out, _) <- readProcess (setStdin (encodeAsInput input) "dot -Tsvg")
  case exitCode of
    ExitSuccess -> pure . either (const Nothing) Just . decodeUtf8' . toStrictBytes $ out
    _ -> pure Nothing

encodeAsInput :: Text -> StreamSpec 'STInput ()
encodeAsInput = byteStringInput . fromStrict . encodeUtf8
