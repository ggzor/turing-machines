{-# LANGUAGE TemplateHaskell #-}

module SVG where

import Control.Lens (both, makeLenses, mapMOf)
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

data RenderOptions = RenderOptions
  { _tapeHeight :: !Int,
    _cellSize :: !Int,
    _cellGap :: !Int
  }

data ComputedRenderOptions = ComputedRenderOptions
  { _width :: !Int,
    _height :: !Int,
    _left :: !Int,
    _cellsCount :: !Int
  }

data StateRenderOptions = StateRenderOptions
  { _pivot :: !Int,
    _steps :: !Int
  }

data RenderSettings = RenderSettings
  { _configuration :: !RenderOptions,
    _computed :: !ComputedRenderOptions,
    _state :: !StateRenderOptions
  }

makeLenses ''RenderSettings
makeLenses ''StateRenderOptions
makeLenses ''ComputedRenderOptions
makeLenses ''RenderOptions

generateComputedRenderOptions :: RenderOptions -> Program Integer -> IO (Maybe ComputedRenderOptions)
generateComputedRenderOptions renderOptions program = do
  mSvg <- graphvizSvg (generateGraph program)
  pure do
    doc <- mSvg

    (_width, _height) <-
      ("width", "height") & mapMOf both \key -> do
        let value = doc ^. root . attr key
        strValue <- headMaybe . splitOn "pt" $ value
        readMaybe . unpack $ strValue

    let size = renderOptions ^. cellSize
        gap = renderOptions ^. cellGap
        _cellsCount = (_width + gap) `div` (size + gap)
    let _left = (_width - (_cellsCount * size + (_cellsCount - 1) * gap)) `div` 2

    pure
      ComputedRenderOptions
        { _width,
          _height,
          _left,
          _cellsCount
        }

graphvizSvg :: Text -> IO (Maybe Document)
graphvizSvg graph = do
  svg <- readProcessMaybe graph
  pure $ svg >>= either (const Nothing) Just . parseText def . TL.fromStrict

data NodeSettings = NodeSettings
  { idx :: Index,
    bit :: Bit,
    originalIdx :: Index,
    stateIdx :: Index
  }

pivotBounds :: Int -> Index -> (Index, Index)
pivotBounds n pivot
  | odd n =
    let space = (n - 1) `div` 2
     in (pivot - space, pivot + space)
  | otherwise =
    let space = n `div` 2
     in (pivot - space, pivot + space - 1)

printImage :: RenderSettings -> Program Integer -> State Integer -> IO ()
printImage renderSettings program state = do
  mSvg <- graphvizSvg (generateStatefulGraph program state)
  let result =
        ( do
            doc <- mSvg

            (x, y, w) <- case T.words $ doc ^. root . attr "viewBox" of
              [x, y, w, _] -> Just (x, y, w)
              _ -> Nothing

            let newHeight = (renderSettings ^. computed . height) + (renderSettings ^. configuration . tapeHeight)
            let newViewBox = T.unwords [x, y, w, pack . show $ newHeight]

            let newDoc =
                  doc & root . attr "height" .~ fmt (newHeight |+ "pt")
                    & root . attr "viewBox" .~ newViewBox
                    & root . nodes %~ (++ renderTape renderSettings state)
            pure . TL.toStrict . renderText def $ newDoc
        )
  case result of
    Just newDoc -> runProcess_ $ setStdin (encodeAsInput newDoc) "magick convert - out.png"
    Nothing -> putStrLn "Failed to process document"

template :: RenderSettings -> NodeSettings -> [Node]
template renderSettings NodeSettings {idx, bit, originalIdx, stateIdx} =
  let fill :: Text = if stateIdx == originalIdx then "#d3d3d3" else "none"
      size = renderSettings ^. configuration . cellSize
      midSize = size `div` 2
      baseX =
        let marginLeft = renderSettings ^. computed . left
            gap = renderSettings ^. configuration . cellGap
         in marginLeft + (gap + size) * idx
      baseY =
        let marginTop = renderSettings ^. computed . height
            midTapeHeight = (renderSettings ^. configuration . tapeHeight) `div` 2
         in marginTop + midTapeHeight - midSize
      templateValue =
        [i|
<svg xmlns="http://www.w3.org/2000/svg">
  <rect x="#{baseX}" y="#{baseY}"
        width="#{size}" height="#{size}" stroke="#000" fill="#{fill}">
  </rect>
  <text x="#{baseX + midSize}"
        y="#{baseY + midSize}"
        font-family="Times,serif" font-size="#{midSize}"
        dominant-baseline="middle" text-anchor="middle" fill="#000">
    #{bit}
  </text>
  <text x="#{baseX + midSize}"
        y="#{baseY - 10}"
        font-family="Times,serif" font-size="#{size `div` 3}"
        dominant-baseline="middle" text-anchor="middle" fill="#000">
    #{originalIdx}
  </text>
</svg>
          |]
      txt = view (root . nodes) <$> parseText def templateValue
   in fromRight [] txt

renderTape :: RenderSettings -> State Integer -> [Node]
renderTape renderSettings (State _ idx tape) =
  let (minIdx, maxIdx) =
        pivotBounds
          (renderSettings ^. computed . cellsCount)
          (renderSettings ^. state . pivot)
   in template renderSettings
        . ( \(screenIdx, j) ->
              NodeSettings
                { idx = screenIdx,
                  bit = readTape (fromIntegral j) tape,
                  originalIdx = j,
                  stateIdx = idx
                }
          )
        =<< zip [0 ..] [minIdx .. maxIdx]

readProcessMaybe :: Text -> IO (Maybe Text)
readProcessMaybe input = do
  (exitCode, out, _) <- readProcess (setStdin (encodeAsInput input) "dot -Tsvg")
  case exitCode of
    ExitSuccess -> pure . either (const Nothing) Just . decodeUtf8' . toStrictBytes $ out
    _ -> pure Nothing

encodeAsInput :: Text -> StreamSpec 'STInput ()
encodeAsInput = byteStringInput . fromStrict . encodeUtf8
