{-# LANGUAGE TemplateHaskell #-}

module SVG where

import Commands.Eval.Speculative
import TuringMachines.Core
import TuringMachines.Eval (readTape)
import TuringMachines.Graphviz

import Control.Lens (ix, makeLenses, (^?), _Just)
import Control.Monad (join)
import Data.Function ((&))
import Data.Maybe (maybeToList)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Utils (tshow)
import Graphviz
import RIO (view, (%~), (.~), (^.))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Temp (emptySystemTempFile)
import System.Process.Typed
import Text.RawString.QQ
import Text.XML (def, parseText, renderText)
import Text.XML.Lens
import Utils (exitError)

data RenderOptions = RenderOptions
  { _tapeHeight :: !Int
  , _cellSize :: !Int
  , _cellGap :: !Int
  , _minWidth :: !Int
  , _stepTextSize :: !Int
  }

data StateRenderOptions = StateRenderOptions
  { _pivot :: !Int
  , _steps :: !Int
  }

data ComputedRenderOptions = ComputedRenderOptions
  { _graphData :: !GraphRenderData
  , _cellCount :: !Int
  , _finalWidth :: !Int
  , _finalHeight :: !Int
  }

data RenderSettings = RenderSettings
  { _configuration :: !RenderOptions
  , _computed :: !ComputedRenderOptions
  , _state :: !StateRenderOptions
  , _speculativeData :: !(Maybe SpeculativeData)
  }

makeLenses ''RenderSettings
makeLenses ''StateRenderOptions
makeLenses ''ComputedRenderOptions
makeLenses ''RenderOptions

printImage :: RenderSettings -> Program Integer -> State Integer -> IO (Maybe FilePath)
printImage renderSettings program pState = do
  mSvg <- graphvizSvg (generateStatefulGraph program pState)
  let result =
        ( do
            doc <- mSvg

            (x, y) <- case T.words $ doc ^. root . attr "viewBox" of
              [x, y, _, _] -> Just (x, y)
              _ -> Nothing

            let newWidth = renderSettings ^. computed . finalWidth
                newHeight = renderSettings ^. computed . finalHeight
                newViewBox = T.unwords [x, y, tshow newWidth, tshow newHeight]
                graphWidth = renderSettings ^. computed . graphData . width
                graphDx = fromIntegral $ (newWidth - graphWidth) `div` 2
                graphDy = fromIntegral $ renderSettings ^. configuration . stepTextSize
                currentStep = renderSettings ^. state . steps

            let newDoc =
                  doc
                    & root . attr "height" .~ [i|#{newHeight}pt|]
                    & root . attr "width" .~ [i|#{newWidth}pt|]
                    & root . attr "viewBox" .~ newViewBox
                    & root . nodes
                      %~ (whiteBackground ++)
                        . (++ renderTape renderSettings pState)
                        . (++ renderStepText renderSettings currentStep)
                        . transformWithGroup (graphDx, graphDy)
            pure . TL.toStrict . renderText def $ newDoc
        )
  case result of
    Just newDoc -> do
      tempPath <- emptySystemTempFile "turing-machine.png"
      readProcessMaybe "rsvg-convert --help" "" >>= \case
        Just _ -> do
          code <-
            runProcess . setStdin (encodeAsInput newDoc) $
              proc "rsvg-convert" ["-o", tempPath]
          case code of
            ExitSuccess -> pure ()
            ExitFailure _ ->
              exitError "No se pudo procesar la imagen con rsvg-convert"
          pure $ Just tempPath
        Nothing -> exitError "rsvg-convert no está instalado"
    Nothing -> putStrLn "Failed to process document" >> pure Nothing

computedRenderOptionsFor :: RenderOptions -> GraphRenderData -> ComputedRenderOptions
computedRenderOptionsFor renderOptions _graphData@GraphRenderData{_width, _height} =
  let gap = renderOptions ^. cellGap
      size = renderOptions ^. cellSize
      _finalWidth = max (renderOptions ^. minWidth) _width
      _finalHeight = _height + renderOptions ^. tapeHeight + renderOptions ^. stepTextSize
      _cellCount = (_finalWidth + gap) `div` (size + gap)
   in ComputedRenderOptions{_graphData, _cellCount, _finalWidth, _finalHeight}

renderTape :: RenderSettings -> State Integer -> [Node]
renderTape renderSettings (State _ idx tape) =
  let targetWidth = renderSettings ^. computed . finalWidth
      targetHeight = renderSettings ^. computed . finalHeight
      count = renderSettings ^. computed . cellCount
      size = renderSettings ^. configuration . cellSize
      gap = renderSettings ^. configuration . cellGap

      marginTop = targetHeight - renderSettings ^. configuration . tapeHeight
      marginLeft = (targetWidth - (count * size + (count - 1) * gap)) `div` 2

      (minIdx, maxIdx) = pivotBounds count (renderSettings ^. state . pivot)

      midTapeHeight = (renderSettings ^. configuration . tapeHeight) `div` 2
      midSize = size `div` 2
   in join do
        (screenIdx, realIdx) <- zip [0 ..] [minIdx .. maxIdx]
        let baseX = marginLeft + (gap + size) * screenIdx
            baseY = marginTop + midTapeHeight - midSize
            isActive = realIdx == idx
            value = readTape realIdx tape
        pure $
          tapeCellTemplate
            TapeCellSettings
              { realIdx
              , baseX
              , baseY
              , value
              , size
              , isActive
              }

data TapeCellSettings = TapeCellSettings
  { realIdx :: !Int
  , baseX :: !Int
  , baseY :: !Int
  , value :: !Bit
  , size :: !Int
  , isActive :: !Bool
  }

tapeCellTemplate :: TapeCellSettings -> [Node]
tapeCellTemplate TapeCellSettings{realIdx, baseX, baseY, value, size, isActive} =
  let fill :: Text = if isActive then "#d3d3d3" else "none"
      midSize = size `div` 2
      templateValue =
        [i|
<svg xmlns="http://www.w3.org/2000/svg">
  <rect x="#{baseX}" y="#{baseY}"
        width="#{size}" height="#{size}" stroke="\#000" fill="#{fill}">
  </rect>
  <text x="#{baseX + midSize}"
        y="#{baseY + midSize + midSize `div` 3}"
        font-family="Times,serif" font-size="#{midSize}"
        dominant-baseline="middle" text-anchor="middle" fill="\#000">
    #{value}
  </text>
  <text x="#{baseX + midSize}"
        y="#{baseY - midSize `div` 4}"
        font-family="Times,serif" font-size="#{size `div` 3}"
        dominant-baseline="middle" text-anchor="middle" fill="\#000">
    #{realIdx}
  </text>
</svg>
|]
   in parseSVG templateValue

renderStepText :: RenderSettings -> Int -> [Node]
renderStepText renderSettings step =
  let steps = renderSettings ^? speculativeData . _Just . totalSteps
      stepTextHeight = renderSettings ^. configuration . stepTextSize
      stepTextContent =
        show step
          ++ maybe "" ((" / " ++) . show) steps
      targetX = 10 :: Int
      targetY = stepTextHeight `div` 2 + 10
      template =
        [i|
<svg xmlns="http://www.w3.org/2000/svg">
  <text x="#{targetX}"
        y="#{targetY}"
        font-family="Times,serif" font-size="#{stepTextHeight - 10}"
        text-anchor="start" fill="\#000">
    #{stepTextContent}
  </text>
</svg>
|]
   in parseSVG template

whiteBackground :: [Node]
whiteBackground =
  parseSVG
    [r|
<svg xmlns="http://www.w3.org/2000/svg">
 <rect width="100%" height="100%" fill="white" />
</svg>
|]

transformWithGroup :: (Double, Double) -> [Node] -> [Node]
transformWithGroup (tx, ty) content =
  let svg =
        parseSVG
          [i|
<svg xmlns="http://www.w3.org/2000/svg">
  <g transform="translate(#{tx} #{ty})" />
</svg>
|]
      element = svg ^? ix 1
   in maybeToList $ element & _Just . _Element . nodes .~ content

parseSVG :: Text -> [Node]
parseSVG = either (const []) (view (root . nodes)) . parseText def . TL.fromStrict

pivotBounds :: Int -> Index -> (Index, Index)
pivotBounds n pivot
  | odd n =
    let space = (n - 1) `div` 2
     in (pivot - space, pivot + space)
  | otherwise =
    let space = n `div` 2
     in (pivot - space, pivot + space - 1)
